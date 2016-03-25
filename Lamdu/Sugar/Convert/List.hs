{-# LANGUAGE NoImplicitPrelude, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Sugar.Convert.List
    ( cons
    , nil
    ) where

import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (guard, void, MonadPlus(..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.MonadA (MonadA)
import qualified Data.Map.Utils as MapUtils
import           Data.Maybe.Utils (maybeToMPlus)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.RecordVal as RecordVal
import qualified Lamdu.Expr.Type as T
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Prelude.Compat

type T = Transaction

nil ::
    (MonadA m, Monoid a) =>
    V.Nom (Val (Input.Payload m a)) -> Input.Payload m a ->
    MaybeT (ConvertM m) (ExpressionU m a)
nil (V.Nom tid val) exprPl =
    do
        guard $ tid == Builtins.streamTid
        injTag <-
            val ^?
            V.body . ExprLens._BAbs . V.lamResult .
            V.body . ExprLens._BInject . V.injectTag & maybeToMPlus
        guard $ injTag == Builtins.nilTag
        let mkListActions exprS =
                ListActions
                { addFirstItem = mkListAddFirstItem exprS
                , replaceNil = EntityId.ofValI <$> DataOps.setToHole exprS
                }
        List
            { _listValues = []
            , _listActions = exprPl ^. Input.stored & mkListActions
            , _listNilEntityId = exprPl ^. Input.entityId
            }
            & BodyList & addActions exprPl
            <&> rPayload . plData
                <>~ val ^. ExprLens.subExprPayloads . Input.userData
            & lift

mkListAddFirstItem :: MonadA m => ExprIRef.ValIProperty m -> T m EntityId
mkListAddFirstItem = fmap (EntityId.ofValI . snd) . DataOps.addListItem

mkListItem ::
    (MonadA m, Monoid a) =>
    ExpressionU m a ->
    Input.Payload m a -> Val (Input.Payload m a) -> T m EntityId ->
    ListItem m (ExpressionU m a)
mkListItem listItemExpr exprPl tailI addNextItem =
    ListItem
    { _liExpr = listItemExpr
    , _liActions =
      ListItemActions
      { _itemAddNext = addNextItem
      , _itemDelete =
          replaceWith (exprPl ^. Input.stored) (tailI ^. V.payload . Input.stored)
          & void
      }
    }

data ConsParams a = ConsParams
    { cpHead :: a
    , cpTail :: a
    } deriving (Functor, Foldable, Traversable)

getSugaredHeadTail ::
    (MonadPlus m, Monoid a) =>
    ExpressionU f a -> m (ConsParams (ExpressionU f a))
getSugaredHeadTail valS =
    do
        Record [headField, tailField] ClosedRecord{} _ <-
            maybeToMPlus $ valS ^? rBody . _BodyRecord
        guard $ Builtins.headTag == headField ^. rfTag . tagVal
        guard $ Builtins.tailTag == tailField ^. rfTag . tagVal
        return ConsParams
            { cpHead = headField ^. rfExpr
            , cpTail = tailField ^. rfExpr
            }

consTags :: ConsParams T.Tag
consTags = ConsParams Builtins.headTag Builtins.tailTag

valConsParams :: Val a -> Maybe ([a], ConsParams (Val a))
valConsParams val =
    do
        recTail ^? ExprLens.valRecEmpty
        consParams <- MapUtils.matchKeys consTags fields
        let payloads = recTail ^. V.payload : consParams ^.. traverse . _1
        return (payloads, consParams <&> snd)
    where
        (fields, recTail) = RecordVal.unpack val

cons ::
    (MonadA m, Monoid a) =>
    V.Nom (Val (Input.Payload m a)) -> Input.Payload m a ->
    MaybeT (ConvertM m) (ExpressionU m a)
cons (V.Nom nomId val) exprPl =
    do
        nomId == Builtins.streamTid & guard
        lamRes <- val ^? V.body . ExprLens._BAbs . V.lamResult & maybeToMPlus
        let lamPl = val ^. V.payload
        V.Inject tag argI <- lamRes ^? V.body . ExprLens._BInject & maybeToMPlus
        tag == Builtins.consTag & guard
        let injPl = lamRes ^. V.payload
        argS <- ConvertM.convertSubexpression argI & lift
        ConsParams headS tailS <- getSugaredHeadTail argS
        (pls, ConsParams headI tailI) <- valConsParams argI & maybeToMPlus
        List innerValues innerListActions nilGuid <-
            tailS ^? rBody . _BodyList & maybeToMPlus
        let listItem =
                mkListItem headS exprPl tailI (addFirstItem innerListActions)
                & liExpr . rPayload . plData <>~ mconcat
                [ tailS ^. rPayload . plData
                , pls ^. traverse . Input.userData
                , lamPl ^. Input.userData
                , injPl ^. Input.userData
                ]
        let actions =
                ListActions
                { addFirstItem = mkListAddFirstItem (exprPl ^. Input.stored)
                , replaceNil = replaceNil innerListActions
                }
        protectedSetToVal <- lift ConvertM.typeProtectedSetToVal
        let setToInner =
                case (headS ^. rBody, tailS ^. rPayload . plActions . setToInnerExpr) of
                (BodyHole{}, _) ->
                    NoInnerExpr
                    -- When tail has SetToInnerExpr,
                    -- it makes sense for us to also set to same expr.
                    -- However its SetToInnerExpr sets the tail to it
                    -- rather than the whole list.
                    -- We should revamp the SetToInnerExpr mechanism
                    -- and make it generic somehow.
                (_, SetToInnerExpr{}) -> NoInnerExpr
                (_, NoInnerExpr) ->
                    headI ^. V.payload . Input.stored . Property.pVal
                    & protectedSetToVal (exprPl ^. Input.stored)
                    <&> EntityId.ofValI
                    & SetToInnerExpr
        List (listItem : innerValues) actions nilGuid
            & BodyList
            & addActions exprPl & lift
            <&> rPayload . plActions . setToInnerExpr .~ setToInner
