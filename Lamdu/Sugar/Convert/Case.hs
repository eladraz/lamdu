{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Case
    ( convert
    , convertAbsurd
    ) where

import           Prelude.Compat

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (void)
import           Data.Maybe.Utils (unsafeUnjust)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.UUID.Types (UUID)
import           Lamdu.Data.Anchors (assocTagOrder)
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Expr.Val as V
import           Lamdu.Expr.Val.Annotated (Val(..))
import qualified Lamdu.Expr.Val.Annotated as Val
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

-- This is mostly a copy&paste of the Convert.Record module, yuck! DRY
-- with some abstraction?

plValI :: Lens.Lens' (Input.Payload m a) (ExprIRef.ValI m)
plValI = Input.stored . Property.pVal

convertTag :: EntityId -> T.Tag -> TagG UUID
convertTag inst tag = TagG inst tag $ UniqueId.toUUID tag

makeAddAlt :: Monad m =>
    ExprIRef.ValIProperty m ->
    ConvertM m (Transaction m CaseAddAltResult)
makeAddAlt stored =
    do
        typeProtect <- ConvertM.typeProtectTransaction
        do
            mResultI <- DataOps.case_ stored & typeProtect
            case mResultI of
                Just caseRes -> return caseRes
                Nothing ->
                    do
                        caseRes <- DataOps.case_ stored
                        DataOps.setToWrapper (DataOps.crResult caseRes) stored & void
                        return caseRes
                <&> result
            & return
    where
        result (DataOps.CaseResult tag newValI resultI) =
            CaseAddAltResult
            { _caarNewTag = TagG (EntityId.ofRecExtendTag resultEntity) tag ()
            , _caarNewVal = EntityId.ofValI newValI
            , _caarCase = resultEntity
            }
            where
                resultEntity = EntityId.ofValI resultI

convertAbsurd :: Monad m => Input.Payload m a -> ConvertM m (ExpressionU m a)
convertAbsurd exprPl =
    do
        addAlt <- exprPl ^. Input.stored & makeAddAlt
        BodyCase Case
            { _cKind = LambdaCase
            , _cAlts = []
            , _cTail =
                    exprPl ^. Input.stored
                    & DataOps.replaceWithHole
                    <&> EntityId.ofValI
                    & ClosedCase
            , _cAddAlt = addAlt
            , _cEntityId = exprPl ^. Input.entityId
            }
            & addActions exprPl

deleteAlt ::
    Monad m =>
    ExprIRef.ValIProperty m -> ExprIRef.ValI m ->
    Case name0 m (Expression name2 m a1) -> Val (Input.Payload m a) ->
    Expression name1 m0 a0 ->
    ConvertM m (Transaction m EntityId)
deleteAlt stored restI restS expr exprS =
    do
        typeProtect <- ConvertM.typeProtectTransaction
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        return $
            if null (restS ^. cAlts)
            then
                case restS ^. cTail of
                ClosedCase{}
                    | Lens.has (rBody . _BodyHole) exprS ->
                            ExprIRef.newVal $ Val () $ V.BLeaf V.LRecEmpty
                    | otherwise ->
                            -- When deleting closed one alt case
                            -- we replace the case with the alt value
                            -- (unless it is a hole)
                            return (expr ^. Val.payload . plValI)
                CaseExtending{} -> return restI
                >>= protectedSetToVal stored
                <&> EntityId.ofValI
            else do
                let delete = DataOps.replace stored restI
                mResult <- typeProtect delete <&> fmap EntityId.ofValI
                case mResult of
                    Just result -> return result
                    Nothing ->
                        unsafeUnjust "should have a way to fix type error" $
                        case restS ^. cTail of
                        CaseExtending ext ->
                            ext ^? rPayload . plActions . wrap . _WrapAction
                            <&> fmap snd
                        ClosedCase open -> delete >> open & Just

convertAlt ::
    (Monad m, Monoid a) =>
    ExprIRef.ValIProperty m -> ExprIRef.ValI m ->
    Case name m (ExpressionU m a) ->
    EntityId -> T.Tag -> Val (Input.Payload m a) ->
    ConvertM m (CaseAlt UUID m (ExpressionU m a))
convertAlt stored restI restS inst tag expr =
    do
        exprS <- ConvertM.convertSubexpression expr
        delAlt <- deleteAlt stored restI restS expr exprS
        return CaseAlt
            { _caTag = convertTag inst tag
            , _caHandler = exprS
            , _caDelete = delAlt
            }

setTagOrder ::
    Monad m => Int -> CaseAddAltResult -> Transaction m CaseAddAltResult
setTagOrder i r =
    do
        Transaction.setP (assocTagOrder (r ^. caarNewTag . tagVal)) i
        return r

convert ::
    (Monad m, Monoid a) => V.Case (Val (Input.Payload m a)) ->
    Input.Payload m a -> ConvertM m (ExpressionU m a)
convert (V.Case tag val rest) exprPl = do
    restS <- ConvertM.convertSubexpression rest
    (restCase, hiddenEntities) <-
        case restS ^. rBody of
        BodyCase r -> return (r, restS ^. rPayload . plData)
        _ ->
            do
                addAlt <- rest ^. Val.payload . Input.stored & makeAddAlt
                return
                    ( Case
                        { _cKind = LambdaCase
                        , _cAlts = []
                        , _cTail = CaseExtending restS
                        , _cAddAlt = addAlt
                        , _cEntityId = exprPl ^. Input.entityId
                        }
                    , mempty
                    )
    altS <-
        convertAlt
        (exprPl ^. Input.stored) (rest ^. Val.payload . plValI) restCase
        (EntityId.ofCaseTag (exprPl ^. Input.entityId)) tag val
    restCase
        & cAlts %~ (altS:)
        & cAddAlt %~ (>>= setTagOrder (1 + length (restCase ^. cAlts)))
        & BodyCase
        & addActions exprPl
        <&> rPayload . plData <>~ hiddenEntities
