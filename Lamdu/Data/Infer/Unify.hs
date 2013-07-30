{-# LANGUAGE PatternGuards, RecordWildCards #-}
module Lamdu.Data.Infer.Unify
  ( unify, forceLam, normalizeScope
  ) where

import Control.Applicative ((<$>), (<$), Applicative(..))
import Control.Lens.Operators
import Control.Monad (when, unless, guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Decycle (DecycleT, runDecycleT, visit)
import Control.Monad.Trans.State (state)
import Control.Monad.Trans.Writer (WriterT(..))
import Data.Foldable (traverse_)
import Data.Maybe.Utils (unsafeUnjust)
import Data.Monoid (Monoid(..))
import Data.Monoid.Applicative (ApplicativeMonoid(..))
import Data.Store.Guid (Guid)
import Data.Traversable (sequenceA)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer, Error(..))
import Lamdu.Data.Infer.RefTags (ExprRef, TagParam)
import System.Random (Random, random)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.List as List
import qualified Data.OpaqueRef as OR
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer.GuidAliases as GuidAliases
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.Trigger as Trigger

newRandom :: Random r => Infer def r
newRandom = InferM.liftContext . Lens.zoom ctxRandomGen $ state random

forceLam :: Eq def => Expr.Kind -> Scope def -> ExprRef def -> Infer def (Guid, ExprRef def, ExprRef def)
forceLam k lamScope destRef = do
  newGuid <- newRandom
  newParamRep <- InferM.liftContext . Lens.zoom ctxGuidAliases $ GuidAliases.getRep newGuid
  newParamTypeRef <- InferM.liftUFExprs . fresh lamScope $ ExprLens.bodyHole # ()
  -- TODO: Directly manipulate RefData to avoid scope buildup?
  let lamResultScope = lamScope & scopeMap %~ ((newParamRep, newParamTypeRef) :)
  newResultTypeRef <- InferM.liftUFExprs . fresh lamResultScope $ ExprLens.bodyHole # ()
  newLamRef <-
    InferM.liftUFExprs . fresh lamScope . Expr.BodyLam $
    Expr.Lam k newGuid newParamTypeRef newResultTypeRef
  rep <- unify newLamRef destRef
  body <- InferM.liftUFExprs $ (^. rdBody) <$> State.gets (UFData.readRep rep)
  return . unsafeUnjust "We just unified Lam into rep" $
    body ^? ExprLens.bodyKindedLam k

normalizeScope :: Scope def -> Infer def (OR.RefMap (TagParam def) (ExprRef def))
normalizeScope (Scope scope) =
  scope
  & Lens.traverse . Lens._1 %%~
    InferM.liftContext . Lens.zoom ctxGuidAliases . GuidAliases.find
  <&> OR.refMapFromList

-- If we don't assert that the scopes have same refs we could be pure
intersectScopes :: Scope def -> Scope def -> Infer def (Scope def)
intersectScopes aScope bScope = do
  aScopeNorm <- normalizeScope aScope
  bScopeNorm <- normalizeScope bScope
  Scope . OR.refMapToList <$> sequenceA (OR.refMapIntersectionWith verifyEquiv aScopeNorm bScopeNorm)
  where
    -- Expensive assertion
    verifyEquiv aref bref = do
      equiv <- InferM.liftUFExprs $ UFData.equiv aref bref
      if equiv
        then return aref
        else error "Scope unification of differing refs"

newtype HoleConstraints def = HoleConstraints
  { hcUnusableScopeReps :: OR.RefSet (TagParam def)
  }

-- You must apply this recursively
checkHoleConstraints :: HoleConstraints def -> Expr.Body def (ExprRef def) -> Infer def ()
checkHoleConstraints (HoleConstraints unusableSet) body =
  case body of
  Expr.BodyLeaf (Expr.GetVariable (Expr.ParameterRef paramGuid)) -> do
    paramIdRep <- getRep paramGuid
    when (unusableSet ^. Lens.contains paramIdRep) $
      InferM.error $ VarEscapesScope paramGuid
  -- Expensive assertion
  Expr.BodyLam lam -> do
    paramIdRep <- getRep (lam ^. Expr.lamParamId)
    when (unusableSet ^. Lens.contains paramIdRep) $
      error "checkHoleConstraints: Shadowing detected"
  _ -> return ()
  where
    getRep = InferM.liftContext . Lens.zoom ctxGuidAliases . GuidAliases.getRep

type U def = DecycleT (ExprRef def) (Infer def)

uInfer :: Infer def a -> U def a
uInfer = lift

type WU def = WriterT (ApplicativeMonoid (U def) ()) (U def)
wuInfer :: Infer def a -> WU def a
wuInfer = lift . uInfer
wuRun :: WU def a -> U def (a, U def ())
wuRun = fmap (Lens._2 %~ runApplicativeMonoid) . runWriterT
wuLater :: U def () -> WU def ()
wuLater = Writer.tell . ApplicativeMonoid

unifyWithHole ::
  Eq def => Scope def -> Scope def -> Expr.Body def (ExprRef def) ->
  WU def (Scope def, Expr.Body def (ExprRef def))
unifyWithHole holeScope otherScope nonHoleBody = do
  unusableScopeReps <-
    wuInfer $
    OR.refMapKeysSet <$>
    ( OR.refMapDifference
      <$> normalizeScope otherScope
      <*> normalizeScope holeScope
    )
  if OR.refSetNull unusableScopeReps
    then return (otherScope, nonHoleBody)
    else
      applyHoleConstraints (HoleConstraints unusableScopeReps)
      nonHoleBody otherScope
      <&> flip (,) nonHoleBody

mergeScopeBodies ::
  Eq def =>
  Scope def -> Expr.Body def (ExprRef def) ->
  Scope def -> Expr.Body def (ExprRef def) ->
  WU def (Scope def, Expr.Body def (ExprRef def))
mergeScopeBodies xScope xBody yScope yBody = do
  case (xBody, yBody) of
    (_, Expr.BodyLeaf Expr.Hole) -> unifyWithHole yScope xScope xBody
    (Expr.BodyLeaf Expr.Hole, _) -> unifyWithHole xScope yScope yBody
    _ -> do
      intersectedScope <- wuInfer $ intersectScopes xScope yScope
      wuLater $
        handleMatchResult =<<
        ExprUtil.matchBody matchLamResult unifyRecurse matchGetPars xBody yBody
      return (intersectedScope, yBody)
  where
    zoomGuidAliases = uInfer . InferM.liftContext . Lens.zoom ctxGuidAliases
    handleMatchResult Nothing = uInfer . InferM.error $ Mismatch xBody yBody
    handleMatchResult (Just _) = return ()
    matchLamResult xGuid yGuid xRef yRef = do
      (_guidRep, resGuid) <- zoomGuidAliases $ GuidAliases.unify xGuid yGuid
      (,) resGuid <$> unifyRecurse xRef yRef
    matchGetPars xGuid yGuid = zoomGuidAliases $ do
      xRep <- GuidAliases.getRep xGuid
      yRep <- GuidAliases.getRep yGuid
      return $ yGuid <$ guard (xRep == yRep)

mergeRefData ::
  Eq def => RefData def -> RefData def ->
  WU def (Bool, RefData def)
mergeRefData
  (RefData aScope aRelations aIsCircumsized aTriggers aBody)
  (RefData bScope bRelations bIsCircumsized bTriggers bBody) =
  mkRefData <$> mergeScopeBodies aScope aBody bScope bBody
  where
    bodyIsUpdated =
      Lens.has ExprLens.bodyHole aBody /=
      Lens.has ExprLens.bodyHole bBody
    mergedRelations = aRelations ++ bRelations
    mkRefData (intersectedScope, mergedBody) =
      ( bodyIsUpdated
      , RefData
        { _rdScope = intersectedScope
        , _rdRelations = mergedRelations
        , _rdIsCircumsized = mappend aIsCircumsized bIsCircumsized
        , _rdTriggers = OR.refMapUnionWith mappend aTriggers bTriggers
        , _rdBody = mergedBody
        }
      )

mergeRefDataAndTrigger ::
  Eq def =>
  ExprRef def -> RefData def -> RefData def ->
  WU def (Bool, RefData def)
mergeRefDataAndTrigger rep a b =
  mergeRefData a b >>= Lens._2 %%~ wuInfer . Trigger.updateRefData rep

applyHoleConstraints ::
  Eq def => HoleConstraints def ->
  Expr.Body def (ExprRef def) -> Scope def ->
  WU def (Scope def)
applyHoleConstraints holeConstraints body oldScope = do
  wuInfer $ checkHoleConstraints holeConstraints body
  let isUnusable x = hcUnusableScopeReps holeConstraints ^. Lens.contains x
  oldScopeNorm <- wuInfer $ normalizeScope oldScope
  let (unusables, usables) = List.partition (isUnusable . fst) $ OR.refMapToList oldScopeNorm
  unless (null unusables) . wuLater $
    (traverse_ . holeConstraintsRecurse . HoleConstraints . OR.refSetFromList . map fst) unusables body
  return $ Scope usables

decycleDefend :: ExprRef def -> (ExprRef def -> U def (ExprRef def)) -> U def (ExprRef def)
decycleDefend ref action = do
  nodeRep <- lift . InferM.liftUFExprs $ UFData.find "holeConstraintsRecurse:rawNode" ref
  mResult <- visit nodeRep (action nodeRep)
  case mResult of
    Nothing -> lift . InferM.error $ InfiniteExpression nodeRep
    Just result -> return result

holeConstraintsRecurse ::
  Eq def => HoleConstraints def -> ExprRef def -> U def (ExprRef def)
holeConstraintsRecurse holeConstraints rawNode =
  decycleDefend rawNode $ \nodeRep -> do
    oldNodeData <- lift . InferM.liftUFExprs $ State.gets (UFData.readRep nodeRep)
    lift . InferM.liftUFExprs . UFData.writeRep nodeRep $
      error "Reading node during write..."
    (newRefData, later) <-
      wuRun $
      oldNodeData
      & rdScope %%~
        applyHoleConstraints holeConstraints
        (oldNodeData ^. rdBody)
    uInfer . InferM.liftUFExprs $ UFData.writeRep nodeRep newRefData
    later
    return nodeRep

unifyRecurse ::
  Eq def => ExprRef def -> ExprRef def -> U def (ExprRef def)
unifyRecurse rawNode other =
  decycleDefend rawNode $ \nodeRep -> do
    (rep, unifyResult) <- lift . InferM.liftUFExprs $ UFData.unifyRefs nodeRep other
    case unifyResult of
      UFData.UnifyRefsAlreadyUnified -> return ()
      UFData.UnifyRefsUnified xData yData -> do
        ((bodyIsUpdated, mergedRefData), later) <-
          wuRun $ mergeRefDataAndTrigger rep xData yData
        -- First let's write the mergedRefData so we're not in danger zone
        -- of reading missing data:
        lift . InferM.liftUFExprs $ UFData.write rep mergedRefData
        -- Now lets do the deferred recursive unifications:
        later
        -- TODO: Remove this when replaced Relations with Rules
        -- Now we can safely re-run the relations
        when bodyIsUpdated . lift $ InferM.rerunRelations rep
    return rep

unify :: Eq def => ExprRef def -> ExprRef def -> Infer def (ExprRef def)
unify x y = runDecycleT $ unifyRecurse x y