module Lamdu.GUI.ExpressionEdit.EventMap (make) where

import Control.Applicative ((<$>), (<$), Applicative(..))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Foldable (sequenceA_)
import Data.Monoid (Monoid(..))
import Data.Traversable (sequenceA)
import Graphics.UI.Bottle.Widget (EventHandlers)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Control.Lens as Lens
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.Modify as Modify
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Expression as SugarExpr
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

make ::
  MonadA m => [T m ()] -> Sugar.Expression name m ExprGuiM.Payload ->
  ExprGuiM m (EventHandlers (T m))
make resultPickers sExpr =
  mconcat <$> sequenceA
  [ maybe (return mempty) (actionsEventMap sExpr) $
    pl ^. Sugar.plActions
  , jumpHolesEventMap resultPickers $ pl ^. Sugar.plData . ExprGuiM.plHoleGuids
  ]
  where
    pl = sExpr ^. Sugar.rPayload

jumpHolesEventMap :: MonadA m => [T m ()] -> ExprGuiM.HoleGuids -> ExprGuiM m (EventHandlers (T m))
jumpHolesEventMap resultPickers hg = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    doc dirStr = E.Doc ["Navigation", "Jump to " ++ dirStr ++ " hole"]
    jumpEventMap keys dirStr lens =
      maybe mempty
      (Widget.keysEventMapMovesCursor (keys config) (doc dirStr) .
       (<$ sequenceA_ resultPickers) . WidgetIds.fromGuid) $ hg ^. lens
  pure $ mconcat
    [ jumpEventMap Config.jumpToNextHoleKeys "next" ExprGuiM.hgMNextHole
    , jumpEventMap Config.jumpToPrevHoleKeys "previous" ExprGuiM.hgMPrevHole
    ]

actionsEventMap ::
  MonadA m =>
  Sugar.Expression name m a -> Sugar.Actions m ->
  ExprGuiM m (EventHandlers (T m))
actionsEventMap sExpr actions = do
  isSelected <- ExprGuiM.widgetEnv . WE.isSubCursor $ WidgetIds.fromGuid exprGuid
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    delKeys = Config.replaceKeys config ++ Config.delKeys config
    replace
      | isSelected = Modify.replaceEventMap config actions
      | otherwise =
        mkEventMap delKeys (E.Doc ["Navigation", "Select parent"])
        FocusDelegator.notDelegatingId $ return exprGuid
  clipboard <-
    case sExpr ^? Sugar.rBody . SugarExpr.bodyHole of
    Just hole -> pasteEventMap hole
    Nothing ->
      return .
      mkEventMap (Config.cutKeys config) (E.Doc ["Edit", "Cut"]) id $
      actions ^. Sugar.cut
  return $ mconcat
    [ Modify.wrapEventMap config actions
    , replace
    , clipboard
    ]
  where
    exprGuid = sExpr ^. Sugar.rPayload . Sugar.plGuid
    mkEventMap keys doc f =
      Widget.keysEventMapMovesCursor keys doc .
      fmap (f . WidgetIds.fromGuid)

pasteEventMap ::
  MonadA m =>
  Sugar.Hole name m expr ->
  ExprGuiM m (Widget.EventHandlers (T m))
pasteEventMap hole = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  return .
    maybe mempty
    (Widget.keysEventMapMovesCursor
     (Config.pasteKeys config) (E.Doc ["Edit", "Paste"]) .
     fmap WidgetIds.fromGuid) $
    hole ^? Sugar.holeMActions . Lens._Just . Sugar.holePaste . Lens._Just
