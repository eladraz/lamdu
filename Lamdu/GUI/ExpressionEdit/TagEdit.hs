{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.TagEdit
  ( makeRecordTag, makeParamTag, diveIntoRecordTag
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..), (<>))
import Graphics.UI.Bottle.Animation (AnimId)

import Lamdu.GUI.ExpressionGui (ExpressionGui)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import Lamdu.GUI.ExpressionGui.Types (WidgetT)
import Lamdu.Sugar.AddNames.Types (Name(..))
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

fdConfig :: FocusDelegator.Config
fdConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = [E.ModKey E.noMods E.Key'Enter]
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Rename tag"]
  , FocusDelegator.stopDelegatingKeys = [E.ModKey E.noMods E.Key'Escape]
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Stop renaming tag"]
  }

makeRecordTagInner ::
  MonadA m => Sugar.TagG (Name m) -> Widget.Id -> ExprGuiM m (WidgetT m)
makeRecordTagInner tagG myId = do
  Config.Name{..} <- Config.name <$> ExprGuiM.widgetEnv WE.readConfig
  ExpressionGui.makeNameEdit (tagG ^. Sugar.tagGName) myId
    & ExprGuiM.withFgColor recordTagColor
    <&> Widget.scale (recordTagScaleFactor <&> realToFrac)

makeRecordTag ::
  MonadA m =>
  ExprGuiM.HoleEntityIds -> Sugar.TagG (Name m) -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeRecordTag holeIds tagG myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  jumpHolesEventMap <- ExprEventMap.jumpHolesEventMap [] holeIds
  let
    eventMap =
      jumpHolesEventMap <>
      maybe mempty jumpNextEventMap (holeIds ^. ExprGuiM.hgMNextHole)
  let Config.Name{..} = Config.name config
  ExprGuiM.wrapDelegated fdConfig FocusDelegator.NotDelegating id
    (makeRecordTagInner tagG) myId
    <&> Widget.weakerEvents eventMap
    <&> ExpressionGui.fromValueWidget
  where
    jumpNextEventMap nextHole =
      Widget.keysEventMapMovesCursor [E.ModKey E.noMods E.Key'Space]
      (E.Doc ["Navigation", "Jump to next hole"]) $
      return $ WidgetIds.fromEntityId nextHole

-- | Unfocusable tag view (e.g: in apply params)
makeParamTag ::
  MonadA m => Sugar.TagG (Name m) -> AnimId -> ExprGuiM m (ExpressionGui m)
makeParamTag t animId = do
  Config.Name{..} <- Config.name <$> ExprGuiM.widgetEnv WE.readConfig
  ExpressionGui.makeNameView (t ^. Sugar.tagGName) animId
    & ExprGuiM.widgetEnv
    & ExprGuiM.withFgColor paramTagColor
    <&> Widget.scale (paramTagScaleFactor <&> realToFrac)
    <&> ExpressionGui.fromValueWidget

diveIntoRecordTag :: Widget.Id -> Widget.Id
diveIntoRecordTag = FocusDelegator.delegatingId
