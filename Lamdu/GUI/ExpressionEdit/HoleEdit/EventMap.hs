{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.EventMap
    ( blockDownEvents, disallowCharsFromSearchTerm
    , makeOpenEventMaps
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import qualified Data.Foldable as Foldable
import           Data.Functor.Identity (Identity(..))
import           Data.List (isInfixOf)
import           Data.List.Class (List)
import qualified Data.List.Class as List
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Store.Property (Property(..))
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import qualified Graphics.UI.Bottle.ModKey as ModKey
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.GLFW as GLFW
import           Lamdu.CharClassification (operatorChars, bracketChars, digitChars)
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Info as HoleInfo
import           Lamdu.GUI.ExpressionEdit.HoleEdit.ShownResult (PickedResult(..), ShownResult(..))
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

import           Prelude.Compat

type T = Transaction.Transaction

blockDownEvents :: Monad f => Widget f -> Widget f
blockDownEvents =
    Widget.weakerEvents $
    E.keyPresses
    [ModKey mempty GLFW.Key'Down]
    (E.Doc ["Navigation", "Move", "down (blocked)"]) $
    return mempty

adHocTextEditEventMap :: Monad m => Config.Hole -> Property m String -> Widget.EventHandlers m
adHocTextEditEventMap holeConfig searchTermProp =
    appendCharEventMap <> deleteCharEventMap
    where
        appendCharEventMap =
            E.allChars "Character"
            (E.Doc ["Edit", "Search Term", "Append character"])
            (changeText . snoc)
            & disallowCharsFromSearchTerm holeConfig searchTerm
            & if null searchTerm
              then E.filterChars (`notElem` operatorChars)
              else id
        deleteCharEventMap
            | null searchTerm = mempty
            | otherwise =
                  E.keyPress (ModKey mempty GLFW.Key'Backspace)
                  (E.Doc ["Edit", "Search Term", "Delete backwards"]) $
                  changeText init
        snoc x = (++ [x])
        searchTerm = Property.value searchTermProp
        changeText f = mempty <$ Property.pureModify searchTermProp f

disallowedHoleChars :: String
disallowedHoleChars = "`\n "

toLiteralTextKeys :: [ModKey]
toLiteralTextKeys =
    [ ModKey.shift GLFW.Key'Apostrophe
    , ModKey mempty GLFW.Key'Apostrophe
    ]

disallowCharsFromSearchTerm :: Config.Hole -> String -> E.EventMap a -> E.EventMap a
disallowCharsFromSearchTerm Config.Hole{..} searchTerm =
    E.filterChars (`notElem` disallowedHoleChars) .
    deleteKeys
    (holePickAndMoveToNextHoleKeys ++ holePickResultKeys) .
    disallowMix
    where
        allowOnly group = E.filterChars (`elem` group)
        disallow group = E.filterChars (`notElem` group)
        disallowMix =
            case searchTerm of
            "" -> id
            '"':_ -> id
            "." -> disallow bracketChars
            '.':x:_
                | x `elem` operatorChars -> allowOnly operatorChars
                | otherwise -> disallow operatorChars . disallow bracketChars
            x:xs
                | x `elem` operatorChars -> allowOnly operatorChars
                | x `elem` bracketChars -> allowOnly bracketChars
                | "." `isInfixOf` xs -> allowOnly digitChars
                | all (`elem` digitChars) searchTerm -> allowOnly ('.':digitChars)
                | all (`notElem` operatorChars) searchTerm -> disallow operatorChars
                | otherwise ->
                  -- Mix of operator/non-operator chars happened in search term
                  -- This can happen when editing a literal text, allow everything
                  id

deleteKeys :: [ModKey] -> E.EventMap a -> E.EventMap a
deleteKeys = E.deleteKeys . map (E.KeyEvent GLFW.KeyState'Pressed)

pickEventMap ::
    Monad m => Config.Hole -> HoleInfo m -> ShownResult m ->
    Widget.EventHandlers (T m)
pickEventMap Config.Hole{..} holeInfo shownResult =
    -- TODO: Does this entityId business make sense?
    case hiNearestHoles holeInfo ^. NearestHoles.next of
    Just nextHoleEntityId | not (srHasHoles shownResult) ->
        simplePickRes holePickResultKeys <>
        pickAndMoveToNextHole nextHoleEntityId
    _ ->
        simplePickRes $
        holePickResultKeys ++
        holePickAndMoveToNextHoleKeys
    <&> pickBefore shownResult
    where
        pickAndMoveToNextHole nextHoleEntityId =
            Widget.keysEventMapMovesCursor holePickAndMoveToNextHoleKeys
            (E.Doc ["Edit", "Result", "Pick and move to next hole"]) .
            return $ WidgetIds.fromEntityId nextHoleEntityId
        simplePickRes keys =
            Widget.keysEventMap keys (E.Doc ["Edit", "Result", "Pick"]) $
            return ()

pickBefore :: Monad m => ShownResult m -> T m Widget.EventResult -> T m Widget.EventResult
pickBefore shownResult action =
    do
        PickedResult{..} <- srPick shownResult
        actionResult <-
            action
            <&> Widget.eCursor . Lens._Wrapped' . Lens.mapped %~
                _pickedIdTranslations
        return $ _pickedEventResult <> actionResult

-- | Remove unwanted event handlers from a hole result
removeUnwanted :: Config -> Widget.EventHandlers f -> Widget.EventHandlers f
removeUnwanted config =
    deleteKeys
    (delKeys ++ gridKeyEvents ++ holeNavigationKeys ++ subexprNavigationKeys ++
     letAddItemKeys)
    where
        letAddItemKeys = Config.letAddItemKeys config
        subexprNavigationKeys =
            config & Config.leaveSubexpressionKeys <> Config.enterSubexpressionKeys
        Config.Hole{..} = Config.hole config
        holeNavigationKeys = holeOpenKeys ++ holeCloseKeys
        gridKeyEvents = Foldable.toList Grid.stdKeys
        delKeys = Config.delKeys config

mkEventsOnPickedResult :: Monad m => ShownResult m -> ExprGuiM m (Widget.EventHandlers (T m))
mkEventsOnPickedResult shownResult =
    do
        config <- ExprGuiM.readConfig
        srMkEventMap shownResult
            <&> E.emDocs . E.docStrs . Lens._last %~ (++ " (On picked result)")
            <&> Lens.mapped %~ pickBefore shownResult
            <&> removeUnwanted config

-- To make HoleEdit
emptyPickEventMap :: Monad f => Config.Hole -> Widget.EventHandlers f
emptyPickEventMap Config.Hole{..} =
    return ()
    & Widget.keysEventMap keys (E.Doc ["Edit", "Result", "Pick (N/A)"])
    where
        keys = holePickResultKeys ++ holePickAndMoveToNextHoleKeys

listTHead :: List t => b -> t b -> List.ItemM t b
listTHead nil l =
    l
    & List.runList
    <&> \case
        List.Nil -> nil
        List.Cons item _ -> item

-- TODO: This is ugly, maybe Sugar.HoleOption should
-- have a canonical result?
toLiteralTextEventMap :: Monad m => HoleInfo m -> Widget.EventHandlers (T m)
toLiteralTextEventMap holeInfo =
    Widget.keysEventMapMovesCursor toLiteralTextKeys
    (E.Doc ["Edit", "Create Text Literal"]) $
    do
        (_score, mkResult) <-
            Sugar.LiteralText (Identity "")
            & hiHole holeInfo ^. Sugar.holeActions . Sugar.holeOptionLiteral
            <&> (^. Sugar.hoResults)
            >>= listTHead (error "Literal hole option has no results?!")
        result <- mkResult
        pickedResult <- result ^. Sugar.holeResultPick
        result ^. Sugar.holeResultConverted . Sugar.rPayload . Sugar.plEntityId
            & (`lookup` (pickedResult ^. Sugar.prIdTranslation))
            & fromMaybe (error "PickedResult missing translation for expr")
            & WidgetIds.fromEntityId
            & WidgetIds.delegatingId
            & pure

makeOpenEventMaps ::
    Monad m =>
    HoleInfo m -> Maybe (ShownResult m) ->
    ExprGuiM m
    ( Widget.EventHandlers (T m)
    , Widget.EventHandlers (T m)
    )
makeOpenEventMaps holeInfo mShownResult =
    do
        holeConfig <- ExprGuiM.readConfig <&> Config.hole
        -- below ad-hoc and search term edit:
        eventMap <-
            case mShownResult of
            Nothing -> pure (emptyPickEventMap holeConfig)
            Just shownResult ->
                mkEventsOnPickedResult shownResult
                <&> mappend (pickEventMap holeConfig holeInfo shownResult)
                <&> deleteKeys toLiteralTextKeys
        let adHocEdit = adHocTextEditEventMap holeConfig searchTermProp
        (eventMap, adHocEdit <> eventMap)
            & Lens.both %~ mappend maybeLiteralTextEventMap
            & pure
    where
        maybeLiteralTextEventMap
            | null (searchTermProp ^. Property.pVal) = toLiteralTextEventMap holeInfo
            | otherwise = mempty
        searchTermProp = HoleInfo.hiSearchTermProperty holeInfo
