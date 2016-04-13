{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
-- | A font attached to its size

module Graphics.UI.Bottle.Font
    ( Underline(..), underlineColor, underlineWidth
    , render
    , DrawUtils.TextSize(..)
    , DrawUtils.textSize, DrawUtils.textHeight
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.Monoid ((<>))
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.DrawingCombinators ((%%))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.DrawingCombinators.Utils (TextSize(..))
import qualified Graphics.DrawingCombinators.Utils as DrawUtils

data Underline = Underline
    { _underlineColor :: Draw.Color
    , _underlineWidth :: Draw.R
    }
Lens.makeLenses ''Underline

render ::
    Draw.Font -> Draw.Color -> Maybe Underline -> String ->
    (TextSize (Vector2 Draw.R), Draw.Image ())
render font color mUnderline str =
    ( size
    , ( DrawUtils.drawText font str & Draw.tint color
      ) <> maybe mempty (drawUnderline font (DrawUtils.bounding size)) mUnderline
    )
    where
        size = DrawUtils.textSize font str

drawUnderline :: Draw.Font -> Vector2 Draw.R -> Underline -> Draw.Image ()
drawUnderline font size (Underline color width) =
    DrawUtils.square
    & (DrawUtils.scale (Vector2 (size ^. _1) width) %%)
    & (DrawUtils.translate (Vector2 0 (size ^. _2 + descender + width/2)) %%)
    & Draw.tint color
    where
        descender = Draw.fontDescender font
