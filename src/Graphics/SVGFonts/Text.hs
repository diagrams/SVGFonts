{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module Graphics.SVGFonts.Text
       ( -- * Setting text as a path using a font.
         TextOpts(..)
       , Spacing(..)

       , horizontalAdvances
       , isKern
       , characterStrings'

       , PreparedText(..)
       , prepare
       , draw_glyphs
       , shift_glyphs

       , svgText
       , svgText_raw
       , svgText_modifyPreglyphs
       , svgText_fitRect
       , svgText_fitRect_stretchySpace

       , textSVG
       ) where

import Control.Arrow (second)

import Data.Default
import Diagrams.Prelude hiding (font, text, width, height, envelope)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Graphics.SVGFonts.Fonts (lin)
import Graphics.SVGFonts.ReadFont
import Graphics.SVGFonts.CharReference (characterStrings)
import Graphics.SVGFonts.PathInRect (PathInRect(..), drop_rect, fit_height)

import System.IO.Unsafe (unsafePerformIO)

data TextOpts n = TextOpts
  { textFont   :: PreparedFont n
  , spacing    :: Spacing
  , underline  :: Bool
  }

instance (Read n, RealFloat n) => Default (TextOpts n) where
  def = TextOpts (unsafePerformIO lin) KERN False

data PreparedText n = PreparedText
  { fontTop :: n  -- ^ y position of font top.
  , fontBottom :: n
  -- ^ y position of font bottom
  -- (usually negative unless the characters are fully above baseline).
  , preglyphs :: [(String, n)]
  -- Ligatures/singleton characters along with their advances (widths).
  }

-- | Break text into preglyphs (= ligatures and singleton characters) and
-- compute their advances.
prepare :: (RealFloat n) => TextOpts n -> String -> PreparedText n
prepare TextOpts{spacing, textFont=(fontD, _)} text =
  PreparedText (bottom + bbox_dy fontD) bottom (zip preglyphs advances)
  where
    bottom = bbox_ly fontD
    preglyphs = characterStrings' fontD text
    advances = horizontalAdvances preglyphs fontD (isKern spacing)

-- | Create a path (glyph) for each preglyph.
draw_glyphs :: (RealFloat n) => TextOpts n -> [(String, n)] -> [Path V2 n]
draw_glyphs TextOpts{underline, textFont=(fontD, outl)} preglyphs =
  map polygonChar preglyphs
  where
    ulinePos = underlinePosition fontD
    ulineThickness = underlineThickness fontD

    polygonChar (ch, a) = fromMaybe mempty (Map.lookup ch outl) <> underlineChar a
    underlineChar a
      | underline = translateX (a/2) $ translateY ulinePos (rect a ulineThickness)
      | otherwise = mempty

-- | Position glyphs according to their advances.
shift_glyphs :: (RealFloat n) => [(n, Path V2 n)] -> [Path V2 n]
shift_glyphs (unzip -> (advs, glyphs)) = zipWith translateX hor_positions glyphs
  where hor_positions = scanl (+) 0 advs

-- | Simply render path from text.
svgText_raw :: (RealFloat n) => TextOpts n -> String -> Path V2 n
svgText_raw topts text = drop_rect$ svgText topts text

-- | Render 'PathInRect' from text. The enclosing rectangle, computed from the
-- font, is kept, to be able to e.g. correctly position lines of text one above other.
svgText :: (RealFloat n) => TextOpts n -> String -> PathInRect n
svgText topts text = PathInRect 0 fontBottom (sum advs) fontTop$
  mconcat$ shift_glyphs$ zip advs glyphs
  where
    PreparedText{fontTop, fontBottom, preglyphs} = prepare topts text
    advs = map snd preglyphs
    glyphs = draw_glyphs topts preglyphs

-- | Like 'svgText' but preglyphs can be modified using the given monad before
-- 'draw_glyphs' is called. Simple examples of this function's specializations are e.g.
-- 'svgText_fitRect' and 'svgText_fitRect_stretchySpace'.
svgText_modifyPreglyphs :: (RealFloat n, Monad m) =>
  TextOpts n -> (PreparedText n -> m [(String, n)]) -> String -> m (PathInRect n)
svgText_modifyPreglyphs topts modif text = do
  preglyphs <- modif prep
  let advs = map snd preglyphs
      glyphs = draw_glyphs topts preglyphs
  return$ PathInRect 0 fontBottom (sum advs) fontTop$
    mconcat$ shift_glyphs$ zip advs glyphs
  where
    prep@PreparedText{fontTop, fontBottom} = prepare topts text

-- | Like 'svgText' but a rectangle is provided, into which the text will
-- fit. The text is scaled according to the height of the rectengle. The glyphs
-- are interleaved with even spaces to fit the width of the rectangle. The text
-- must have at least two characters for correct functionality.
svgText_fitRect :: forall n. (RealFloat n) =>
  TextOpts n -> (n, n) -> String -> (PathInRect n)
svgText_fitRect topts (desired_width, desired_height) text =
  fit_height desired_height$ runIdentity$ svgText_modifyPreglyphs topts modif text
  where
    modif :: PreparedText n -> Identity [(String, n)]
    modif PreparedText{fontTop, fontBottom, preglyphs} =
      return$ map (second (+ addition)) (init preglyphs) ++ [last preglyphs]
      where
        scale_ = desired_height / (fontTop - fontBottom)

        advs = map snd preglyphs

        width = sum (init advs)
        desired_width' = desired_width / scale_ - last advs

        addition = (desired_width' - width) / fromIntegral (length advs - 1)

-- | Like 'svgText_fitRect' but space characters are stretched @k@ times more
-- than others for @svgText_fitRect_stretchySpace opts (w, h) k text@.
svgText_fitRect_stretchySpace :: forall n. (RealFloat n) =>
  TextOpts n -> (n, n) -> n -> String -> (PathInRect n)
svgText_fitRect_stretchySpace
  topts
  (desired_width, desired_height)
  space_flexibility
  text
  =
  fit_height desired_height$ runIdentity$ svgText_modifyPreglyphs topts modif text
  where
    modif :: PreparedText n -> Identity [(String, n)]
    modif PreparedText{fontTop, fontBottom, preglyphs} =
      return$ scaled_preglyphs' ++ [last_preglyph]
      where
        scale_ = desired_height / (fontTop - fontBottom)

        scaled_preglyphs = init preglyphs
        last_preglyph = last preglyphs

        width = sum$ map snd scaled_preglyphs
        desired_width' = desired_width / scale_ - snd last_preglyph

        width_diff = desired_width' - width

        weight " " = space_flexibility
        weight _ = 1

        weights = map weight$ map fst$ init preglyphs
        additions = map (*coef) weights
          where coef = width_diff / sum weights

        scaled_preglyphs' =
          zipWith (\add (c, adv) -> (c, adv + add)) additions scaled_preglyphs

characterStrings' :: FontData n -> String -> [String]
characterStrings' fontD = \text -> map T.unpack $ characterStrings text ligatures
  where ligatures = filter ((>1) . length) . Map.keys . fontDataGlyphs$ fontD


-- | See <http://en.wikipedia.org/wiki/Kerning>
--
data Spacing = HADV -- ^ Every glyph has a unique horiz. advance
                    --
                    --  <<diagrams/src_Graphics_SVGFonts_ReadFont_textHADV.svg#diagram=textHADV&width=400>>
             | KERN -- ^ Recommended, same as HADV but sometimes overridden by kerning:
                    -- As You can see there is less space between \"A\" and \"V\":
                    --
                    --   <<diagrams/src_Graphics_SVGFonts_ReadFont_textKern.svg#diagram=textKern&width=400>>
             deriving Show


isKern :: Spacing -> Bool
isKern KERN = True
isKern _    = False


-- | Horizontal advances of characters inside a string.
-- A character is stored with a string (because of ligatures like \"ffi\").
horizontalAdvances :: RealFloat n => [String] -> FontData n -> Bool -> [n]
horizontalAdvances []          _  _       = []
horizontalAdvances [ch]        fd _       = [horizontalAdvance ch fd]
horizontalAdvances (ch0:ch1:s) fd kerning =
  ((horizontalAdvance ch0 fd) - (ka (fontDataKerning fd)))
  : (horizontalAdvances (ch1:s) fd kerning)
  where ka kern | kerning   = (kernAdvance ch0 ch1 kern True) + (kernAdvance ch0 ch1 kern False)
                | otherwise = 0


------------------------ Backward Compatibility Layer ------------------------

textSVG :: (Read n, RealFloat n) => String -> n -> Path V2 n
textSVG text height = drop_rect$ fit_height height$ svgText def text
