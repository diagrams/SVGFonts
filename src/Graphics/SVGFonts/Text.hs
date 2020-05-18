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
       , map_render
       , shift_glyphs

       , BoundedPath
       , render
       , render_raw
       , render_modifyPreglyphs
       , render_fitRect
       , render_fitRect'
       , fit_height
       , fit_width
       , set_envelope
       , drop_bounds
       ) where

import Control.Arrow (second)

import Data.Default.Class
import Diagrams.Prelude hiding (font, text, render, width, height, envelope)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Graphics.SVGFonts.Fonts (lin)
import Graphics.SVGFonts.ReadFont
import Graphics.SVGFonts.CharReference (characterStrings)

import System.IO.Unsafe (unsafePerformIO)

data TextOpts n = TextOpts
  { textFont   :: PreparedFont n
  , spacing    :: Spacing
  , underline  :: Bool
  }

instance (Read n, RealFloat n) => Default (TextOpts n) where
    def = TextOpts (unsafePerformIO lin) KERN False

data PreparedText n = PreparedText
  { fontTop :: n
  , fontBottom :: n
  , preglyphs :: [(String, n)]
  }

prepare :: (TypeableFloat n) => TextOpts n -> String -> PreparedText n
prepare TextOpts{spacing, textFont=(fontD, _)} text =
  PreparedText (bottom + bbox_dy fontD) bottom (zip preglyphs advances)
  where
    bottom = bbox_ly fontD
    preglyphs = characterStrings' fontD text
    advances = horizontalAdvances preglyphs fontD (isKern spacing)

map_render :: (TypeableFloat n) => TextOpts n -> [(String, n)] -> [Path V2 n]
map_render TextOpts{underline, textFont=(fontD, outl)} preglyphs =
  map polygonChar preglyphs
  where
    ulinePos = underlinePosition fontD
    ulineThickness = underlineThickness fontD

    polygonChar (ch, a) = fromMaybe mempty (Map.lookup ch outl) <> underlineChar a
    underlineChar a
      | underline = translateX (a/2) $ translateY ulinePos (rect a ulineThickness)
      | otherwise = mempty

shift_glyphs :: (TypeableFloat n) => [(n, Path V2 n)] -> [Path V2 n]
shift_glyphs (unzip -> (advs, glyphs)) = zipWith translateX hor_positions glyphs
  where hor_positions = scanl (+) 0 advs

render_raw :: (TypeableFloat n) => TextOpts n -> String -> Path V2 n
render_raw topts text = drop_bounds$ render topts text

type BoundedPath n = (((n, n), n), Path V2 n)

render :: (TypeableFloat n) => TextOpts n -> String -> BoundedPath n
render topts text =
  ( ((fontTop, fontBottom), sum advs)
  , mconcat$ shift_glyphs$ zip advs glyphs
  )
  where
    PreparedText{fontTop, fontBottom, preglyphs} = prepare topts text
    advs = map snd preglyphs
    glyphs = map_render topts preglyphs

fit_height :: (TypeableFloat n) => n -> BoundedPath n -> BoundedPath n
fit_height desired_height (((top, bottom), width), path) =
  (((scale_*top, scale_*bottom), scale_*width), scale scale_ path)
  where scale_ = desired_height / (top - bottom)

fit_width :: (TypeableFloat n) => n -> BoundedPath n -> BoundedPath n
fit_width desired_width (((top, bottom), width), path) =
  (((scale_*top, scale_*bottom), desired_width), scale scale_$ path)
  where scale_ = desired_width / width

render_modifyPreglyphs :: (TypeableFloat n, Monad m) =>
  TextOpts n -> (PreparedText n -> m [(String, n)]) -> String -> m (BoundedPath n)
render_modifyPreglyphs topts modif text = do
  preglyphs <- modif prep
  let advs = map snd preglyphs
      glyphs = map_render topts preglyphs
  return
    ( ((fontTop, fontBottom), sum advs)
    , mconcat$ shift_glyphs$ zip advs glyphs
    )
  where
    prep@PreparedText{fontTop, fontBottom} = prepare topts text

render_fitRect :: forall n. (TypeableFloat n) =>
  TextOpts n -> (n, n) -> String -> (BoundedPath n)
render_fitRect topts (desired_width, desired_height) text =
  fit_height desired_height$ runIdentity$ render_modifyPreglyphs topts modif text
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

render_fitRect' :: forall n. (TypeableFloat n) =>
  TextOpts n -> (n, n) -> n -> String -> (BoundedPath n)
render_fitRect' topts (desired_width, desired_height) space_flexibility text =
  fit_height desired_height$ runIdentity$ render_modifyPreglyphs topts modif text
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

set_envelope :: forall b n. (TypeableFloat n, Renderable (Path V2 n) b) =>
  BoundedPath n -> QDiagram b V2 n Any
set_envelope (((top, bottom), width), path) = path # stroke # withEnvelope envelope
  where
    envelope :: D V2 n
    envelope = translate (r2 (width/2, height/2 + bottom))$ rect width height
    height = top - bottom

drop_bounds :: forall n. (TypeableFloat n) => BoundedPath n -> Path V2 n
drop_bounds = snd

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
