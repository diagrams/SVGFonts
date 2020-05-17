{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.SVGFonts.Text
       ( -- * Setting text as a path using a font.

         TextOpts(..)
       , Mode(..)
       , Spacing(..)

       , textSVG
       , textSVG'
       , textSVG_
       , textSVGStretchingWeights

       ) where

import Data.Default.Class
import Diagrams.Prelude hiding (font, text)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Graphics.SVGFonts.Fonts (lin)
import Graphics.SVGFonts.ReadFont
import Graphics.SVGFonts.CharReference (characterStrings)

import System.IO.Unsafe (unsafePerformIO)

data TextOpts n = TextOpts
  { textFont       :: PreparedFont n
  , mode       :: Mode
  , spacing    :: Spacing
  , underline  :: Bool
  , textWidth  :: n
  , textHeight :: n
  }

instance (Read n, RealFloat n) => Default (TextOpts n) where
    def = TextOpts (unsafePerformIO lin) INSIDE_H KERN False 1 1

-- | A short version of textSVG' with standard values. The Double value is the height.
--
-- > import Graphics.SVGFonts
-- >
-- > textSVGExample = stroke $ textSVG "Hello World" 1
--
-- <<diagrams/src_Graphics_SVGFonts_ReadFont_textSVGExample.svg#diagram=textSVGExample&width=300>>
textSVG :: (Read n, RealFloat n) => String -> n -> Path V2 n
textSVG t h = textSVG' with { textHeight = h } t

-- | Create a path from the given text and options.
--   The origin is at the center of the text and the boundaries are
--   given by the outlines of the chars.
--
-- > import Graphics.SVGFonts
-- >
-- > text' t = stroke (textSVG' (TextOpts lin INSIDE_H KERN False 1 1) t)
-- >            # fc blue # lc blue # bg lightgrey # fillRule EvenOdd # showOrigin
-- >
-- > textPic0 = (text' "Hello World") # showOrigin
--
-- <<diagrams/src_Graphics_SVGFonts_ReadFont_textPic0.svg#diagram=textPic0&width=300>>
textSVG' :: RealFloat n => TextOpts n -> String -> Path V2 n
textSVG' topts text =
  case mode topts of
    INSIDE_WH -> makeString (textHeight topts * sumh / maxY)
                            (textHeight topts) (textWidth topts / (textHeight topts * sumh / maxY))
    INSIDE_W  -> makeString (textWidth topts) -- the third character is used to scale horizontal advances
                            (textWidth topts * maxY / sumh) 1
    INSIDE_H  -> makeString (textHeight topts * sumh / maxY) (textHeight topts) 1
  where
    makeString w h space = (scaleY (h/maxY) $ scaleX (w/sumh) $
                            mconcat $
                            zipWith translate (horPos space)
                           (map polygonChar (zip str (adjusted_hs space))) ) # centerXY
    (fontD,outl) = textFont topts
    polygonChar (ch,a) = (fromMaybe mempty (Map.lookup ch outl)) <> (underlineChar a)
    underlineChar a | underline topts = translateY ulinePos (rect a ulineThickness)
                    | otherwise = mempty
    ulinePos = underlinePosition fontD
    ulineThickness = underlineThickness fontD
    horPos space = reverse $ added ( zero : (map (unitX ^*) (adjusted_hs space)) )
    adjusted_hs space = map (*space) hs
    hs = horizontalAdvances str fontD (isKern (spacing topts))
    sumh = sum hs
    added = snd.(foldl (\(h,l) (b,_) -> (h ^+^ b, (h ^+^ b):l))
                       (zero,[])).  (map (\x->(x,[]))) -- [o,o+h0,o+h0+h1,..]
    maxY = bbox_dy fontD -- max height of glyph

    str = characterStrings' fontD text


-- | Create a path from the given text and options.
-- The origin is at the left end of the baseline of of the text and the boundaries
-- are given by the bounding box of the Font. This is best for combining Text of different
-- fonts and for several lines of text.
-- As you can see you can also underline text by setting underline to True.
--
-- > import Graphics.SVGFonts
-- >
-- > text'' t = (textSVG_ (TextOpts lin INSIDE_H KERN True 1 1) t)
-- >            # fc blue # lc blue # bg lightgrey # fillRule EvenOdd # showOrigin
-- >
-- > textPic1 = text'' "Hello World"
--
-- <<diagrams/src_Graphics_SVGFonts_ReadFont_textPic1.svg#diagram=textPic1&width=300>>
textSVG_ :: forall b n. (TypeableFloat n, Renderable (Path V2 n) b) =>
            TextOpts n -> String -> QDiagram b V2 n Any
textSVG_ = textSVGStretchingWeights (\_ _ -> 1)

-- | Create a path from the given text and options. Provided stretching
-- function sets weights for spacing between characters (type of character is
-- String because of ligatures) used for 'INSIDE_WH' stretching. Spaces with
-- lower weights will stretch less than the ones with the higher weights. For
-- the edge case where sum of weights is zero, equal stretching is applied.
--
-- The origin is at the left end of the baseline of of the text and the boundaries
-- are given by the bounding box of the Font. This is best for combining Text of different
-- fonts and for several lines of text.
-- As you can see you can also underline text by setting underline to True.
--
-- > import Graphics.SVGFonts
-- >
-- > text'' t = (textSVG_ (TextOpts lin INSIDE_H KERN True 1 1) t)
-- >            # fc blue # lc blue # bg lightgrey # fillRule EvenOdd # showOrigin
-- >
-- > textPic1 = text'' "Hello World"
--
-- <<diagrams/src_Graphics_SVGFonts_ReadFont_textPic1.svg#diagram=textPic1&width=300>>
textSVGStretchingWeights :: forall b n. (TypeableFloat n, Renderable (Path V2 n) b) =>
  (String -> String -> n) -> TextOpts n -> String -> QDiagram b V2 n Any
textSVGStretchingWeights stretching_fn topts text =
  case mode topts of
    INSIDE_WH -> makeString (textHeight topts * sumh / maxY) (textHeight topts)
                            (textWidth topts / (textHeight topts * sumh / maxY))
    INSIDE_W  -> makeString (textWidth topts) (textWidth topts * maxY / sumh)   1
    INSIDE_H  -> makeString (textHeight topts * sumh / maxY) (textHeight topts) 1
  where
    makeString w h space =
      ( translate (r2 (-w*space/2,-h/2)) $
        scaleY (h/maxY) $ scaleX (w/sumh) $
        translateY (- bbox_ly fontD) $
        mconcat $
        zipWith translate (horPos space)
        (map polygonChar (zip str (adjusted_hs space)))
      )
        # stroke # withEnvelope ((rect (w*space) h) :: D V2 n)
        # alignBL # translateY (bbox_ly fontD*h/maxY)
    (fontD,outl) = (textFont topts)
    polygonChar (ch,a) = (fromMaybe mempty (Map.lookup ch outl)) <> (underlineChar a)
    underlineChar a | underline topts = translateX (a/2) $ translateY ulinePos (rect a ulineThickness)
                    | otherwise = mempty
    ulinePos = underlinePosition fontD
    ulineThickness = underlineThickness fontD
    horPos space = reverse $ added ( zero : (map (unitX ^*) (adjusted_hs space)) )
    hs = horizontalAdvances str fontD (isKern (spacing topts))

    stretch_weights
      | sum_ == 0 = replicate (length ws + 1) 1
      | otherwise = map (coef*) ws ++ [1]
      where
        ws = zipWith stretching_fn str (tail str)
        sum_ = sum ws
        coef = fromIntegral (length ws) / sum_

    adjusted_hs space = zipWith
      (\adv weight -> adv*(1 - (1 - space)*weight))
      hs stretch_weights -- the last char should not have space to the border
    sumh = sum hs
    added = snd.(foldl (\(h,l) (b,_) -> (h ^+^ b, (h ^+^ b):l))
                       (zero,[])).  (map (\x->(x,[]))) -- [o,o+h0,o+h0+h1,..]
    maxY = bbox_dy fontD -- max height of glyph

    str = characterStrings' fontD text


characterStrings' :: FontData n -> String -> [String]
characterStrings' fontD = \text -> map T.unpack $ characterStrings text ligatures
  where ligatures = filter ((>1) . length) . Map.keys . fontDataGlyphs$ fontD


data Mode = INSIDE_H  -- ^ The string fills the complete height, width adjusted. Used in text editors.
                      -- The result can be smaller or bigger than the bounding box:
                      --
                      --   <<diagrams/src_Graphics_SVGFonts_ReadFont_textH.svg#diagram=textH&width=400>>
          | INSIDE_W  -- ^ The string fills the complete width, height adjusted.
                      -- May be useful for single words in a diagram, or for headlines.
                      -- The result can be smaller or bigger than the bounding box:
                      --
                      -- <<diagrams/src_Graphics_SVGFonts_ReadFont_textW.svg#diagram=textW&width=400>>
          | INSIDE_WH -- ^ The string is stretched inside Width and Height boundaries.
                      -- The horizontal advances are increased if the string is shorter than there is space.
                      -- The horizontal advances are decreased if the string is longer than there is space.
                      -- This feature is experimental and might change in the future.
                      --
                      -- <<diagrams/src_Graphics_SVGFonts_ReadFont_textWH.svg#diagram=textWH&width=400>>
           deriving Show


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
horizontalAdvances (ch0:ch1:s) fd kerning = ((horizontalAdvance ch0 fd) - (ka (fontDataKerning fd))) :
                                            (horizontalAdvances (ch1:s) fd kerning)
  where ka kern | kerning   = (kernAdvance ch0 ch1 kern True) + (kernAdvance ch0 ch1 kern False)
                | otherwise = 0
