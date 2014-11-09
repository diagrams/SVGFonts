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

       ) where

import Data.Default.Class
import Diagrams.Prelude hiding (font, text)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Graphics.SVGFonts.Fonts (lin)
import Graphics.SVGFonts.ReadFont
import Graphics.SVGFonts.CharReference (characterStrings)

data TextOpts n = TextOpts
  { font       :: PreparedFont n
  , mode       :: Mode
  , spacing    :: Spacing
  , underline  :: Bool
  , textWidth  :: n
  , textHeight :: n
  }

instance (Read n, RealFloat n) => Default (TextOpts n) where
    def = TextOpts lin INSIDE_H KERN False 1 1

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
textSVG' to text =
  case mode to of
    INSIDE_WH -> makeString (textHeight to * sumh / maxY) (textHeight to) (textWidth to / (textHeight to * sumh / maxY))
    INSIDE_W  -> makeString (textWidth to) (textWidth to * maxY / sumh)   1 -- the third character is used to scale horizontal advances
    INSIDE_H  -> makeString (textHeight to * sumh / maxY) (textHeight to) 1
  where
    makeString w h space = (scaleY (h/maxY) $ scaleX (w/sumh) $
                            mconcat $
                            zipWith translate (horPos space)
                           (map polygonChar (zip str (adjusted_hs space))) ) # centerXY
    (fontD,outl) = font to
    polygonChar (ch,a) = (fromMaybe mempty (Map.lookup ch outl)) <> (underlineChar a)
    underlineChar a | underline to = translateY ulinePos (rect a ulineThickness)
                    | otherwise = mempty
    ulinePos = underlinePosition fontD
    ulineThickness = underlineThickness fontD
    horPos space = reverse $ added ( zero : (map (unitX ^*) (adjusted_hs space)) )
    adjusted_hs space = map (*space) hs
    hs = horizontalAdvances str fontD (isKern (spacing to))
    sumh = sum hs
    added = snd.(foldl (\(h,l) (b,_) -> (h ^+^ b, (h ^+^ b):l))
                       (zero,[])).  (map (\x->(x,[]))) -- [o,o+h0,o+h0+h1,..]
    maxY = bbox_dy fontD -- max height of glyph

    ligatures = ((filter ((>1) . length)) . Map.keys . fontDataGlyphs) fontD
    str = map T.unpack $ characterStrings text ligatures


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
textSVG_ to text =
  case mode to of
    INSIDE_WH -> makeString (textHeight to * sumh / maxY) (textHeight to) ((textWidth to) / (textHeight to * sumh / maxY))
    INSIDE_W  -> makeString (textWidth to) (textWidth to * maxY / sumh)   1
    INSIDE_H  -> makeString (textHeight to * sumh / maxY) (textHeight to) 1
  where
    makeString w h space =( ( translate (r2 (-w*space/2,-h/2)) $
                            scaleY (h/maxY) $ scaleX (w/sumh) $
                            translateY (- bbox_ly fontD) $
                            mconcat $
                            zipWith translate (horPos space)
                            (map polygonChar (zip str (adjusted_hs space))) ) # stroke # withEnvelope ((rect (w*space) h) :: D V2 n)
                          ) # alignBL # translateY (bbox_ly fontD*h/maxY)
    (fontD,outl) = (font to)
    polygonChar (ch,a) = (fromMaybe mempty (Map.lookup ch outl)) <> (underlineChar a)
    underlineChar a | underline to = translateX (a/2) $ translateY ulinePos (rect a ulineThickness)
                    | otherwise = mempty
    ulinePos = underlinePosition fontD
    ulineThickness = underlineThickness fontD
    horPos space = reverse $ added ( zero : (map (unitX ^*) (adjusted_hs space)) )
    hs = horizontalAdvances str fontD (isKern (spacing to))
    adjusted_hs space = map (*space) hs -- the last char should not have space to the border
    sumh = sum hs
    added = snd.(foldl (\(h,l) (b,_) -> (h ^+^ b, (h ^+^ b):l))
                       (zero,[])).  (map (\x->(x,[]))) -- [o,o+h0,o+h0+h1,..]
    maxY = bbox_dy fontD -- max height of glyph

    ligatures = (filter ((>1) . length) . Map.keys . fontDataGlyphs) fontD
    str = map T.unpack $ characterStrings text ligatures


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
