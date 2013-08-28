module Graphics.SVGFonts.WriteFont where 

import Data.String
import qualified Data.Set as Set

import Text.Blaze.Svg11 ((!), toValue)
import qualified Text.Blaze.Internal as B
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import Graphics.SVGFonts.ReadFont

makeSvgFont :: (FontData, OutlineMap) -> Set.Set String -> S.Svg
makeSvgFont (fd, om) gs = do
  font ! A.horizAdvX horizAdvX $ do
    S.fontFace ! A.fontFamily fontFamily
               ! A.fontWeight fontWeight
               ! A.fontStretch fontStretch
               ! A.unitsPerEm unitsPerEm
               # maybeString A.panose1 fontDataPanose
               ! A.ascent ascent
               ! A.descent descent
               ! A.xHeight xHeight
               ! A.capHeight capHeight
               ! A.bbox bbox
               ! A.underlineThickness underlineT
               ! A.underlinePosition underlineP
               ! A.unicodeRange unicodeRange
               # maybeMaybe A.stemv fontDataHorizontalStem
               # maybeMaybe A.stemh fontDataVerticalStem
   
  
  where
    (#) :: S.Svg -> Maybe S.Attribute -> S.Svg
    (#) x Nothing = x
    (#) x (Just a) = x ! a
    
    maybeMaybe :: (S.ToValue a) 
               => (S.AttributeValue -> S.Attribute) -> (FontData -> Maybe a) 
               -> Maybe S.Attribute
    maybeMaybe toF fromF = (toF . toValue) `fmap` fromF fd
    
    maybeString :: (S.AttributeValue -> S.Attribute) -> (FontData -> String) 
                -> Maybe S.Attribute
    maybeString toF fromF = case fromF fd of
      "" -> Nothing
      s -> Just $ toF $ toValue $ s
    
    font :: S.Svg -> S.Svg
    font m = B.Parent (fromString "font") (fromString "<font") (fromString "</font>") m
    
    horizAdvX = toValue $ fontDataHorizontalAdvance fd
    fontFamily = toValue $ fontDataFamily fd
    fontWeight = toValue $ fontDataWeight fd
    fontStretch = toValue $ fontDataStretch fd
    unitsPerEm = toValue $ fontDataUnitsPerEm fd 
    ascent = toValue $ fontDataAscent fd
    descent = toValue $ fontDataDescent fd
    xHeight = toValue $ fontDataXHeight fd
    capHeight = toValue $ fontDataCapHeight fd
    bbox = toValue $ fontDataFamily fd
    underlineT = toValue $ fontDataUnderlineThickness fd
    underlineP = toValue $ fontDataUnderlinePos fd
    unicodeRange = toValue $ fontDataUnicodeRange fd
    
    
        {-
          fontDataBoundingBox = parsedBBox
  , fontDataFileName    = fname file
  , fontDataUnderlinePos       = fontface `readAttr` "underline-position"
  , fontDataUnderlineThickness = fontface `readAttr` "underline-thickness"
  , fontDataHorizontalAdvance  = fontHadv
  , fontDataFamily     = fontFamily
  , fontDataWeight     = fontface `readAttr` "font-weight"
  , fontDataStretch    = fontStretch
  , fontDataUnitsPerEm = fontface `readAttr` "units-per-em"
  , fontDataPanose     = panose
  , fontDataAscent     = fontface `readAttr` "ascent"
  , fontDataDescent    = fontface `readAttr` "descent"
  , fontDataXHeight    = fontface `readAttr` "x-height"
  , fontDataCapHeight  = fontface `readAttr` "cap-height"
  , fontDataHorizontalStem = fontface `readAttrM` "stemh"
  , fontDataVerticalStem   = fontface `readAttrM` "stemv"
  , fontDataUnicodeRange = unicodeRange
          
          font-weight="400"
    font-stretch="normal"
    units-per-em="2048"
    panose-1="2 11 6 9 3 8 4 2 2 4"
    ascent="1556"
    descent="-492"
    x-height="1118"
    cap-height="1493"
    bbox="-10 -483 1241 1901"
    underline-thickness="141"
    underline-position="-143"
    unicode-range="U+0020-FB02"
          -}