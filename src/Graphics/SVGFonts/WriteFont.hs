module Graphics.SVGFonts.WriteFont where 

import Numeric ( showHex )

import Data.String ( fromString )
import Data.Char ( ord )
import Data.List ( intercalate )
import qualified Data.Set as Set
import qualified Data.Map as M

import Control.Monad ( forM_ )

import Text.Blaze.Svg11 ((!), toValue)
import qualified Text.Blaze.Internal as B
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import Graphics.SVGFonts.ReadFont

makeSvgFont :: (FontData, OutlineMap) -> Set.Set String -> S.Svg
makeSvgFont (fd, _) gs = do
  font ! A.horizAdvX horizAdvX $ do
    -- Font meta information
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
    -- Insert the 'missing-glyph'
    case M.lookup ".notdef" (fontDataGlyphs fd) of
      Nothing -> return ()
      Just (_, _, gPath) -> S.missingGlyph ! A.d (toValue gPath) 
                                           $ return ()
    -- Insert all other glyphs
    let gs' = Set.insert ".notdef" gs
    forM_ (Set.toList gs') $ \g -> case M.lookup g (fontDataGlyphs fd) of
      Nothing -> return ()
      Just (gName, gHAdv, gPath) -> do
        S.glyph ! A.glyphName (toValue gName)
                ! A.horizAdvX (toValue gHAdv)
                ! A.d (toValue gPath) 
                # maybeUnicode g
                $ return ()

  
  where
    (#) :: (B.Attributable h) => h -> Maybe S.Attribute -> h
    (#) x Nothing = x
    (#) x (Just a) = x ! a
    
    unicodeBlacklist :: Set.Set String
    unicodeBlacklist = Set.fromList 
      [ ".notdef"
      , ".null"
      ]
    
    maybeUnicode :: String -> Maybe S.Attribute
    maybeUnicode [] = Nothing
    maybeUnicode s | s `Set.member` unicodeBlacklist || length s >= 10 = Nothing
    maybeUnicode s = Just $ A.unicode $ toValue $ concatMap encodeUnicode s
    
    encodeUnicode :: Char -> String
    encodeUnicode c = 
      let cOrd = ord c
      in if cOrd >= 32 && cOrd <= 126 
            then [c] 
            else "&#x" ++ (showHex cOrd "")
    
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
    bbox = toValue $ intercalate " " $ fmap show $ fontDataBoundingBox fd
    underlineT = toValue $ fontDataUnderlineThickness fd
    underlineP = toValue $ fontDataUnderlinePos fd
    unicodeRange = toValue $ fontDataUnicodeRange fd
    