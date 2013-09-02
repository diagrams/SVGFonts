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
               ! A.fontStyle fontStyle
               ! A.fontWeight fontWeight
               ! A.fontStretch fontStretch
               ! A.fontVariant fontVariant
               # maybeMaybe A.fontSize fontDataSize
               ! A.unitsPerEm unitsPerEm
               # maybeString A.panose1 fontDataPanose
               # maybeMaybe A.slope fontDataSlope
               ! A.ascent ascent
               ! A.descent descent
               ! A.xHeight xHeight
               ! A.capHeight capHeight
               # maybeMaybe A.accentHeight fontDataAccentHeight
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
    forM_ (Set.toList gs') $ \g -> case M.lookup g (fontDataGlyphs fd) of
      Nothing -> return ()
      Just (gName, gHAdv, gPath) -> do
        S.glyph ! A.glyphName (toValue gName)
                ! A.horizAdvX (toValue gHAdv)
                ! A.d (toValue gPath) 
                # maybeUnicode g
                $ return ()
    let
    forM_ (fontDataRawKernings fd) $ \(k, g1, g2, u1, u2) -> do
      let g1' = filter isGlyph g1
          g2' = filter isGlyph g2
          u1' = filter isGlyph u1
          u2' = filter isGlyph u2
      case (not (null g1') && not (null g2')) || (not (null u1') && not (null u2')) of
        True -> do
          S.hkern ! A.k (toValue k)
                  # maybeString A.g1 (const $ intercalate "," g1')
                  # maybeString A.g2 (const $ intercalate "," g2')
                  # maybeString A.u1 (const $ intercalate "," u1')
                  # maybeString A.u2 (const $ intercalate "," u2')
        False -> return ()

  
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
    
    isGlyph :: String -> Bool
    isGlyph g = g `Set.member` gs'
    
    gs' = Set.insert ".notdef" gs
    
    horizAdvX = toValue $ fontDataHorizontalAdvance fd
    fontFamily = toValue $ fontDataFamily fd
    fontStyle = toValue $ fontDataStyle fd
    fontWeight = toValue $ fontDataWeight fd
    fontStretch = toValue $ fontDataStretch fd
    fontVariant = toValue $ fontDataVariant fd
    unitsPerEm = toValue $ fontDataUnitsPerEm fd 
    ascent = toValue $ fontDataAscent fd
    descent = toValue $ fontDataDescent fd
    xHeight = toValue $ fontDataXHeight fd
    capHeight = toValue $ fontDataCapHeight fd
    bbox = toValue $ intercalate " " $ fmap show $ fontDataBoundingBox fd
    underlineT = toValue $ fontDataUnderlineThickness fd
    underlineP = toValue $ fontDataUnderlinePos fd
    unicodeRange = toValue $ fontDataUnicodeRange fd
    