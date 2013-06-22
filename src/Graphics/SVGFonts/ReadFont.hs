{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Graphics.SVGFonts.ReadFont
-- (openFont, outlMap, textSVG, textSVG_, Mode(..), Spacing(..), Kern, SvgGlyph, FontData, OutlineMap, TextOpts(..))
where

import Data.Char (isSpace)
import Data.Default
import Data.List (intersect,sortBy)
import Data.List.Split (splitOn, splitWhen)
import Data.Maybe (fromMaybe, fromJust, isJust, isNothing, maybeToList, catMaybes)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Tuple.Select
import Data.Vector (Vector)
import Data.VectorSpace
import Diagrams.Path
import Diagrams.Segment
import Diagrams.TwoD.Types
import Diagrams.Prelude
import qualified Data.Vector as V
import Graphics.SVGFonts.CharReference (charsFromFullName, characterStrings)
import Graphics.SVGFonts.ReadPath (pathFromString, PathCommand(..))
import Paths_SVGFonts(getDataFileName)
import System.IO.Unsafe (unsafePerformIO)
import Text.XML.Light

instance Default TextOpts where
    def = TextOpts "text" lin INSIDE_H KERN False 1 1

-- | A short version of textSVG' with standard values. The Double value is the height.
--
-- @
-- \{-\# LANGUAGE NoMonomorphismRestriction \#-\}
-- 
--import Diagrams.Prelude
--import Diagrams.Backend.SVG.CmdLine
--import Graphics.SVGFonts.ReadFont
-- 
--main = defaultMain (textSVG \"Hello World\" 1)
-- @
--
textSVG :: String -> Double -> Path R2
textSVG t h = textSVG' with { txt = t, textHeight = h }

data TextOpts = TextOpts
                { txt :: String
                , fdo :: (FontData, OutlineMap)
                , mode :: Mode
                , spacing :: Spacing
                , underline :: Bool
                , textWidth :: Double
                , textHeight :: Double
                }

-- > import Graphics.SVGFonts.ReadFont
-- > text' t = stroke (textSVG' $ TextOpts t lin INSIDE_H KERN False 1 1 )
-- >            # fc blue # lc blue # bg lightgrey # fillRule EvenOdd # showOrigin
-- > textPic0 = (text' "Hello World") # showOrigin

-- | The origin is at the center of the text and the boundaries are given by the outlines of the chars.
--
-- @
-- \{-\# LANGUAGE NoMonomorphismRestriction \#-\}
-- 
--import Diagrams.Prelude
--import Diagrams.Backend.SVG.CmdLine
--import Graphics.SVGFonts.ReadFont
-- 
--main = defaultMain (text' \"Hello World\")
--text' t = stroke (textSVG' $ TextOpts t lin INSIDE_H KERN False 1 1 )
--          \# fc blue \# lc blue \# bg lightgrey \# fillRule EvenOdd \# showOrigin
-- @
--
--   <<diagrams/textPic0.svg#diagram=textPic0&width=300>>
textSVG' :: TextOpts -> Path R2
textSVG' to =
  case mode to of
    INSIDE_WH -> makeString (textHeight to * sumh / maxY) (textHeight to) ((textWidth to) / (textHeight to * sumh / maxY))
    INSIDE_W  -> makeString (textWidth to) (textWidth to * maxY / sumh)   1 -- the third character is used to scale horizontal advances
    INSIDE_H  -> makeString (textHeight to * sumh / maxY) (textHeight to) 1
  where
    makeString w h space = (scaleY (h/maxY) $ scaleX (w/sumh) $
                            mconcat $
                            zipWith translate (horPos space)
                           (map polygonChar (zip str (adjusted_hs space))) ) # centerXY
    (fontD,outl) = (fdo to)
    polygonChar (ch,a) = (fromMaybe mempty (Map.lookup ch outl)) <> (underlineChar a)
    underlineChar a | underline to = translateY ulinePos (rect a ulineThickness)
                    | otherwise = mempty
    ulinePos = underlinePosition fontD
    ulineThickness = underlineThickness fontD
    horPos space = reverse $ added ( zeroV : (map (unitX ^*) (adjusted_hs space)) )
    adjusted_hs space = map (*space) hs
    hs = horizontalAdvances str fontD (isKern (spacing to))
    sumh = sum hs
    added = snd.(foldl (\(h,l) (b,_) -> (h ^+^ b, (h ^+^ b):l))
                       (zeroV,[])).  (map (\x->(x,[]))) -- [o,o+h0,o+h0+h1,..]
    maxY = bbox_dy fontD -- max height of glyph

    ligatures = ((filter ((>1).length)).(Map.keys).sel1) fontD
    str = map T.unpack $ characterStrings (txt to) ligatures

-- > import Graphics.SVGFonts.ReadFont
-- > text'' t = (textSVG_ $ TextOpts t lin INSIDE_H KERN True 1 1 )
-- >            # fc blue # lc blue # bg lightgrey # fillRule EvenOdd # showOrigin
-- > textPic1 = text'' "Hello World"

-- | The origin is at the left end of the baseline of of the text and the boundaries 
-- are given by the bounding box of the Font. This is best for combining Text of different
-- fonts and for several lines of text.
-- As you can see you can also underline text by setting underline to True.
--
-- @
-- \{-\# LANGUAGE NoMonomorphismRestriction \#-\}
-- 
--import Diagrams.Prelude
--import Diagrams.Backend.SVG.CmdLine
--import Graphics.SVGFonts.ReadFont
-- 
--main = defaultMain (text'' \"Hello World\")
--
--text'' t = (textSVG_ $ TextOpts t lin INSIDE_H KERN True 1 1 )
--           \# fc blue \# lc blue \# bg lightgrey \# fillRule EvenOdd \# showOrigin
-- @
--
--   <<diagrams/textPic1.svg#diagram=textPic1&width=300>>
textSVG_ :: forall b . Renderable (Path R2) b => TextOpts -> QDiagram b R2 Any
textSVG_ to =
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
                            (map polygonChar (zip str (adjusted_hs space))) ) # stroke # withEnvelope ((rect (w*space) h) :: D R2)
                          ) # alignBL # translateY (bbox_ly fontD*h/maxY)
    (fontD,outl) = (fdo to)
    polygonChar (ch,a) = (fromMaybe mempty (Map.lookup ch outl)) <> (underlineChar a)
    underlineChar a | underline to = translateX (a/2) $ translateY ulinePos (rect a ulineThickness)
                    | otherwise = mempty
    ulinePos = underlinePosition fontD
    ulineThickness = underlineThickness fontD
    horPos space = reverse $ added ( zeroV : (map (unitX ^*) (adjusted_hs space)) )
    hs = horizontalAdvances str fontD (isKern (spacing to))
    adjusted_hs space = map (*space) hs -- the last char should not have space to the border
    sumh = sum hs
    added = snd.(foldl (\(h,l) (b,_) -> (h ^+^ b, (h ^+^ b):l))
                       (zeroV,[])).  (map (\x->(x,[]))) -- [o,o+h0,o+h0+h1,..]
    maxY = bbox_dy fontD -- max height of glyph

    ligatures = ((filter ((>1).length)).(Map.keys).sel1) fontD
    str = map T.unpack $ characterStrings (txt to) ligatures

-- | This type contains everything that a typical SVG font file produced by fontforge contains.
--
-- (SvgGlyph, Kern, bbox-string, filename, (underlinePos, underlineThickness), 
--   (fontHadv, fontFamily, fontWeight, fontStretch, unitsPerEm, panose, ascent, descent, xHeight, capHeight, stemh, stemv, unicodeRange) )
--
type FontData = (SvgGlyph, Kern, [Double], String, (Double, Double),
                (Double,String,String,String,String,String,String,String,String,String,String,String,String))

-- | Open an SVG-Font File and extract the data
--
openFont :: FilePath -> FontData
openFont file = ( Map.fromList glyphs,
                  (transformChars u1s, transformChars u2s, transformChars g1s, transformChars g2s, kAr), -- kerning data
                  parsedBBox,
                  fname file,
                  (underlinePos, underlineThick),
                  (fontHadv, fontFamily, fontWeight, fontStretch, unitsPerEm, panose,
                   ascent, descent, xHeight, capHeight, stemh, stemv, unicodeRange)
                )
  where
    xml = onlyElems $ parseXML $ unsafePerformIO $ readFile file

    fontElement = head $ catMaybes $ map (findElement (unqual "font")) xml
    fontHadv = fromMaybe ((parsedBBox!!2) - (parsedBBox!!0)) -- BBox is used if there is no "horiz-adv-x" attribute
                         (fmap read (findAttr (unqual "horiz-adv-x") fontElement) )
    fontface = fromJust $ findElement (unqual "font-face") fontElement -- there is always a font-face node
    bbox     = fromMaybe "" $ findAttr (unqual "bbox") fontface
    parsedBBox :: [Double]
    parsedBBox = map read $ splitWhen isSpace bbox
    underlineThick = read $ fromMaybe "" $ findAttr (unqual "underline-thickness") fontface
    underlinePos   = read $ fromMaybe "" $ findAttr (unqual "underline-position") fontface
    fontFamily   = read $ fromMaybe "" $ findAttr (unqual "font-family") fontface
    fontWeight   = read $ fromMaybe "" $ findAttr (unqual "font-weight") fontface
    fontStretch   = read $ fromMaybe "" $ findAttr (unqual "font-stretch") fontface
    unitsPerEm   = read $ fromMaybe "" $ findAttr (unqual "units-per-em") fontface
    panose     = read $ fromMaybe "" $ findAttr (unqual "panose") fontface
    ascent    = read $ fromMaybe "" $ findAttr (unqual "ascent") fontface
    descent   = read $ fromMaybe "" $ findAttr (unqual "descent") fontface
    xHeight   = read $ fromMaybe "" $ findAttr (unqual "xHeight") fontface
    capHeight = read $ fromMaybe "" $ findAttr (unqual "capHeight") fontface
    stemh = read $ fromMaybe "" $ findAttr (unqual "stemh") fontface
    stemv = read $ fromMaybe "" $ findAttr (unqual "stemv") fontface
    unicodeRange = read $ fromMaybe "" $ findAttr (unqual "unicode-range") fontface

    glyphElements = findChildren (unqual "glyph") fontElement
    kernings = findChildren (unqual "hkern") fontElement
    glyphs = map glyphsWithDefaults glyphElements

    -- monospaced fonts sometimes don't have a "horiz-adv-x="-value , replace with "horiz-adv-x=" in <font>
    glyphsWithDefaults g = (charsFromFullName $ fromMaybe gname (findAttr (unqual "unicode") g), -- there is always a name or unicode
                             (
                               gname,
                               fromMaybe fontHadv (fmap read (findAttr (unqual "horiz-adv-x") g)),
                               fromMaybe "" (findAttr (unqual "d") g)
                             )
                           )
      where gname = fromMaybe "" (findAttr (unqual "glyph-name") g)

    u1s         = map (fromMaybe "") $ map (findAttr (unqual "u1"))  kernings
    u2s         = map (fromMaybe "") $ map (findAttr (unqual "u2"))  kernings
    g1s         = map (fromMaybe "") $ map (findAttr (unqual "g1"))  kernings
    g2s         = map (fromMaybe "") $ map (findAttr (unqual "g2"))  kernings
    ks          = map (fromMaybe "") $ map (findAttr (unqual "k"))   kernings
    kAr     = V.fromList (map read ks)

    transformChars chars = Map.fromList $ map ch $ multiSet $ 
                                          map (\(x,y) -> (x,[y])) $ sort fst $ concat $ index chars
    ch (x,y) | null x = ("",y)
             | otherwise = (x,y)

    index u = addIndex (map (splitWhen isColon) u) -- ie ["aa,b","c,d"] to [["aa","b"],["c","d"]]
    isColon = (== ',')                             -- to [("aa",0),("b",0)],[("c",1), ("d",1)]

    addIndex qs = zipWith (\x y -> (map (\z -> (z,x)) y)) [0..] qs
    sort f xs = sortBy (\x y -> compare (f x) (f y) ) xs

    multiSet [] = []
    multiSet (a:[]) = [a] -- example: [("n1",[0]),("n1",[1]),("n2",[1])] to [("n1",[0,1]),("n2",[1])]
    multiSet (a:b:bs) | fst a == fst b = multiSet ( (fst a, (snd a) ++ (snd b)) : bs)
                      | otherwise = a : (multiSet (b:bs))

    fname f = last $ init $ concat (map (splitOn "/") (splitOn "." f))


type SvgGlyph = Map.Map String (String, Double, String) -- ^ \[ (unicode, (glyph_name, horiz_advance, ds)) \]

-- | Horizontal advances of characters inside a string.
-- A character is stored with a string (because of ligatures like \"ffi\").
horizontalAdvances :: [String] -> FontData -> Bool -> [Double]
horizontalAdvances []          _  _       = []
horizontalAdvances [ch]        fd _       = [hadv ch fd]
horizontalAdvances (ch0:ch1:s) fd kerning = ((hadv ch0 fd) - (ka (sel2 fd))) :
                                            (horizontalAdvances (ch1:s) fd kerning)
  where ka kern | kerning   = (kernAdvance ch0 ch1 kern True) + (kernAdvance ch0 ch1 kern False)
                | otherwise = 0

-- | Horizontal advance of a character consisting of its width and spacing, extracted out of the font data
hadv :: String -> FontData -> Double
hadv ch fontD | isJust char = sel2 (fromJust char)
              | otherwise   = sel1 (sel6 fontD)
  where char = (Map.lookup ch (sel1 fontD))

-- | See <http://www.w3.org/TR/SVG/fonts.html#KernElements>
--
-- Some explanation how kerning is computed:
--
-- In Linlibertine.svg, there are two groups of chars: e.g.
-- \<hkern g1=\"f,longs,uni1E1F,f_f\" g2=\"parenright,bracketright,braceright\" k=\"-37\" />
-- This line means: If there is an f followed by parentright, reduce the horizontal advance by -37 (add 37).
-- Therefore to quickly check if two characters need kerning assign an index to the second group (g2 or u2)
-- and assign to every unicode in the first group (g1 or u1) this index, then sort these tuples after their
-- name (for binary search). Because the same unicode char can appear in several g1s, reduce this 'multiset',
-- ie all the (\"name1\",0) (\"name1\",1) to (\"name1\",[0,1]).
-- Now the g2s are converted in the same way as the g1s.
-- Whenever two consecutive chars are being printed try to find an
-- intersection of the list assigned to the first char and second char
type Kern = ( Map.Map String [Int],
              Map.Map String [Int],
              Map.Map String [Int],
              Map.Map String [Int], Vector Double ) -- ^ u1s, u2s, g1s, g2s, k

-- | Change the horizontal advance of two consective chars (kerning)
kernAdvance :: String -> String -> Kern -> Bool -> Double
kernAdvance ch0 ch1 kern u |     u && not (null s0) = (sel5 kern) V.! (head s0)
                           | not u && not (null s1) = (sel5 kern) V.! (head s1)
                           | otherwise = 0
  where s0 = intersect (s sel1 ch0) (s sel2 ch1)
        s1 = intersect (s sel3 ch0) (s sel4 ch1)
        s sel ch = concat (maybeToList (Map.lookup ch (sel kern)))

type OutlineMap =  Map.Map String (Path R2)

-- > import Graphics.SVGFonts.ReadFont
-- > textWH0 = (rect 8 1) # alignBL <> ((textSVG_ $ TextOpts "SPACES" lin INSIDE_WH KERN False 8 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd) # alignBL
-- > textWH1 = (rect 8 1) # alignBL <> ((textSVG_ $ TextOpts "are sometimes better." lin INSIDE_WH KERN False 8 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd) # alignBL
-- > textWH2 = (rect 8 1) # alignBL <> ((textSVG_ $ TextOpts "But too many chars are not good." lin INSIDE_WH KERN False 8 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd) # alignBL
-- > textWH = textWH0 # alignBL === strutY 0.3 === textWH1 === strutY 0.3 === textWH2 # alignBL
-- > textW0 = (rect 3 1) # alignBL <> ( (textSVG_ $ TextOpts "HEADLINE" lin INSIDE_W KERN False 3 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd ) # alignBL
-- > textW1 = (rect 10 1) # alignBL <> ( (textSVG_ $ TextOpts "HEADLINE" lin INSIDE_W KERN False 10 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd ) # alignBL
-- > textW = textW0 # alignBL ||| strutX 1 ||| textW1 # alignBL
-- > textH0 = (rect 10 1) # alignBL <> ((textSVG_ $ TextOpts "Constant font size" lin INSIDE_H KERN False 10 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd) # alignBL
-- > textH1 = (rect 3 1) # alignBL <> ((textSVG_ $ TextOpts "Constant font size" lin INSIDE_H KERN False 3 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd) # alignBL
-- > textH = textH0 # alignBL === strutY 0.5 === textH1 # alignBL

data Mode = INSIDE_H  -- ^ The string fills the complete height, width adjusted. Used in text editors.
                      -- The result can be smaller or bigger than the bounding box:
                      --
                      --   <<diagrams/textH.svg#diagram=textH&width=400>>
          | INSIDE_W  -- ^ The string fills the complete width, heigth adjusted.
                      -- Maybe useful for single words in a diagram or headlines.
                      -- The result can be smaller or bigger than the bounding box:
                      --
                      -- <<diagrams/textW.svg#diagram=textW&width=400>>
          | INSIDE_WH -- ^ The string is stretched inside Width and Height boundaries.
                      -- The horizontal advances are increased if the string is shorter than there is space.
                      -- The horizontal advances are decreased if the string is longer than there is space.
                      -- This feature is experimental and might change in the future.
                      --
                      -- <<diagrams/textWH.svg#diagram=textWH&width=400>>

mWH :: Mode -> Bool
mWH INSIDE_WH = True
mWH _ = False
mW :: Mode -> Bool
mW INSIDE_W = True
mW _ = False
mH :: Mode -> Bool
mH INSIDE_H = True
mH _ = False

-- > import Graphics.SVGFonts.ReadFont
-- > textHADV = (textSVG_ $ TextOpts "AVENGERS" lin INSIDE_H HADV False 10 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd

-- > import Graphics.SVGFonts.ReadFont
-- > textKern = (textSVG_ $ TextOpts "AVENGERS" lin INSIDE_H KERN False 10 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd

-- | See <http://en.wikipedia.org/wiki/Kerning>
--
data Spacing = HADV -- ^ Every glyph has a unique horiz. advance
                    --
                    --  <<diagrams/textHADV.svg#diagram=textHADV&width=400>>
             | KERN -- ^ Recommended, same as HADV but sometimes overridden by kerning:
                    -- As You can see there is less space between \"A\" and \"V\":
                    --
                    --   <<diagrams/textKern.svg#diagram=textKern&width=400>>

isKern :: Spacing -> Bool
isKern KERN = True
isKern _    = False

type FileName = String

-- | read only of static data (safe)
ro :: FilePath -> FilePath
ro = unsafePerformIO . getDataFileName

-- | Difference between highest and lowest y-value of bounding box
bbox_dy :: FontData -> Double
bbox_dy fontData = (bbox!!3) - (bbox!!1)
  where bbox = sel3 fontData -- bbox = [lowest x, lowest y, highest x, highest y]

-- | Lowest x-value of bounding box
bbox_lx :: FontData -> Double
bbox_lx fontData   = (sel3 fontData) !! 0

-- | Lowest y-value of bounding box
bbox_ly :: FontData -> Double
bbox_ly fontData   = (sel3 fontData) !! 1

-- | Position of the underline bar
underlinePosition :: FontData -> Double
underlinePosition fontData = fst $ sel5 fontData

-- | Thickness of the underline bar
underlineThickness :: FontData -> Double
underlineThickness fontData = snd $ sel5 fontData

-- | Generate Font Data and a Map from chars to outline paths
outlMap :: String -> (FontData, OutlineMap)
outlMap str = ( fontD, Map.fromList [ (ch, outlines ch) | ch <- allUnicodes ] )
  where
  allUnicodes = Map.keys (sel1 fontD)
  outlines ch = mconcat $ commandsToTrails (commands ch (sel1 fontD)) [] zeroV zeroV zeroV
  fontD = openFont str

commandsToTrails :: [PathCommand] -> [Segment Closed R2] -> R2 -> R2 -> R2 -> [Path R2]
commandsToTrails [] _ _ _ _ = []
commandsToTrails (c:cs) segments l lastContr beginPoint -- l is the endpoint of the last segment
      | isNothing nextSegment = (translate beginPoint (pathFromTrail . wrapTrail  . closeLine $ lineFromSegments segments)) :
                  ( commandsToTrails cs [] (l ^+^ offs) (contr c) (beginP c) ) -- one outline completed
      | otherwise = commandsToTrails cs (segments ++ [fromJust nextSegment])
                                           (l ^+^ offs) (contr c) (beginP c)   -- work on outline
  where nextSegment = go c
        offs | isJust nextSegment 
               = segOffset (fromJust nextSegment)
             | otherwise = zeroV
        (x0,y0) = unr2 offs
        (cx,cy) = unr2 lastContr -- last control point is always in absolute coordinates
        beginP ( M_abs (x,y) ) = r2 (x,y)
        beginP ( M_rel (x,y) ) = l ^+^ r2 (x,y)
        beginP _ = beginPoint
        contr ( C_abs (_x1,_y1,x2,y2,x,y) ) = r2 (x0+x-x2, y0+y-y2 ) -- control point of bezier curve
        contr ( C_rel (_x1,_y1,x2,y2,x,y) ) = r2 (   x-x2,    y-y2 )
        contr ( S_abs (x2,y2,x,y) )         = r2 (x0+x-x2, y0+y-y2 )
        contr ( S_rel (x2,y2,x,y) )         = r2 (   x-x2,    y-y2 )
        contr ( Q_abs (x1,y1,x,y) ) = r2 (x0+x-x1, y0+y-y1 )
        contr ( Q_rel (x1,y1,x,y) ) = r2 (   x-x1,    y-y1 )
        contr ( T_abs (_x,_y) )     = r2 (2*x0-cx, 2*y0-cy )
        contr ( T_rel (x,y) )       = r2 (   x-cx,    y-cy )
        contr ( L_abs (_x,_y) ) = r2 (x0, y0)
        contr ( L_rel (_x,_y) ) = r2 ( 0,  0)
        contr ( M_abs (_x,_y) ) = r2 (x0, y0)
        contr ( M_rel (_x,_y) ) = r2 ( 0,  0)
        contr ( H_abs _x ) = r2 (x0, y0)
        contr ( H_rel _x ) = r2 ( 0, y0)
        contr ( V_abs _y ) = r2 (x0, y0)
        contr ( V_rel _y ) = r2 (x0,  0)
        contr ( Z ) = r2 (0, 0) -- to get rid of warnings
        contr ( A_abs ) = r2 (0, 0) -- to get rid of warnings
        contr ( A_rel ) = r2 (0, 0) -- to get rid of warnings

        straight' = straight . r2
        bezier3' point1 point2 point3 = bezier3 (r2 point1) (r2 point2) (r2 point3)

        go ( M_abs (_x,_y) ) = Nothing
        go ( M_rel (_x,_y) ) = Nothing
        go ( L_abs (x,y) ) = Just $ straight' (x0+x, y0+y)
        go ( L_rel (x,y) ) = Just $ straight' (x, y)
        go ( H_abs x) = Just $ straight' (x0 + x, y0)
        go ( H_rel x) = Just $ straight' (x, 0)
        go ( V_abs y) = Just $ straight' (x0, y0 + y)
        go ( V_rel y) = Just $ straight' (0, y)
        go ( C_abs (x1,y1,x2,y2,x,y) ) = Just $ bezier3' (x0+x1, y0+y1) (x0+x2,y0+y2) (x0+x,y0+y)
        go ( C_rel (x1,y1,x2,y2,x,y) ) = Just $ bezier3' (x1, y1) (x2, y2) (x, y)
        go ( S_abs (      x2,y2,x,y) ) = Just $ bezier3' (cx, cy) (x0+x2, y0+y2) (x0+x, y0+y)
        go ( S_rel (      x2,y2,x,y) ) = Just $ bezier3' (cx, cy) (x2, y2) (x, y)
        go ( Q_abs (x1,y1,x,y) ) = Just $ bezier3' (x0 + x1, y0 + y1) (x0 + x, y0 + y) (x0 + x, y0 + y)
        go ( Q_rel (x1,y1,x,y) ) = Just $ bezier3' (x1, y1) (x, y) (x, y)
        go ( T_abs (x,y) ) = Just $ bezier3' (cx, cy) (x0 + x, y0 + y) (x0 + x, y0 + y)
        go ( T_rel (x,y) ) = Just $ bezier3' (cx, cy) (x, y) (x, y)
        go ( Z ) = Nothing
        go ( A_abs ) = Nothing
        go ( A_rel ) = Nothing

commands :: String -> SvgGlyph -> [PathCommand]
commands ch glyph | isJust element = unsafePerformIO $ pathFromString $ sel3 $ fromJust element
                  | otherwise      = []
  where element = Map.lookup ch glyph

-- | Bitstream, a standard monospaced font (used in gedit)
bit :: (FontData, OutlineMap)
bit = outlMap (ro "fonts/Bitstream.svg")

-- | Linux Libertine, for non-monospaced text: <http://www.linuxlibertine.org/>, contains a lot of unicode characters
lin :: (FontData, OutlineMap)
lin = outlMap (ro "fonts/LinLibertine.svg")

-- | Linux Libertine, cut to the most common Characters, smaller file, quicker load time
lin2 :: (FontData, OutlineMap)
lin2 = outlMap (ro "fonts/LinLibertineCut.svg")

-- | SourceCode Pro
-- sourcePro :: (FontData, OutlineMap)
-- sourcePro = outlMap (ro "fonts/SourceCodePro-Regular.svg")
