module Graphics.SVGFonts.ReadFont
-- (openFont, outlMap, textSVG, textSVG_, Mode(..), Spacing(..), Kern, SvgGlyph, FontData, OutlineMap, TextOpts(..))
where

import Data.Char (isSpace)
import Data.Default
import Data.List (intersect,sortBy)
import Data.List.Split (splitOn, splitWhen)
import Data.Maybe (fromMaybe, fromJust, isJust, isNothing, maybeToList)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Monoid (mconcat)
import Data.Tuple.Select (sel1, sel2, sel3, sel4, sel5)
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
import System.Directory
import System.IO.Unsafe (unsafePerformIO)
import Text.XML.Light

-- | See <http://www.w3.org/TR/SVG/fonts.html#KernElements>
--
-- Some explanation how kerning is computed:
--
-- In Linlibertine.svg, there are two groups of chars: i.e.
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

type SvgGlyph = Map.Map String (String, Double, String) -- ^ \[ (unicode, (glyph_name, horiz_advance, ds)) \]
type FontData = (SvgGlyph, Kern, [Double], String) -- ^ (SvgGlyph, Kern, bbox-string, filename)

-- | Open an SVG-Font File and extract the data
--
openFont :: FilePath -> FontData
openFont file = ( Map.fromList (myZip4 (unicodes, glyphNames, horiz, ds)),  -- Map with unicode keys
                  (transform u1s, transform u2s, transform g1s, transform g2s, kAr), -- kerning data
                  parsedBBox,
                  fname file
                )
  where
    -- monospaced fonts sometimes don't have a "horiz-adv-x="-value , replace with bbox value
    myZip4 (a:as, b:bs, c:cs, d:ds) | c == []   = (a, (b, (parsedBBox!!2) - (parsedBBox!!0), d)) :
                                                  (myZip4 (as,bs,cs,ds))
                                    | otherwise = (a, (b, read c, d)) : (myZip4 (as,bs,cs,ds))
    myZip4 _ = []
    xml = onlyElems $ parseXML $ unsafePerformIO $ readFile $ file
    selectFontface = concat $ map (findElements (unqual "font-face")) xml
    selectGlyphs   = concat $ map (findElements (unqual "glyph"))     xml
    selectKerns    = concat $ map (findElements (unqual "hkern"))     xml
    bbox        = fromMaybe "" $ head $ map (findAttr (unqual "bbox")) selectFontface
    parsedBBox = map read $ splitWhen isSpace bbox
    glyphNames  = map (fromMaybe "") $ map (findAttr (unqual "glyph-name")) selectGlyphs
    unicodes    = map charsFromFullName  $ map (findAttr (unqual "unicode")) selectGlyphs
    horiz       = map (fromMaybe "") $ map (findAttr (unqual "horiz-adv-x")) selectGlyphs
    ds          = map (fromMaybe "") $ map (findAttr (unqual "d"))           selectGlyphs
    u1s         = map (fromMaybe "") $ map (findAttr (unqual "u1"))  selectKerns
    u2s         = map (fromMaybe "") $ map (findAttr (unqual "u2"))  selectKerns
    g1s         = map (fromMaybe "") $ map (findAttr (unqual "g1"))  selectKerns
    g2s         = map (fromMaybe "") $ map (findAttr (unqual "g2"))  selectKerns
    ks          = map (fromMaybe "") $ map (findAttr (unqual "k"))   selectKerns
    kAr     = V.fromList (map read ks)

    transform chars = Map.fromList $ map ch $ multiSet $ map (\(x,y) -> (x,[y])) $ sort fst $ concat $ index chars
    ch (x,y) | null x = ("",y)
             | otherwise = (x,y)

    index u = addIndex (map (splitWhen isColon) u) -- ie ["aa,b","c,d"] to [["aa","b"],["c","d"]]
    isColon = (== ',')                             -- to [("aa",0),("b",0)],[("c",1), ("d",1)]

    addIndex qs = zipWith (\x y -> (map (f x) y)) [0..] qs
    f = \index char -> (char,index)
    sort f xs = sortBy (\x y -> compare (f x) (f y) ) xs

    multiSet [] = []
    multiSet (a:[]) = [a] -- example: [("n1",[0]),("n1",[1]),("n2",[1])] to [("n1",[0,1]),("n2",[1])]
    multiSet (a:b:bs) | fst a == fst b = multiSet ( (fst a, (snd a) ++ (snd b)) : bs)
                      | otherwise = a : (multiSet (b:bs))

    fname f = last $ init $ concat (map (splitOn "/") (splitOn "." f))


-- | horizontal advances of characters inside a string
-- a character is stored with a string (originally because of ligatures)
horizontalAdvances :: [String] -> FontData -> Bool -> [Double]
horizontalAdvances []          _  _       = []
horizontalAdvances [ch]        fd _       = [hadv ch fd]
horizontalAdvances (ch0:ch1:s) fd kerning = ((hadv ch0 fd) - (ka (sel2 fd))) :
                                            (horizontalAdvances (ch1:s) fd kerning)
  where ka kern | kerning = (kernAdvance ch0 ch1 kern True) + (kernAdvance ch0 ch1 kern False)
                | otherwise = 0

hadv ch fontD | isJust lookup = sel2 (fromJust (Map.lookup ch (sel1 fontD)))
              | otherwise = 0
  where lookup = Map.lookup ch (sel1 fontD)

kernAdvance :: String -> String -> Kern -> Bool -> Double
kernAdvance ch0 ch1 kern u |     u && not (null s0) = (sel5 kern) V.! (head s0)
                           | not u && not (null s1) = (sel5 kern) V.! (head s1)
                           | otherwise = 0
  where s0 = intersect (s sel1 ch0) (s sel2 ch1)
        s1 = intersect (s sel3 ch0) (s sel4 ch1)
        s sel ch = concat (maybeToList (Map.lookup ch (sel kern)))

type OutlineMap =  Map.Map String (Path R2)
data Mode = INSIDE_WH-- ^ INSIDE_WH: The string is stretched inside Width and Height boundaries
          | INSIDE_W -- ^ INSIDE_W:  The string fills the complete width, heigth adjusted
          | INSIDE_H -- ^ INSIDE_H:  The string fills the complete height, width adjusted

mWH INSIDE_WH = True
mWH _ = False
mW INSIDE_W = True
mW _ = False
mH INSIDE_H = True
mH _ = False

-- | See <http://en.wikipedia.org/wiki/Kerning>
data Spacing = KERN -- ^ Recommended, same as HADV but sometimes overridden by kerning:
                    -- i.e. the horizontal advance in "VV" is bigger than in "VA"
             | HADV -- ^ Every glyph has a unique constant horiz. advance

isKern KERN = True
isKern _    = False

type FileName = String


textSVG :: String -> Double -> Path R2
textSVG t h = textSVG_ with { txt = t, textHeight = h }

data TextOpts = TextOpts
                { txt :: String
                , fdo :: (FontData, OutlineMap)
                , mode :: Mode
                , spacing :: Spacing
                , textWidth :: Double
                , textHeight :: Double
                }

ro = unsafePerformIO . getDataFileName -- read only of static data (safe)

bit = outlMap (ro "src/Test/Bitstream.svg")
lin = outlMap (ro "src/Test/LinLibertine.svg")
lin2 = outlMap (ro "src/Test/LinLibertineCut.svg")

instance Default TextOpts where
    def = TextOpts "text" lin INSIDE_H KERN 1 1

-- | Main library functions, usage:
--
-- @
-- \{-\# LANGUAGE NoMonomorphismRestriction \#-\}
--
--import Diagrams.Prelude
--import Diagrams.Backend.Cairo.CmdLine
--import Graphics.SVGFonts.ReadFont
--
--main = defaultMain ( (text' \"Hello World\") \<\> (rect 8 1) \# alignBL )
--
--text' t = stroke (textSVG_ $ TextOpts t lin INSIDE_H KERN 1 1 ) \# fc purple \# fillRule EvenOdd
-- @
--
textSVG_ :: TextOpts -> Path R2
textSVG_ to | mWH (mode to) = makeString (textWidth to) (textHeight to)
            | mW  (mode to) = makeString (textWidth to) ((textWidth to) * maxY / sumh)
            | mH  (mode to) = makeString ((textHeight to) * sumh / maxY) (textHeight to)
  where
    makeString w h = -- translate (-w/2, - h/2) $ -- origin in the middle
                     scaleY (h/maxY) $ scaleX (w/sumh) $
                     translateY (- bbox_ly fontD) $
                     mconcat $
                     zipWith translate horPos
                     (map (polygonChar outl) str)
    (fontD,outl) = (fdo to)
    polygonChar outl ch = fromJust (Map.lookup ch outl)
    horPos = reverse $ added ( zeroV : (map (unitX ^*) hs) )
    hs = horizontalAdvances str fontD (isKern (spacing to))
    sumh = sum hs
    added = snd.(foldl (\(h,l) (b,_) -> (h ^+^ b, (h ^+^ b):l))
                       (zeroV,[])).  (map (\x->(x,[]))) -- [o,o+h0,o+h0+h1,..]
    maxY = bbox_dy fontD -- max height of glyph

    ligatures = ((filter ((>1).length)).(Map.keys).sel1) fontD
    str = map T.unpack $ characterStrings (txt to) ligatures


bbox_dy fontData = (bbox!!3) - (bbox!!1)
  where bbox = sel3 fontData -- bbox = [lower left x, lower left y, upper right x, upper right y]

bbox_lx fontData   = (sel3 fontData) !! 0
bbox_ly fontData   = (sel3 fontData) !! 1


-- | Generate Paths of outlines
--
outlMap :: String -> (FontData, OutlineMap)
outlMap str = ( fontD, Map.fromList [ (ch, outlines ch) | ch <- allUnicodes ] )
  where
  allUnicodes = Map.keys (sel1 fontD)
  outlines ch = mconcat $ commandsToTrails (commands ch (sel1 fontD)) [] zeroV zeroV zeroV
  fontD = openFont str

commandsToTrails :: [PathCommand] -> [Segment R2] -> R2 -> R2 -> R2 -> [Path R2]
commandsToTrails [] _ _ _ _ = []
commandsToTrails (c:cs) segments l lastContr beginPoint -- l is the endpoint of the last segment
      | isNothing nextSegment = (translate beginPoint (pathFromTrail . close $ fromSegments segments)) :
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
        contr ( C_abs (x1,y1,x2,y2,x,y) ) = r2 (x0+x-x2, y0+y-y2 ) -- control point of bezier curve
        contr ( C_rel (x1,y1,x2,y2,x,y) ) = r2 (   x-x2,    y-y2 )
        contr ( S_abs (x2,y2,x,y) )       = r2 (x0+x-x2, y0+y-y2 )
        contr ( S_rel (x2,y2,x,y) )       = r2 (   x-x2,    y-y2 )
        contr ( Q_abs (x1,y1,x,y) ) = r2 (x0+x-x1, y0+y-y1 )
        contr ( Q_rel (x1,y1,x,y) ) = r2 (   x-x1,    y-y1 )
        contr ( T_abs (x,y) )       = r2 (2*x0-cx, 2*y0-cy )
        contr ( T_rel (x,y) )       = r2 (   x-cx,    y-cy )
        contr ( L_abs (x,y) ) = r2 (x0, y0)
        contr ( L_rel (x,y) ) = r2 ( 0,  0)
        contr ( M_abs (x,y) ) = r2 (x0, y0)
        contr ( M_rel (x,y) ) = r2 ( 0,  0)
        contr ( H_abs x ) = r2 (x0, y0)
        contr ( H_rel x ) = r2 ( 0, y0)
        contr ( V_abs y ) = r2 (x0, y0)
        contr ( V_rel y ) = r2 (x0,  0)

        straight' = straight . r2
        bezier3' p1 p2 p3 = bezier3 (r2 p1) (r2 p2) (r2 p3)

        go ( M_abs (x,y) ) = Nothing
        go ( M_rel (x,y) ) = Nothing
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

commands :: String -> SvgGlyph -> [PathCommand]
commands ch glyph | isJust element = unsafePerformIO $ pathFromString $ sel3 $ fromJust element
                  | otherwise      = []
  where element = Map.lookup ch glyph

