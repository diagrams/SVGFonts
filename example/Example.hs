{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
-- import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Backend.SVG.CmdLine
import Graphics.SVGFonts.ReadFont
import Text.Highlighting.Kate
import qualified Diagrams.TwoD.Size as Size
import Data.Char

-- compile this example with: ghc --make Example
-- main = defaultMain (text'' "Hello World")

main = do getLines <- readFile "Example.hs"
          defaultMain (highlightedText (lines getLines) 1 "haskell")

text'  t = stroke (textSVG t 1) # fc blue # lc blue # bg lightgrey # fillRule EvenOdd # showOrigin

text'' t = stroke (textSVG' $ TextOpts t lin INSIDE_H KERN False 1 1 )
             # fc blue # lc blue # bg lightgrey # fillRule EvenOdd # showOrigin

text''' t = (textSVG_ $ TextOpts t lin INSIDE_H KERN True 1 1 )
             # fc blue # lc blue # bg lightgrey # fillRule EvenOdd # showOrigin

textBit t h c | null t = mempty
              | otherwise = textSVG_ (TextOpts t bit INSIDE_H HADV False 1 h)
                            # fc c # lc c # alignBL # fillRule EvenOdd

-- copy and change it to your needs
highlightedText code h lang = (border 0 w hh placedTextLines) `atop` roundedBox
  where
    -- placing textLines below each other
    placedTextLines = vcat' with { sep = h/5 } textLines # alignBL
    textLines = map l hlines # alignBL
    l line | null line = hcat' with { sep = 0.1 } [textBit " " h black]
           | otherwise = hcat' with { sep = 0.1 } (map token line)
    token (KeywordTok,str)  = textBit str h darkred
    token (DataTypeTok,str) = textBit str h seagreen
    token (DecValTok,str)   = textBit str h fuchsia
    token (BaseNTok,str)    = textBit str h fuchsia
    token (FloatTok,str)    = textBit str h fuchsia
    token (CharTok,str)     = textBit str h fuchsia
    token (StringTok,str)   = textBit str h fuchsia
    token (CommentTok,str)  = textBit str h blue
    token (OtherTok,str)    = textBit str h darkviolet
    token (AlertTok,str)    = textBit str h darkviolet
    token (FunctionTok,str) = textBit str h black
    token (RegionMarkerTok,str) = textBit str h black
    token (ErrorTok,str)    = textBit str h black
    token (NormalTok,str)   = textBit str h black
    roundedBox = (roundedRect w hh (h/5)) # alignBL # opacity 0.8
    w = Size.width  (placedTextLines :: D R2)
    hh = Size.height (placedTextLines :: D R2)
    hlines :: [SourceLine]
    hlines = map concat $ map (highlightAs lang) code

-- reduce the size of an object by len in all directions
border hrel w h obj | w > len && h > len = obj # scaleX ((w-len)/w)
                                               # scaleY ((h-len)/h)
                                               # translateX (len/2)
                                               # translateY (-len/2)
                    | otherwise           = obj
  where len = hrel*h

