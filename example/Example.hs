{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Graphics.SVGFonts.ReadFont

main = defaultMain ( (text'' "To Hello World") <> (rect 8 1) # alignBL )

text'  t = stroke (textSVG t 1) # fc purple # fillRule EvenOdd
text'' t = stroke (textSVG_ $ TextOpts t lin INSIDE_H KERN 1 1 )
             # fc purple # fillRule EvenOdd
