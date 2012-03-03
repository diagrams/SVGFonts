 {-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Graphics.SVGFonts.ReadFont

main = defaultMain ( (text' "Hello World") <> (rect 8 1) # alignBL )

text' t = stroke (textSVG_ $ TextOpts t lin INSIDE_H KERN 1 100) # lc red # lw 1 # fc purple # fillRule EvenOdd
text'' t = stroke (textSVG_ $ TextOpts t bm INSIDE_H KERN 1 17) # lc orange # lw 0.5 # fc yellow # fillRule EvenOdd

bla = outlMap "src/Test/Blavicke.svg"
bm = outlMap "src/Test/BodieMFHolly.svg"
els = outlMap "src/Test/ElzevierCaps.svg"
gaw = outlMap "src/Test/GirlsareWeird.svg"
boat = outlMap "src/Test/Showboat.svg"
