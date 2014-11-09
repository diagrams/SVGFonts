# SVGFonts

Native font support for the Diagrams library. The SVG-Font format is
easy to parse and was therefore chosen for a font library completely
written in Haskell.

You can convert your own font to SVG with
http://fontforge.sourceforge.net/ or use the included LinLibertine,
Bitstream.

## Features 

Complete implementation of the features that fontforge produces (but
not the complete SVG format):

* Kerning (e.g. the two characters in "VA" have a shorter distance than in "VV") 
* Unicode 
* Ligatures 
* Text boxes with syntax highlighting

XML speed issues can be solved by trimming the svg file to only those
characters that are used (or maybe binary xml one day)

Version 1.0 of this library supports texturing which would only make
sense in a Diagrams Backend that does rasterization in Haskell.

## Example

```
{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Graphics.SVGFonts

main = defaultMain ( (text' "Hello World") <> (rect 8 1) # alignBL )

text'  t = stroke (textSVG t 1) # fc purple # fillRule EvenOdd
text'' t = stroke (textSVG_ (TextOpts lin INSIDE_H KERN True 1 1) t)
             # fc purple # fillRule EvenOdd

-- using a local font
text''' t = do
    font <- loadFont "path/to/font.xml"
    return $ stroke (textSVG' (TextOpts font INSIDE_H KERN False 1 1) t)
```

## Usage

Convert your favourite font (i.e.  .ttf) into a .svg file with
fontforge (the menu item under "Save All"). If a font converted on
your own doesn't work, try the repair options, and if this still
doesn't work edit the file by hand or tell me.  Remember that a lot of
fonts are not allowed to be distributed freely.
