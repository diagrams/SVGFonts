# SVGFonts

Native font support for the Diagrams library. The SVG-Font format is
easy to parse and was therefore chosen for a font library completely
written in Haskell.

You can convert your own font to SVG with
[FontForge](http://fontforge.sourceforge.net/) or use one of the
SVG fonts included with the library.

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
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import qualified Graphics.SVGFonts as F

main = do
  font <- F.loadFont "/path/to/font.svg"
  let
    diagram :: Diagram B
    diagram =
      (F.drop_rect$ F.fit_height 22$ F.svgText def{F.textFont = font} "Hello World!")
      # stroke # fc blue # lc blue # bg lightgrey # fillRule EvenOdd # showOrigin
  mainWith diagram
```

## Usage

Convert your favourite font (*i.e.* `.ttf`) into a `.svg` file with
fontforge (the menu item under "Save All"). If a font converted on
your own doesn't work, try the repair options, and if this still
doesn't work edit the file by hand or [report an
issue](https://github.com/diagrams/SVGFonts/issues).  Remember that a
lot of fonts are not allowed to be distributed freely.
