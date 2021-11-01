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

## Porting to version 1.8

Version 1.8 of the library greatly improved the API but introduced a
number of breaking changes.

Previously, functions provided by the library took a large `TextOpts`
options record and directly produced a diagrams `Path`. For example,
it was common to see code like this:

```
text' font h s = (strokeP $ textSVG' (TextOpts font INSIDE_H KERN False h h) s)
               # lw none # fc black
```

Compared to this, version 1.8:
- Introduces a new intermediate type `PathInRect`
- Splits out width and height options into dedicated
  combinators such as `fit_width` and `fit_height`
- There are also several new functions that allow specifying width and
  height in different ways, such as `svgText_fitRect` and `svgText_fitRect_stretchySpace`.

The only things remaining in `TextOpts` are options for the font,
spacing mode, and underline.

Here is an idiomatic way to translate the above example code into the
new API:

```
text' font h s
  = (set_envelope . fit_height h . svgText def { textFont = font } $ s)
  # lw none # fc black
```

- We use the `svgText` function with a default options record `def`,
  overriding the `textFont` field to set the font explicitly.
- We then use the `fit_height` function to scale the resulting text so
  it has height `h`.  This corresponds to our previous use of
  `INSIDE_H`.  This is probably the most common mode, but `fit_width`
  and `svgText_fitRect` also available.
- Finally, we call `set_envelope` which converts a `PathInRect` to a
  `Diagram`, by stroking the text path and appropriately setting the
  envelope.
- If you actually want an explicit `Path` instead of a `Diagram`, you
  can call `drop_rect` instead of `set_envelope`.
- Note that the old `textSVG'` function resulted in a centered local
  origin, whereas all the new API functions result in a local origin
  and the left end of the text baseline.  If you need the text
  centered, you can of course call `centerXY`.
