{-|
  == Text drawing example

  The following is a simple example that covers basic use cases of the
  predefined text drawing functions. The text rendering can be even more
  customized, see e.g. the implementation of 'svgText_fitRect_stretchySpace'.

  The example creates:

  * a width-adjusted text: @textw@

  * a height-adjusted text: @texth@

  * a text adjusted to both width and height with evenly stretched advances of
  characters: @textr@

  * a text adjusted to both width and height, where space characters are
  stretched more than others: @textrs@

  * a text, from which only the path is extracted, dropping the enclosing
  rectangle given by font: @textp@

  * a text wrapped and block-adjusted into a box: @textbox@

  <<diagrams/src_Graphics_SVGFonts_allTexts.svg#diagram=allTexts&width=400>>

  > {-# LANGUAGE FlexibleContexts #-}
  >
  > import Diagrams.Prelude hiding (text, width, height)
  > import Diagrams.Backend.SVG.CmdLine
  > import qualified Graphics.SVGFonts as F
  > import qualified Graphics.SVGFonts.Wrap as FW
  >
  > main = mainWith diagram
  >
  > diagram :: Diagram B
  > diagram = vsep 1 [textw, texth, textr, textrs, textp, textbox]
  >
  > text = "Hello World, ahoy!"
  > long_text =
  >   "At vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis \
  >   \praesentium voluptatum deleniti atque corrupti quos dolores."
  >
  > width = 200 :: Double
  > height = 22
  >
  > stylize text = text # fc blue # lc blue # bg lightgrey # fillRule EvenOdd # showOrigin
  > text_diagram = stylize . F.set_envelope
  >
  > textw = text_diagram$ F.fit_width width$ F.svgText def text
  > texth = text_diagram$ F.fit_height height$ F.svgText def text
  > textr = text_diagram$ F.svgText_fitRect def (width, height) text
  > textrs = text_diagram$ F.svgText_fitRect_stretchySpace def (width, height) 5 text
  > textp = stylize$ stroke$ F.drop_rect$ F.fit_height height$ F.svgText def text
  >
  > textbox = stylize$ vcat$ map F.set_envelope$
  >   map (F.svgText_fitRect_stretchySpace def (width, height) 5) (init texts)
  >   ++ [F.fit_height height$ F.svgText def$ last texts]
  >   where
  >     texts = case FW.wrapText def height splits long_text of
  >       Just texts -> texts
  >       Nothing -> map return long_text
  >
  >     splits =
  >       [ (FW.splitAtSpaces, (width - 30, width + 10))
  >       , (FW.splitEachTwoChars, (width - 30, width + 10))
  >       , (const Nothing, (-1, 1/0))
  >       ]

  == Font loading example

  > main = do
  >   font1 <- loadDataFont "fonts/LinLibertine.svg"
  >   font2 <- loadFont "/path/to/font.svg"
  >   let
  >     t1 = F.svgText_raw def{textFont = font1} "Hello"
  >     t2 = F.svgText_raw def{textFont = font2} "Hello"
  >   mainWith (stroke t1 === stroke t2 :: Diagram B)

-}

module Graphics.SVGFonts
    (
      -- * Drawing text
      svgText
    , svgText_raw
    , svgText_modifyPreglyphs
    , svgText_fitRect
    , svgText_fitRect_stretchySpace
    , fit_height
    , fit_width
    , set_envelope
    , drop_rect

      -- * Options
    , TextOpts(..), Spacing(..)

      -- * Provided fonts
    , bit, lin, lin2

      -- * Loading fonts
    , loadFont, loadDataFont

      -- * Backward compatibility
    , textSVG
    )
    where

import Graphics.SVGFonts.Text
import Graphics.SVGFonts.Fonts    (bit, lin, lin2, loadDataFont)
import Graphics.SVGFonts.ReadFont (loadFont)
import Graphics.SVGFonts.PathInRect
