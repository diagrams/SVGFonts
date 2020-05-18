module Graphics.SVGFonts
    (
      -- * Drawing text
      render
    , render_raw
    , render_modifyPreglyphs
    , render_fitRect
    , render_fitRect'
    , fit_height
    , fit_width
    , set_envelope
    , drop_bounds

      -- * Text boxes
    , wrapTextLine

      -- * Options
    , TextOpts(..), Spacing(..)

      -- * Provided fonts
    , bit, lin, lin2

      -- * Loading fonts
    , loadFont, loadDataFont
    )
    where

import Graphics.SVGFonts.Text
import Graphics.SVGFonts.Fonts    (bit, lin, lin2, loadDataFont)
import Graphics.SVGFonts.ReadFont (loadFont)
import Graphics.SVGFonts.Wrap (wrapTextLine)
