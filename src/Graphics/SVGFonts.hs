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
