module Graphics.SVGFonts
    (
      -- * Drawing text
      textSVG, textSVG', textSVG_

      -- * Options
    , TextOpts(..), Mode(..), Spacing(..)

      -- * Provided fonts
    , bit, lin, lin2

      -- * Loading fonts
    , loadFont
    )
    where

import Graphics.SVGFonts.Text
import Graphics.SVGFonts.Fonts    (bit, lin, lin2)
import Graphics.SVGFonts.ReadFont (loadFont)
