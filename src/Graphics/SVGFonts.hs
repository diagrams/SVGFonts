module Graphics.SVGFonts
    (
      -- * Drawing text
      textSVG, textSVG', textSVG_, textSVGStretchingWeights

      -- * Options
    , TextOpts(..), Mode(..), Spacing(..)

      -- * Provided fonts
    , bit, lin, lin2

      -- * Loading fonts
    , loadFont, loadDataFont
    )
    where

import Graphics.SVGFonts.Text
import Graphics.SVGFonts.Fonts    (bit, lin, lin2, loadDataFont)
import Graphics.SVGFonts.ReadFont (loadFont)
