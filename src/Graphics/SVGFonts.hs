{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Graphics.SVGFonts
    (
      -- * Drawing text
      textSVG, textSVG', textSVG_

      -- * Options
    , TextOpts(..), Mode(..), Spacing(..)

      -- * Provided fonts
    , bit, lin, lin2
    )
    where

import           Graphics.SVGFonts.ReadFont
