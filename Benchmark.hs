{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main
import qualified Data.ByteString.Char8 as BS
import Graphics.SVGFonts.ReadFont (loadFont)
import Graphics.SVGFonts.ReadPath (pathFromString)

path :: BS.ByteString
path = BS.unlines
    [ "M600 1100q147 0 219 -49t72 -150q0 -71 -44 -117.5t-124 -60.5q19 -5 48 -38.5t67 -92.5l114 -186h-143l-107 174q-49 80 -79.5 101t-75.5 21h-55v-296h-130v694h238zM594 1016h-102v-232h102q90 0 127 27t37 90q0 62 -37 88.5t-127 26.5zM616 1358q127 0 236 -45"
    , "t199 -135q90 -91 136 -201t46 -236q0 -125 -45.5 -234t-136.5 -200t-200 -136.5t-235 -45.5q-125 0 -234 45.5t-200 136.5t-136.5 200t-45.5 234q0 126 46 236t136 201q90 90 199 135t235 45zM616 1255q-106 0 -196.5 -37t-165.5 -112t-113.5 -167t-38.5 -198"
    , "q0 -104 38.5 -195.5t113.5 -166.5q76 -76 166.5 -114t195.5 -38q106 0 196.5 38t166.5 114t113.5 166.5t37.5 195.5q0 106 -38 198t-113 167t-165.5 112t-197.5 37z"
    ]

main = defaultMain
    [ bench "parse path" $ whnf pathFromString $ BS.unpack path
    , bench "parse font" $ whnfIO $ loadFont "fonts/Bitstream.svg"
    ]
