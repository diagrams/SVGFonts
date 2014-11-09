module Graphics.SVGFonts.Fonts
       ( -- * Built-in fonts
         bit, lin, lin2
       ) where

import System.IO.Unsafe (unsafePerformIO)

import Graphics.SVGFonts.ReadFont (loadFont, PreparedFont)
import Paths_SVGFonts (getDataFileName)

-- | Get full path of data file.
-- Safe if package is installed correctly.
dataFile :: FilePath -> FilePath
dataFile = unsafePerformIO . getDataFileName

-- | Load a font from a file in the data directory.
loadDataFont :: (Read n, RealFloat n) =>
                FilePath -> PreparedFont n
loadDataFont = unsafePerformIO . loadFont . dataFile

-- | Bitstream, a standard monospaced font (used in gedit)
bit :: (Read n, RealFloat n) => PreparedFont n
bit = loadDataFont "fonts/Bitstream.svg"

-- | Linux Libertine, for non-monospaced text.
--   <http://www.linuxlibertine.org/>
--   Contains a lot of unicode characters.
lin :: (Read n, RealFloat n) => PreparedFont n
lin = loadDataFont "fonts/LinLibertine.svg"

-- | Linux Libertine, cut to contain only the most common characters.
--   This results in a smaller file and hence a quicker load time.
lin2 :: (Read n, RealFloat n) => PreparedFont n
lin2 = loadDataFont "fonts/LinLibertineCut.svg"
