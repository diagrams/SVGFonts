{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Graphics.SVGFonts.Wrap
  ( wrapTextLine
  , example
  ) where

import Diagrams.Prelude hiding (font, text)
import Graphics.SVGFonts.Text

data Modification
  = Append Char
  | Erase
  deriving Show

data Split
  = Split String String [Modification]
  | TextEnd
  deriving Show

wrapTextLine :: forall n m. (TypeableFloat n, Show n, Monad m) =>
  TextOpts n -> [String -> m Split] -> [(n, n)] -> String -> m (String, String)
wrapTextLine topts = throughLevels 0
  where
    throughLevels w0 (split:splits) ((minw, maxw):rranges) text =
      split text >>= oneChunk w0 w0 text
      where
        oneChunk w wmod full_text TextEnd
          | w' > maxw =
              if wmod >= minw
                then return ("", full_text)
                else throughLevels w splits rranges full_text
          | otherwise = return (full_text, "")
          where ((w+) -> w', _) = fontInfoOf full_text

        oneChunk w wmod full_text (Split chunk rest modifs)
          | wmod' > maxw =
              if wmod >= minw
                then return ("", full_text)
                else throughLevels w splits rranges full_text
          | otherwise = do
              (appendix, rest') <- split rest >>= oneChunk w' wmod' rest
              if null appendix
                then return (chunk', rest')
                else return (chunk ++ appendix, rest')
          where
            ((w+) -> w', ligs) = fontInfoOf chunk
            (reverse -> chunk', wdiff) = applyMods modifs$ reverse$ ligs
            wmod' = w' + wdiff

    throughLevels _ _ _ _ = error "split levels exhausted"

    (fontD, _) = textFont topts
    isKern_ = isKern (spacing topts)

    characterStrings_ = characterStrings' fontD

    fontInfoOf text = (sum hs, zip str hs)
      where
        hs = horizontalAdvances str fontD isKern_
        str = characterStrings_ text

    applyMods :: [Modification] -> [(String, n)] -> (String, n)
    applyMods [] text = (concatMap fst text, 0)
    applyMods (Erase:modifs) ((_, advance):text) = (text', wdiff - advance)
      where (text', wdiff) = applyMods modifs text
    applyMods (Append c2 : modifs) text = (text', wdiff + advance)
      where
        lastChars = case text of
          (c1, _):_ -> [c1, [c2]]
          _ -> [[c2]]
        advance = last$ horizontalAdvances lastChars fontD isKern_
        (text', wdiff) = applyMods modifs (([c2], advance):text)
    applyMods _ _ = error "modification not applicable"


example :: ((String, String), (String, String))
example =
  ( runIdentity$ wrapTextLine def splits [(4000 :: Double, 5000), (0, 5000)] text
  , runIdentity$ wrapTextLine def splits [(6000 :: Double, 8000), (0, 7000)] text
  )
  where
    text = "mornin' gentlemen, how is the business going today?"
    splits = [split1, split2]
    split1 txt = return$
      case break (== ' ') txt of
        (_, "") -> TextEnd
        (chunk, _:rest) -> Split (chunk ++ " ") rest [Erase]
    split2 txt = return$
      case splitAt 2 txt of
        (_, "") -> TextEnd
        (chunk, rest) -> Split (chunk) rest [Append '-']
