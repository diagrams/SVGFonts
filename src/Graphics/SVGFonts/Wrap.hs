{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.SVGFonts.Wrap
  ( wrapTextLine
  , wrapText
  , splitAtSpaces
  , splitEachTwoChars
  , example
  ) where

import Diagrams.Prelude hiding (font, text)
import Graphics.SVGFonts.Text
import Graphics.SVGFonts.ReadFont (bbox_dy)

data Modification
  = Append Char
  | Erase
  deriving Show

data Split
  = Split String String [Modification]
  | TextEnd
  deriving Show

wrapTextLine :: forall n m. (TypeableFloat n, Monad m) =>
  TextOpts n -> n -> [(String -> m Split, (n, n))] -> String -> m (String, String)
wrapTextLine topts desired_height = throughLevels 0
  where
    throughLevels w0 ((split, scale_range -> (minw, maxw)):splits) text =
      split text >>= oneChunk w0 w0 text
      where
        oneChunk w wmod full_text TextEnd
          | w' > maxw =
              if wmod > minw
                then return ("", full_text)
                else throughLevels w splits full_text
          | otherwise = return (full_text, "")
          where ((w+) -> w', _) = fontInfoOf full_text

        oneChunk w wmod full_text (Split chunk rest modifs)
          | wmod' > maxw =
              if wmod > minw
                then return ("", full_text)
                else throughLevels w splits full_text
          | otherwise = do
              (appendix, rest') <- split rest >>= oneChunk w' wmod' rest
              return$ if null appendix
                then (chunk', rest')
                else (chunk ++ appendix, rest')
          where
            ((w+) -> w', ligs) = fontInfoOf chunk
            (reverse -> chunk', wdiff) = applyMods modifs$ reverse$ ligs
            wmod' = w' + wdiff

    throughLevels _ _ _ = error "split levels exhausted"

    (fontD, _) = textFont topts
    isKern_ = isKern (spacing topts)

    font_height = bbox_dy fontD
    font_scale = font_height / desired_height
    scale_range (minw, maxw) = (minw*font_scale, maxw*font_scale)

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


wrapText :: forall n m. (TypeableFloat n, Monad m) =>
  TextOpts n -> n -> [(String -> m Split, (n, n))] -> String -> m [String]
wrapText topts desired_height splits text = closure text
  where
    closure "" = return []
    closure text_ = do
      (line, rest) <- wrapTextLine' text_
      rest' <- closure rest
      return$ line : rest'
    wrapTextLine' = wrapTextLine topts desired_height splits


splitAtSpaces :: Monad m => String -> m Split
splitAtSpaces txt = return$
  case break (== ' ') txt of
    (_, "") -> TextEnd
    (chunk, _:rest) -> Split (chunk ++ " ") rest [Erase]


splitEachTwoChars :: Monad m => String -> m Split
splitEachTwoChars txt = return$
  case splitAt 2 txt of
    (_, "") -> TextEnd
    (chunk, rest) -> Split (chunk) rest [Append '-']


example :: (Maybe [String], Maybe [String])
example =
  ( wrapText def 10 splits1 text
  , wrapText def 10 splits2 text
  )
  where
    text = "mornin' gentlemen, how is the business going today?"
    splits1 =
      [ (splitAtSpaces, (40 :: Double, 50))
      , (splitEachTwoChars, (0, 50))
      , (const Nothing, (-1, 1/0))
      ]

    splits2 =
      [ (splitAtSpaces, (0 :: Double, 2))
      , (splitEachTwoChars, (0, 2))
      , (const Nothing, (-1, 1/0))
      ]
