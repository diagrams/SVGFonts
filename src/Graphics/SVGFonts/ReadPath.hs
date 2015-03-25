{-# LANGUAGE CPP                        #-}

--------------------------------------------------------------------
-- |
-- Module    : Graphics.SVG.ReadPath
-- Copyright : (c) 2011 Tillmann Vogt
-- License   : BSD3
--
-- Maintainer: Tillmann Vogt <tillk.vogt@googlemail.com>
-- Stability : stable
-- Portability: portable
--
-- Parsing the SVG path command, see <http://www.w3.org/TR/SVG/paths.html#PathData> :

module Graphics.SVGFonts.ReadPath
 ( pathFromString,
   PathCommand(..),
 )
 where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative                    hiding (many, (<|>))
#endif

import           Text.ParserCombinators.Parsec          hiding (spaces)
import           Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token    as P

data PathCommand n =
  M_abs (n, n) | -- ^Establish a new current point (with absolute coords)
  M_rel (n, n) | -- ^Establish a new current point (with coords relative to the current point)
  Z | -- ^Close current subpath by drawing a straight line from current point to current subpath's initial point
  L_abs (n, n) | -- ^A line from the current point to (n, n) which becomes the new current point
  L_rel (n, n) |
  H_abs n | -- ^A horizontal line from the current point (cpx, cpy) to (x, cpy)
  H_rel n |
  V_abs n | -- ^A vertical line from the current point (cpx, cpy) to (cpx, y)
  V_rel n |
  C_abs (n,n,n,n,n,n) | -- ^Draws a cubic Bézier curve from the current point to (x,y) using (x1,y1) as the
  -- ^control point at the beginning of the curve and (x2,y2) as the control point at the end of the curve.
  C_rel (n,n,n,n,n,n) |
  S_abs (n,n,n,n) | -- ^Draws a cubic Bézier curve from the current point to (x,y). The first control point is
-- assumed to be the reflection of the second control point on the previous command relative to the current point.
-- (If there is no previous command or if the previous command was not an C, c, S or s, assume the first control
-- point is coincident with the current point.) (x2,y2) is the second control point (i.e., the control point at
-- the end of the curve).
  S_rel (n,n,n,n) |
  Q_abs (n,n,n,n) | -- ^A quadr. Bézier curve from the curr. point to (x,y) using (x1,y1) as the control point
  Q_rel (n,n,n,n) | -- ^Nearly the same as cubic, but with one point less
  T_abs (n, n) | -- ^T_Abs = Shorthand/smooth quadratic Bezier curveto
  T_rel (n, n) |
  A_abs | -- ^A = Elliptic arc (not used)
  A_rel
  deriving Show

-- | Convert a SVG path string into a list of commands
pathFromString :: Fractional n => String -> Either String [PathCommand n]
pathFromString str = case parse path "" str of
  Left  err -> Left  (show err)
  Right p   -> Right p

spaces :: Parser ()
spaces = skipMany space

path :: Fractional n => Parser [PathCommand n]
path = do{ l <- many pathElement
         ; eof
         ; return (concat l)
         }

pathElement :: Fractional n => Parser [PathCommand n]
pathElement =
  whiteSpace *>
  (  symbol "M" *> many1 (M_abs <$> tupel2)
 <|> symbol "m" *> many1 (M_rel <$> tupel2)
 <|> symbol "z" *> pure [Z]
 <|> symbol "Z" *> pure [Z]
 <|> symbol "L" *> many1 (L_abs <$> tupel2)
 <|> symbol "l" *> many1 (L_rel <$> tupel2)
 <|> symbol "H" *> many1 (H_abs <$> myfloat)
 <|> symbol "h" *> many1 (H_rel <$> myfloat)
 <|> symbol "V" *> many1 (V_abs <$> myfloat)
 <|> symbol "v" *> many1 (V_rel <$> myfloat)
 <|> symbol "C" *> many1 (C_abs <$> tupel6)
 <|> symbol "c" *> many1 (C_rel <$> tupel6)
 <|> symbol "S" *> many1 (S_abs <$> tupel4)
 <|> symbol "s" *> many1 (S_rel <$> tupel4)
 <|> symbol "Q" *> many1 (Q_abs <$> tupel4)
 <|> symbol "q" *> many1 (Q_rel <$> tupel4)
 <|> symbol "T" *> many1 (T_abs <$> tupel2)
 <|> symbol "t" *> many1 (T_rel <$> tupel2)
 <|> symbol "A" *> many1 (A_abs <$  (tupel2::Parser (Double,Double)))
 <|> symbol "a" *> many1 (A_rel <$  (tupel2::Parser (Double,Double)))
  )

comma :: Parser ()
comma = spaces *> (try (() <$ char ',' ) <|> spaces)

tupel2 :: Fractional n => Parser (n,n)
tupel2 = do{ x <- myfloat; comma; y <- myfloat; spaces;
             return (x, y)
           }

tupel4 :: Fractional n => Parser (n,n,n,n)
tupel4 = do{ x1 <- myfloat; comma; y1 <- myfloat; spaces;
              x <- myfloat; comma;  y <- myfloat; spaces;
             return (x1, y1, x, y)
           }

tupel6 :: Fractional n => Parser (n,n,n,n,n,n)
tupel6 = do{ x1 <- myfloat; comma; y1 <- myfloat; spaces;
             x2 <- myfloat; comma; y2 <- myfloat; spaces;
              x <- myfloat; comma;  y <- myfloat; spaces;
             return (x1, y1, x2, y2, x, y)
           }

myfloat :: Fractional n => Parser n
myfloat = try (do{ _ <- symbol "-"; n <- float; return (negate n) }) <|>
          try float <|> -- 0 is not recognized as a float, so recognize it as an integer and then convert to float
              do { i<-integer; return(fromIntegral i) }

lexer :: P.TokenParser a
lexer = P.makeTokenParser emptyDef

whiteSpace :: Parser ()
whiteSpace      = P.whiteSpace lexer
symbol :: String -> Parser String
symbol          = P.symbol lexer
integer :: Parser Integer
integer         = P.integer lexer
float :: Fractional n => Parser n
float           = realToFrac <$> P.float lexer
