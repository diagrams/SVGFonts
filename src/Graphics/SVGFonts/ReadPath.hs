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

import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(emptyDef)

type X = Double
type Y = Double
type Tup = (X,Y)
type X1 = X
type Y1 = Y
type X2 = X
type Y2 = Y
data PathCommand =
  M_abs Tup | -- ^Establish a new current point (with absolute coords)
  M_rel Tup | -- ^Establish a new current point (with coords relative to the current point)
  Z | -- ^Close current subpath by drawing a straight line from current point to current subpath's initial point
  L_abs Tup | -- ^A line from the current point to Tup which becomes the new current point
  L_rel Tup |
  H_abs X | -- ^A horizontal line from the current point (cpx, cpy) to (x, cpy)
  H_rel X |
  V_abs Y | -- ^A vertical line from the current point (cpx, cpy) to (cpx, y)
  V_rel Y |
  C_abs (X1,Y1,X2,Y2,X,Y) | -- ^Draws a cubic Bézier curve from the current point to (x,y) using (x1,y1) as the
  -- ^control point at the beginning of the curve and (x2,y2) as the control point at the end of the curve.
  C_rel (X1,Y1,X2,Y2,X,Y) |
  S_abs (X2,Y2,X,Y) | -- ^Draws a cubic Bézier curve from the current point to (x,y). The first control point is
-- assumed to be the reflection of the second control point on the previous command relative to the current point.
-- (If there is no previous command or if the previous command was not an C, c, S or s, assume the first control
-- point is coincident with the current point.) (x2,y2) is the second control point (i.e., the control point at
-- the end of the curve).
  S_rel (X2,Y2,X,Y) |
  Q_abs (X1,Y1,X,Y) | -- ^A quadr. Bézier curve from the curr. point to (x,y) using (x1,y1) as the control point
  Q_rel (X1,Y1,X,Y) | -- ^Nearly the same as cubic, but with one point less
  T_abs Tup | -- ^T_Abs = Shorthand/smooth quadratic Bezier curveto
  T_rel Tup |
  A_abs | -- ^A = Elliptic arc (not used)
  A_rel
  deriving Show

-- | Convert a SVG path string into a list of commands
pathFromString :: String -> Either String [PathCommand]
pathFromString str = case parse path "" str of
  Left  err -> Left  (show err)
  Right p   -> Right p

spaces :: Parser ()
spaces = skipMany space

path :: Parser [PathCommand]
path = do{ l <- many pathElement
         ; eof
         ; return (concat l)
         }

pathElement :: Parser [PathCommand]
pathElement = do{ whiteSpace;
              do{ symbol "M";  l <- many1 tupel2; return (map (\x-> M_abs x) l) } <|>
              do{ symbol "m";  l <- many1 tupel2; return (map (\x-> M_rel x) l) } <|>
              do{ symbol "z"; return [Z]; } <|>
              do{ symbol "Z"; return [Z]; } <|>
              do{ symbol "L";  l <- many1 tupel2; return (map (\x-> L_abs x) l) } <|>
              do{ symbol "l";  l <- many1 tupel2; return (map (\x-> L_rel x) l) } <|>
              do{ symbol "H";  l <- many1 myfloat; return (map (\x-> H_abs (realToFrac x)) l) } <|>
              do{ symbol "h";  l <- many1 myfloat; return (map (\x-> H_rel (realToFrac x)) l) } <|>
              do{ symbol "V";  l <- many1 myfloat; return (map (\x-> V_abs (realToFrac x)) l) } <|>
              do{ symbol "v";  l <- many1 myfloat; return (map (\x-> V_rel (realToFrac x)) l) } <|>
              do{ symbol "C";  l <- many1 tupel6; return (map (\x-> C_abs x) l) } <|>
              do{ symbol "c";  l <- many1 tupel6; return (map (\x-> C_rel x) l) } <|>
              do{ symbol "S";  l <- many1 tupel4; return (map (\x-> S_abs x) l) } <|>
              do{ symbol "s";  l <- many1 tupel4; return (map (\x-> S_rel x) l) } <|>
              do{ symbol "Q";  l <- many1 tupel4; return (map (\x-> Q_abs x) l) } <|>
              do{ symbol "q";  l <- many1 tupel4; return (map (\x-> Q_rel x) l) } <|>
              do{ symbol "T";  l <- many1 tupel2; return (map (\x-> T_abs x) l) } <|>
              do{ symbol "t";  l <- many1 tupel2; return (map (\x-> T_rel x) l) } <|>
              do{ symbol "A";  l <- many1 tupel2; return (map (\_-> A_abs) l) } <|> -- not used
              do{ symbol "a";  l <- many1 tupel2; return (map (\_-> A_rel) l) }     -- not used
            }

comma :: Parser ()
comma = do{ spaces; try (do { (char ','); return () }) <|> spaces }

tupel2 :: Parser (X,Y)
tupel2 = do{ x <- myfloat; comma; y <- myfloat; spaces;
             return (realToFrac x, realToFrac y)
           }

tupel4 :: Parser (X,Y,X,Y)
tupel4 = do{ x1 <- myfloat; comma; y1 <- myfloat; spaces;
              x <- myfloat; comma;  y <- myfloat; spaces;
             return (realToFrac x1, realToFrac y1, realToFrac x, realToFrac y)
           }

tupel6 :: Parser (X,Y,X,Y,X,Y)
tupel6 = do{ x1 <- myfloat; comma; y1 <- myfloat; spaces;
             x2 <- myfloat; comma; y2 <- myfloat; spaces;
              x <- myfloat; comma;  y <- myfloat; spaces;
             return (realToFrac x1, realToFrac y1, realToFrac x2, realToFrac y2, realToFrac x, realToFrac y)
           }

myfloat :: Parser Double
myfloat = try (do{ symbol "-"; n <- float; return (negate n) }) <|>
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
float :: Parser Double
float           = P.float lexer
