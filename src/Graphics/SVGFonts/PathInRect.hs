{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.SVGFonts.PathInRect where

import Diagrams.Prelude hiding (font, text, render, width, height, envelope)

data PathInRect n = PathInRect n n n n (Path V2 n)

fit_height :: (RealFloat n) => n -> PathInRect n -> PathInRect n
fit_height desired_height (PathInRect x1 y1 x2 y2 path) = PathInRect
  (scale_*x1) (scale_*y1) (scale_*x2) (scale_*y2) (scale scale_ path)
  where scale_ = desired_height / (y2 - y1)

fit_width :: (RealFloat n) => n -> PathInRect n -> PathInRect n
fit_width desired_width (PathInRect x1 y1 x2 y2 path) = PathInRect
  (scale_*x1) (scale_*y1) (scale_*x2) (scale_*y2) (scale scale_$ path)
  where scale_ = desired_width / (x2 - x1)

set_envelope :: forall b n. (TypeableFloat n, Renderable (Path V2 n) b) =>
  PathInRect n -> QDiagram b V2 n Any
set_envelope (PathInRect x1 y1 x2 y2 path) =
  path # stroke # withEnvelope envelope
  where
    envelope :: D V2 n
    envelope = translate (r2 (width/2 + x1, height/2 + y1))$ rect width height
    height = y2 - y1
    width = x2 - x1

drop_rect :: forall n. (RealFloat n) => PathInRect n -> Path V2 n
drop_rect (PathInRect _ _ _ _ path) = path
