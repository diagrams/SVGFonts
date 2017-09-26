1.6.0.3 (25 September 2017)
---------------------------

- Fix link on Hackage page

1.6.0.2 (21 August 2017)
------------------------

- Bug fix: don't print blank line to stdout when there are no errors. (#23)
  Thanks to Tim Docker for the fix.

1.6.0.1 (27 October 2016)
-------------------------

- Allow `diagrams-core-1.4`
- Allow `diagrams-lib-1.4`

1.6 (8 August 2016)
-------------------

- Performance improvement: port ReadPath to use attoparsec
- New `loadFont'` function, to read font data from an XmlSource
- Export `Kern(..)` from `Graphics.SVGFonts.ReadFont`
- New `Serialize` instances for `FontData` and `Kern`

1.5.0.1 (6 June 2016)
---------------------

- allow `base-4.9`
- allow `data-default-class-0.1`
- test with GHC 8.0.1

1.5.0.0 (19 April 2015)
-----------------------

- Split functionality out of `ReadFont`, into `Fonts` (built-in fonts) and
  `Text` (rendering text to Diagrams).
- `textSVG'` and `textSVG_` now have the text as a separate argument,
  distinct from `TextOptions`.
- `ReadFont` does not use `unsafePerformIO` any more. `unsafePerformIO` is
  now only used to load built-in fonts.

1.4.0.3 (2 June 2014)
----------------------

- Allow `diagrams-lib-1.2`.

1.4.0.1 (25 November 2013)
--------------------------

- Bump `diagrams-lib` upper bound to `< 1.1`.

1.4 (10 September 2013)
-----------------------

- `FontData` can now be written back to SVG using `makeSvgFont` inside of the `WriteFont` module.
  It is possible to only write back a specfic set of glyphs.
- The SVG `font-face` element is now completely supported.
  This means all possible attributes are read and written back to it.
  Correct defaults are also set on non optional attributes. Some optional attributes are still
  required though, due to their use in font rendering.
- The `ReadPath` module does not use `unsafePerformIO` anymore. `ReadFont` is now the only module with unsafe calls.
- Minor bug fixes:
  - `stemh` and `stemv` are now optional attributes.

1.3.0.2 (14 August 2013)
------------------------

- remove old comment causing Haddock build to fail

1.3.0.1 (12 August 2013)
------------------------

- fix repo location in .cabal file

1.3: 9 August 2013
------------------

- A bunch of bug fixes, cleanup, and reorganization:
  - Proper data type for `Kern` and `FontData` instead of mega-tuples.
  - Fixed several `Prelude.read` error on `font-face` attributes.
  - Fixed wrong attribute names.
  - Switched to `data-default-class` instead of `data-default`.
- Require `diagrams-lib-0.7`.
