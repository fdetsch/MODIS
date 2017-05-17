## MODIS 1.1.0

New features:

  * `getTile()` now supports interactive tile selection from the MODIS Sinusoidal grid powered by **mapedit** (https://github.com/r-spatial/mapedit).

Bugfixes:

  * Fixed 'condition has length > 1' warning message from `transDate()` when specifying multiple 'begin' or 'end' dates.

Changes:

  * `transDate()` is now also compatible with true 'Date' objects.
  * Argument 'buffer' is no longer available for `getTile()`. As a result, **rgeos** could be removed from package imports. 
  * `getTile()` does no longer support MERIS and SRTM data. As regards the latter, `raster::getData(name = "SRTM", ...)` could be used instead.


## MODIS 1.0.0

* Initial release
