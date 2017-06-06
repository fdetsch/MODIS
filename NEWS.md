## MODIS 1.1.0

New features:

  * `getTile()` now supports interactive tile selection from the MODIS Sinusoidal grid powered by **mapedit** (https://github.com/r-spatial/mapedit).
  * Creation of yearly composite layers has been made available through `temporalComposite()` and `aggInterval()`.

Bugfixes:

  * Fixed 'condition has length > 1' warning message from `transDate()` when specifying multiple 'begin' or 'end' dates.
  * Fixed 'no non-missing arguments to min (max); returning Inf' warning message from `getTile()` when 'x' was missing and 'tileH' or 'tileV' were specified as `numeric`. 

Changes:

  * `transDate()` is now also compatible with true 'Date' objects.
  * Argument 'buffer' is no longer available for `getTile()`. As a result, **rgeos** could be removed from package imports. 
  * `getTile()` does no longer support MERIS and SRTM data. As regards the latter, `raster::getData(name = "SRTM", ...)` could be used instead. 
  * Further, `getTile()` is no longer compatible with 'list' input. In the course of this, argument 'extent' has been replaced by 'x' in order to avoid confusion with `raster::extent()`.
  * `temporalComposite()` now relies on `raster::calc()` instead of `raster::overlay()`, which allows the specification of 'na.rm' separate from 'fun'.


## MODIS 1.0.0

* Initial release
