## MODIS 1.2.3.9000 (2021-05-26)

tbc.


## MODIS 1.2.3

Bugfixes:

  * Explicitly format sf::st_drivers() name columns as 'character', which caused problems on systems running R < 4.0.0.


## MODIS 1.2.2

Changes:

  * .Rbuildignore test-runGdal().


## MODIS 1.2.1

Changes:

  * Migrated unit tests to tinytest.


## MODIS 1.2.0

Changes:

  * Moved GDAL operations to sf::gdal_utils().
  * getHdf(), runGdal() with no extent info (ie. 'x = tileH = tileV = NULL') triggers interactive tile selection. 
  * LP DAAC is default download server (previously LAADS).
  * MRT method for getSds() is no longer available.
  * In runGdal(), 'maskValue' is currently ignored until a suitable workaround for multiple nodata values per raster band is found.


## MODIS 1.1.7

Changes:

  * Re-enabled LAADS data retrieval.


## MODIS 1.1.6

New features:

  * New set of MODIS products, including MCD43GF, MOD/MYD16A2GF, MOD/MYD16A3GF, MOD/MYD17A2HGF, MOD/MYD17A3, and MOD/MYD17A3HGF.
  * On Windows, if gdalinfo.exe couldn't be found in specified 'gdalPath', scan other environment variables and, in case of success, provide further instructions.

Changes:

  * Ensured compatibility with GDAL3/PROJ6.
  * For non-NSIDC products, getCollection(..., forceCheck = TRUE) no longer requires Earthdata credentials.
  * Fixed broken weblinks.


## MODIS 1.1.5

Changes:

  * Moved internal datasets (products, collections, etc.) from manual creation in zz_lazyload.R, which also had an unnecessary installed.packages() call in it, to R/sysdata.rda.

Bugfixes:

  * Missing compatibility of repDoy() with results from extractDate().
  * Duplicate server issue when passing multiple products on to runGdal().


## MODIS 1.1.4

New features: 

  * After temporary inoperability, runMRT() is fully functional again.
  * Interactive feature drawing is now implemented in getTile(), check out the documentation.
  * getTile() supports 'sp' or 'sf' point geometries with length = 1 (i.e. a single point), in which case the target extent is the MODIS tile covering the specified location.
  * Multiple source download using aria2 whenever more than one server is available (e.g. LAADS and LP DAAC)
  * New classes 'MODISproduct' and 'MODISfile' created by getProduct().

Changes:

  * When running getHdf() (or runGdal()) with LP DAAC or NSIDC as target download server and Earthdata login credentials in ~/.netrc are missing, the user is forced to insert the required information on the command line. This is intended to avoid repeat authentication failures.
  * Functions checkIntegrity(), OutProj(), PixelSize(), ResamplingType(), BlockSize(), OutputCompression(), QuietOutput(), genString(), checkTools(), ModisFileDownloader(), and doCheckIntegrity() no longer have an 'opts' argument.


## MODIS 1.1.3

New features: 

  * Support for MOD/MYD10 and MOD/MYD29 product series distributed by National Snow & Ice Data Center (NSIDC; <https://nsidc.org/>).
  
Bugfixes: 

  * Curl-based download from LP DAAC.
  * "Error: object 'tid' not found" from climate modeling grids (CMG) not being identified as such, but treated as tiled products.
  * Wrong dimension and resolution of images created by runGdal() when working with whole tiles instead of spatial subsets (i.e. 'tileH,tileV' specified; see <https://github.com/MatMatt/MODIS/issues/46>).
  
Changes:

  * Code adjustments related to LAADS transition from FTP to HTTPS. In this context, lpdaacLogin() is now deprecated and has been replaced with EarthdataLogin() since both LP DAAC and LAADS require the specification of Earthdata login credentials.
  * EarthdataLogin() now allows multiple entries in a .netrc file in case users have other servers not related to Earthdata.
  * Disabled retrieval of MOD16 products from NTSG server, which is no longer updated.
  * When 'extent' is a country name, 'outProj' is taken from MODISoptions() rather than hard-coded EPSG:4326.
  * If 'begin' falls in between two composite release dates, it is set back to start date of preceding release (see <https://github.com/MatMatt/MODIS/issues/43>).
  * When working with Extent (raster) or bbox objects (sf) and 'outProj' and 'pixelSize' are "asIn", the output grid and resolution is aligned with the original MODIS Sinusoidal grid. 
  * Replaced 'XML' package dependency through regular expression matching.
  

## MODIS 1.1.2

New features:

  * Added remaining products from the LP DAAC MODIS Products Table (<https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table>).
  * Explicit specification of 'pos1','pos2' arguments passed to extractDate() and orgTime() is no longer required when dealing with standard MODIS file names.

Bugfixes:

  * 'unable to find an inherited method for function 'extent' for signature '"MODISextent"'' from getHdf() when 'extent' was a Spatial* object.
  * 'Error in rgdal::rawTransform(projfrom, projto, nrow(xy), xy[, 1], xy[, : no arguments in initialization list' due to insufficient strsplit() on Windows
  * Orphaned hole and self-intersection errors from getTile() due to some non-valid geometries in map("worldHires", ...) (eg. "Philippines", "Spain").
  * 'DATE' subfolder was created in getOption("MODIS_localArcPath") when running MODIS:::genString() with no particular date specified.
  
Changes:

  * Disabled use of EPSV (see <https://curl.haxx.se/libcurl/c/CURLOPT_FTP_USE_EPSV.html>) when downloading structure from LP DAAC, LAADS. The latter didn't work anymore with EPSV enabled.
  * getProduct() and getCollection() are now compatible with more than one input 'product' provided using eg. c().
  * At the same time, pattern matching for a distinct set of products (see <https://github.com/MatMatt/MODIS/issues/22>) is switched off as long as a proper regular expression is omitted.
  * The MODIS package is now licensed under the MIT license (<https://www.r-project.org/Licenses/MIT>).
  

## MODIS 1.1.0

New features:

  * getTile() now supports interactive tile selection from the MODIS Sinusoidal grid powered by mapedit (<https://github.com/r-spatial/mapedit>).
  * Creation of yearly composite layers has been made available through temporalComposite() and aggInterval().
  * Meaning of 'quiet' argument in MODISoptions() has changed and now determines whether getHdf() (or runGdal()) print download information to the console. 

Bugfixes:

  * 'condition has length > 1' warning message from transDate() when specifying multiple 'begin' or 'end' dates.
  * 'no non-missing arguments to min (max); returning Inf' warning message from getTile() when 'x' was missing and 'tileH' or 'tileV' were specified as numeric. 
  * ''begin' and 'end' dates seem to be confused, reordering dates...' warning message from aggInterval() when actual end date (ie end of current fortnightly/monthly time interval) lies in the future.
  * 'length of 'dimnames' [2] not equal to array extent' error in temporalComposite() when only one layer is available for a particular aggregation period. 

Changes:

  * transDate() is now also compatible with true 'Date' objects.
  * Argument 'buffer' is no longer available for getTile(). As a result, rgeos could be removed from package imports. 
  * getTile() does no longer support MERIS and SRTM data. As regards the latter, raster::getData(name = "SRTM", ...) could be used instead. 
  * Further, getTile() is no longer compatible with 'list' input. In the course of this, argument 'extent' has been replaced by 'x' in order to avoid confusion with raster::extent().
  * temporalComposite() now relies on raster::calc() instead of raster::overlay(), which allows the specification of 'na.rm' separate from 'fun'.
  * aggInterval() does no longer take 'numeric' input (ie years).


## MODIS 1.0.0

* Initial release
