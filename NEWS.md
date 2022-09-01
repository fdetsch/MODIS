# MODIS 1.2.6.9001 (2022-08-31)

#### ✨ features and improvements

#### 🐛 bug fixes

  * Respects metacharacters in Earthdata Login passwords (#105), but does not 
    allow for single quotes (i.e. `'`) when download method is other than curl 
    to prevent from "sh: 1: Syntax error: Unterminated quoted string" errors

#### 💬 documentation etc

#### 🍬 miscellaneous


# MODIS 1.2.6 (2022-08-30)

#### ✨ features and improvements

  * Enables the verification of Earthdata login credentials against multiple
    servers, e.g. if one is not reachable (#124)


# MODIS 1.2.5 (2022-08-26)

#### ✨ features and improvements

  * Extended list of supported MODIS products and collections (#118)

#### 🍬 miscellaneous

  * Set `sf::sf_use_s2(FALSE)` for spherical geometry (#110)
  * Introduces further checks for valid Earthdata Login credentials (#111, #114)


# MODIS 1.2.3

#### 🐛 bug fixes

  * Explicitly format sf::st_drivers() name columns as 'character', which caused problems on systems running R < 4.0.0.


# MODIS 1.2.2

#### 🍬 miscellaneous

  * .Rbuildignore test-runGdal().


# MODIS 1.2.1

#### 🍬 miscellaneous

  * Migrated unit tests to tinytest.


# MODIS 1.2.0

#### 🍬 miscellaneous

  * Moved GDAL operations to sf::gdal_utils().
  * getHdf(), runGdal() with no extent info (ie. 'x = tileH = tileV = NULL') triggers interactive tile selection.
  * LP DAAC is default download server (previously LAADS).
  * MRT method for getSds() is no longer available.
  * In runGdal(), 'maskValue' is currently ignored until a suitable workaround for multiple nodata values per raster band is found.


# MODIS 1.1.7

#### 🍬 miscellaneous

  * Re-enabled LAADS data retrieval.


# MODIS 1.1.6

#### ✨ features and improvements

  * New set of MODIS products, including MCD43GF, MOD/MYD16A2GF, MOD/MYD16A3GF, MOD/MYD17A2HGF, MOD/MYD17A3, and MOD/MYD17A3HGF.
  * On Windows, if gdalinfo.exe couldn't be found in specified 'gdalPath', scan other environment variables and, in case of success, provide further instructions.

#### 🍬 miscellaneous

  * Ensured compatibility with GDAL3/PROJ6.
  * For non-NSIDC products, getCollection(..., forceCheck = TRUE) no longer requires Earthdata credentials.
  * Fixed broken weblinks.


# MODIS 1.1.5

#### 🍬 miscellaneous

  * Moved internal datasets (products, collections, etc.) from manual creation in zz_lazyload.R, which also had an unnecessary installed.packages() call in it, to R/sysdata.rda.

#### 🐛 bug fixes

  * Missing compatibility of repDoy() with results from extractDate().
  * Duplicate server issue when passing multiple products on to runGdal().


# MODIS 1.1.4

#### ✨ features and improvements

  * After temporary inoperability, runMRT() is fully functional again.
  * Interactive feature drawing is now implemented in getTile(), check out the documentation.
  * getTile() supports 'sp' or 'sf' point geometries with length = 1 (i.e. a single point), in which case the target extent is the MODIS tile covering the specified location.
  * Multiple source download using aria2 whenever more than one server is available (e.g. LAADS and LP DAAC)
  * New classes 'MODISproduct' and 'MODISfile' created by getProduct().

#### 🍬 miscellaneous

  * When running getHdf() (or runGdal()) with LP DAAC or NSIDC as target download server and Earthdata login credentials in ~/.netrc are missing, the user is forced to insert the required information on the command line. This is intended to avoid repeat authentication failures.
  * Functions checkIntegrity(), OutProj(), PixelSize(), ResamplingType(), BlockSize(), OutputCompression(), QuietOutput(), genString(), checkTools(), ModisFileDownloader(), and doCheckIntegrity() no longer have an 'opts' argument.


# MODIS 1.1.3

#### ✨ features and improvements

  * Support for MOD/MYD10 and MOD/MYD29 product series distributed by National Snow & Ice Data Center (NSIDC; <https://nsidc.org/home>).

#### 🐛 bug fixes

  * Curl-based download from LP DAAC.
  * "Error: object 'tid' not found" from climate modeling grids (CMG) not being identified as such, but treated as tiled products.
  * Wrong dimension and resolution of images created by runGdal() when working with whole tiles instead of spatial subsets (i.e. 'tileH,tileV' specified; see <https://github.com/fdetsch/MODIS/issues/46>).

#### 🍬 miscellaneous

  * Code adjustments related to LAADS transition from FTP to HTTPS. In this context, lpdaacLogin() is now deprecated and has been replaced with EarthdataLogin() since both LP DAAC and LAADS require the specification of Earthdata login credentials.
  * EarthdataLogin() now allows multiple entries in a .netrc file in case users have other servers not related to Earthdata.
  * Disabled retrieval of MOD16 products from NTSG server, which is no longer updated.
  * When 'extent' is a country name, 'outProj' is taken from MODISoptions() rather than hard-coded EPSG:4326.
  * If 'begin' falls in between two composite release dates, it is set back to start date of preceding release (see <https://github.com/fdetsch/MODIS/issues/43>).
  * When working with Extent (raster) or bbox objects (sf) and 'outProj' and 'pixelSize' are "asIn", the output grid and resolution is aligned with the original MODIS Sinusoidal grid.
  * Replaced 'XML' package dependency through regular expression matching.


# MODIS 1.1.2

#### ✨ features and improvements

  * Added remaining products from the LP DAAC MODIS Products Table (<https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table>).
  * Explicit specification of 'pos1','pos2' arguments passed to extractDate() and orgTime() is no longer required when dealing with standard MODIS file names.

#### 🐛 bug fixes

  * 'unable to find an inherited method for function 'extent' for signature '"MODISextent"'' from getHdf() when 'extent' was a Spatial* object.
  * 'Error in rgdal::rawTransform(projfrom, projto, nrow(xy), xy[, 1], xy[, : no arguments in initialization list' due to insufficient strsplit() on Windows
  * Orphaned hole and self-intersection errors from getTile() due to some non-valid geometries in map("worldHires", ...) (eg. "Philippines", "Spain").
  * 'DATE' subfolder was created in getOption("MODIS_localArcPath") when running MODIS:::genString() with no particular date specified.

#### 🍬 miscellaneous

  * Disabled use of EPSV (see <https://curl.haxx.se/libcurl/c/CURLOPT_FTP_USE_EPSV.html>) when downloading structure from LP DAAC, LAADS. The latter didn't work anymore with EPSV enabled.
  * getProduct() and getCollection() are now compatible with more than one input 'product' provided using eg. c().
  * At the same time, pattern matching for a distinct set of products (see <https://github.com/fdetsch/MODIS/issues/22>) is switched off as long as a proper regular expression is omitted.
  * The MODIS package is now licensed under the MIT license (<https://www.r-project.org/Licenses/MIT>).


# MODIS 1.1.0

#### ✨ features and improvements

  * getTile() now supports interactive tile selection from the MODIS Sinusoidal grid powered by mapedit (<https://github.com/r-spatial/mapedit>).
  * Creation of yearly composite layers has been made available through temporalComposite() and aggInterval().
  * Meaning of 'quiet' argument in MODISoptions() has changed and now determines whether getHdf() (or runGdal()) print download information to the console.

#### 🐛 bug fixes

  * 'condition has length > 1' warning message from transDate() when specifying multiple 'begin' or 'end' dates.
  * 'no non-missing arguments to min (max); returning Inf' warning message from getTile() when 'x' was missing and 'tileH' or 'tileV' were specified as numeric.
  * ''begin' and 'end' dates seem to be confused, reordering dates...' warning message from aggInterval() when actual end date (ie end of current fortnightly/monthly time interval) lies in the future.
  * 'length of 'dimnames' [2] not equal to array extent' error in temporalComposite() when only one layer is available for a particular aggregation period.

#### 🍬 miscellaneous

  * transDate() is now also compatible with true 'Date' objects.
  * Argument 'buffer' is no longer available for getTile(). As a result, rgeos could be removed from package imports.
  * getTile() does no longer support MERIS and SRTM data. As regards the latter, raster::getData(name = "SRTM", ...) could be used instead.
  * Further, getTile() is no longer compatible with 'list' input. In the course of this, argument 'extent' has been replaced by 'x' in order to avoid confusion with raster::extent().
  * temporalComposite() now relies on raster::calc() instead of raster::overlay(), which allows the specification of 'na.rm' separate from 'fun'.
  * aggInterval() does no longer take 'numeric' input (ie years).


# MODIS 1.0.0
