# **MODIS**: Acquisition and Processing of MODIS Products

⚠️ **Note:** The `MODIS` package is currently not fully functional due to recent 
changes in authentication mechanisms at LP DAAC and LAADS (Earthdata Login and 
token-based access). While some core features may still work, automated 
downloading and processing are affected. Users working with MODIS data are 
encouraged to explore alternative packages listed below.

## 🔍 Related and Similar Packages

While the `MODIS` package provides an integrated workflow for discovering, 
downloading, mosaicking, and processing MODIS satellite data, users may also 
find value in the following actively maintained alternatives—especially for 
workflows requiring compatibility with newer versions of R:

### [`MODISTools`](https://github.com/sntck/MODISTools)
- **Focus:** Efficient access to MODIS subsets via the ORNL DAAC web service 
  (MODIS Subsets API).
- **Highlights:** Streamlined for point-based time series extraction (e.g., 
  vegetation indices at specific locations). Ideal for ecological or 
  site-based studies.
- **Limitations:** Does not support full-tile downloading or mosaicking of 
  spatial rasters.

### [`MODIStsp`](https://github.com/ropensci/MODIStsp)
- **Focus:** GUI and command-line tool for pre-processing MODIS time series.
- **Highlights:** Provides a user-friendly interface (also scriptable) to 
  automate downloading, reprojecting, mosaicking, and time series creation.
- **Best for:** Batch processing of MODIS raster products across time and 
  space with minimal coding.

### [`rsat`](https://github.com/ropensci/rsat)
- **Focus:** General framework for working with remote sensing time series 
  from multiple platforms.
- **Highlights:** MODIS is supported alongside Sentinel, Landsat, and others. 
  Designed for harmonized, cross-sensor workflows.
- **Best for:** Projects integrating MODIS with other remote sensing data 
  sources in a unified workflow.

### [`rspatial.org` MODIS Guide](https://rspatial.org/modis/2-download.html)
- **Focus:** Educational resource demonstrating MODIS data download and 
  pre-processing using base R and `terra`.
- **Highlights:** Teaches how to manually script MODIS data access and 
  preparation. Good for learning fundamentals and customizing your own 
  pipeline.

---

If you continue to rely on `MODIS` for legacy workflows or specific utilities, 
we recommend documenting your environment carefully and checking for ongoing 
support issues (e.g., via GitHub). For new projects, consider evaluating the 
packages above to ensure long-term maintainability.


====

### Package downloads

This month      | In total
--------------- | -----------
![month](http://cranlogs.r-pkg.org/badges/MODIS) | ![total](http://cranlogs.r-pkg.org/badges/grand-total/MODIS)


====

### Installation

**MODIS** can be installed via 


```r
remotes::install_github("fdetsch/MODIS")
```


====

### Additional resources

* https://stevemosher.wordpress.com/modis-tutorial/
* https://cornelllabofornithology.github.io/ebird-best-practices/covariates.html#covariates-dl


====

### Contact

Please file bug reports and feature requests at https://github.com/fdetsch/MODIS/issues.
