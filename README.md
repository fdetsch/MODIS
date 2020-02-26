## **MODIS**: Acquisition and Processing of MODIS Products

[![CRAN_version](http://www.r-pkg.org/badges/version/MODIS)](https://cran.r-project.org/package=MODIS)
[![CRAN_check](https://cranchecks.info/badges/worst/MODIS)](https://cran.r-project.org/web/checks/check_results_MODIS.html)
[![GitHub](https://img.shields.io/github/license/mashape/apistatus.svg)](https://opensource.org/licenses/MIT)
[![Join the chat at https://gitter.im/r-modis/Lobby](https://badges.gitter.im/r-modis/Lobby.svg)](https://gitter.im/r-modis/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


====

### Important information

As of 2019-12-05, LAADS requires [authentication via the User Registration System (URS)](https://ladsweb.modaps.eosdis.nasa.gov/alerts-and-issues/?id=35216). Since this is NOT YET part of the **MODIS** package, users are advised to use `MODISserverOrder = "LPDAAC"` until further notice. Please refer to [#77](https://github.com/MatMatt/MODIS/issues/77) for further information.


====

### Package downloads

This month      | In total
--------------- | -----------
![month](http://cranlogs.r-pkg.org/badges/MODIS) | ![total](http://cranlogs.r-pkg.org/badges/grand-total/MODIS)


====

### Installation

**MODIS** can be installed via 


```S
install.packages("MODIS")
```


To install the latest development version, first install **[devtools](https://cran.r-project.org/package=devtools)** and subsequently run

```S
devtools::install_github("MatMatt/MODIS", ref = "develop")
```


====

### Additional resources

* https://stevemosher.wordpress.com/modis-tutorial/
* https://cornelllabofornithology.github.io/ebird-best-practices/covariates.html#covariates-dl


====

### Contact

Please file bug reports and feature requests at https://github.com/MatMatt/MODIS/issues.
