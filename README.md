


# Purpose

Interfacing the [Geosphere Austria Data Hub](https://data.hub.zamg.ac.at/)
(formerly known as ZAMG; Austrian national weather service).
Their API provides a series of publicly available meteorological/climatological data sets.
Currently, this package only allows to download stationdata (no gridded/spatial data).

# Installation

This package is available via [github](https://github.com/retostauffer/gsdata).
The simplest way to install is to use [`remotes`](https://cran.r-project.org/package=remotes)
as follows:

```
library("remotes")
install_github("retostauffer/gsdata")
```

This will automatically resolve reverse dependencies. Note that the
[`sf`](https://cran.r-project.org/package=sf) package may need
additional libraries to be installed on your system (geos, proj4).
For details please read the installation instructions on
<https://r-spatial.github.io/sf/>.

### Dependencies

The package depends on:

* [`httr`](https://cran.r-project.org/package=httr): for data requests
* [`zoo`](https://cran.r-project.org/package=zoo): time series data
* [`sf`](https://cran.r-project.org/package=sf): spatial information
* [`parsedate`](https://cran.r-project.org/package=parsedate): parsing dates

