
#' \code{gsdata}: Interface to the GeoSphere Austria DataHub API (Data Access)
#'
#' This package allows convenient access to data provided by GeoSphere Austria
#' (Austrias federal agency for geology, geophysics, climatology and
#' meteorology) via their data API which exists since around mid 2023.
#'
#' The API not only provides access to station data (the one thing currently
#' covered by this package; will be extended) but also access to spatial
#' data; a catalogue which has been extended over and over again over the past
#' 10 months. Details about all available data sets and their temporal and
#' spatial extent can be found on their website:
#'
#' \itemize{
#'    \item <https://data.hub.geosphere.at/>
#' }
#'
#' @section Data request limit:
#'
#' The API has a request limit; a limit to how much data one is allowed
#' to retrieve in one API request. Details on the current limit can be found
#' in the [GeoSphere Dataset API Documentation](https://dataset.api.hub.geosphere.at/v1/docs/user-guide/request_size_limit.html).
#'
#' This package internally tries to estimate the request size and split the
#' request into multiple batches in case one single request would (likely)
#' exceed these limits.
#'
#' Thus, one single call to e.g., \code{gs_stationdata()} can trigger multiple
#' API calls. If used without `expert = TRUE` two initial calls are made to
#' check if the data set requested does exist, and that the
#' stations and parameters requested exist in this data set. If the data request
#' needs to be split in addition, this can cause a series of calls to the API
#' which also has a limit on number of requests per time.
#'
#' In the worst case this causes a temporary ban (timeout due to too many requests)
#' from the servers. One way around is to limit the number of requests per time,
#' more details about this in the next section.
#'
#'
#' @section Cooldown time/limiting number of requests per time:
#'
#'
#' Note that each function call can result in multiple API requests which can
#' lead to a timeout (too many requests). To avoid running into timeout issues:
#'
#' \itemize{
#'      \item use \code{expert = TURUE} where possible as it
#'            lowers the number of calls to the api.
#'      \item request data for multiple stations at once, especially
#'            when requesting short time periods/few parameters as, in the best case,
#'            all data can be retrieved on one single call (if below estimated
#'            data request limit).
#'      \item wait between requests using e.g., \code{Sys.sleep(...)}.
#'      \item or use the packages own 'cooldown' option. By default,
#'            a cooldown time of \code{0.1} seconds is used (the minimum
#'            time between two requests. You can set a custom cooldown time
#'            via \code{options('gsdata.cooldown' = 1)}. Will overwrite the
#'            default and ensure that there will be at least one second
#'            between consecutive API calls. If you have no time critical
#'            requests this is a good way to be nice to the data provider!
#' }
#'
#' @docType package
#' @name gsdata
"_PACKAGE"

