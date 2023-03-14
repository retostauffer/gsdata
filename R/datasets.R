

#' Getting Available Datasets
#'
#' The Geosphere (ZAMG) datahub API provides an endpoint to get all
#' available datasets. This function returns 
#'
#' @param type \code{NULL} or character. Allows to filter certain
#'        data set types (see 'Details').
#' @param version integer, API version (defaults to 1).
#' @param config empty list by default; can be a named list to be fowrarded
#'        to the \code{httr::GET} request if needed.
#'
#' @return Returns a \code{data.frame} with all available data types and
#' API endpoints.
#'
#' @details
#' The API provides an enpoint to get all available data sets which
#' can be filtered. If \code{type = NULL} all available data sets
#' will be returned, else a specific type is requested.
#'
#' * Unfiltered: \url{https://dataset.api.hub.zamg.ac.at/v1/datasets}
#' * Filtered: \url{https://dataset.api.hub.zamg.ac.at/v1/datasets?type=station}
#' * Filtered: \url{https://dataset.api.hub.zamg.ac.at/v1/datasets?type=timeseries}
#' * Filtered: \url{https://dataset.api.hub.zamg.ac.at/v1/datasets?type=grid}
#' * \dots
#'
#' @importFrom httr GET content status_code
#' @author Reto Stauffer
#' @export
gs_datasets <- function(type = NULL, version = 1L, config = list()) {
    stopifnot("wrong argument 'type'" = is.null(type) || (is.character(type) && length(type) == 1))

    # Get base URL; performs version sanity check
    baseurl <- gs_baseurl(version)

    args <- if (is.null(type)) list() else list(type = type)
    URL  <- paste(baseurl, "datasets", sep = "/")

    req  <- GET(URL, config = config, query = args)
    if (!status_code(req) == 200) {
        tmp <- try(content(req))
        if (is.list(tmp) && !is.null(tmp$detail[[1]]$msg)) {
            stop(tmp$detail[[1]]$msg)
        } else {
            stop("HTTP request error: got status code", status_code(req))
        }
    }
    content <- content(req)

    keys <- unique(unlist(lapply(content, names)))

    # Splitting up the path to get the information
    tmp <- strsplit(sub("^\\/", "", names(content)), "/")
    if (!all(sapply(tmp, length)))
        stop("problems decoding the path argument (expected three-part-path)")

    # Else setting up results data.frame
    res <- data.frame(type = sapply(tmp, function(x) x[1]),
                      mode = sapply(tmp, function(x) x[2]),
                      resource_id = sapply(tmp, function(x) x[3]))

    fn <- function(x, k) if (is.character(x[[k]])) x[[k]] else paste(x[[k]], collapse = "|")
    for (k in keys) {
        kn <- if (k == "type") "data_format" else k # New name to not overwrite 'type'
        res[[kn]] <- unname(sapply(content, fn, k = k))
    }
    return(res)
}
