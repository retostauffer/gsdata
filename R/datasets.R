

#' Getting Available Datasets
#'
#' The Geosphere (ZAMG) datahub API provides an endpoint to get all
#' available datasets. This function returns 
#'
#' @param \dots query arguments passed to the API. See 'Details' for 
#'        more examples and classical use cases.
#' @param version integer, API version (defaults to 1).
#' @param config empty list by default; can be a named list to be fowrarded
#'        to the \code{httr::GET} request if needed.
#' @param verbose logical, if set \code{TRUE} some more output will be produced.
#'
#' @return Returns a \code{data.frame} with all available data types and
#' API endpoints.
#'
#' @details
#' The API provides an enpoint to get all available data sets which
#' can be filtered using the \code{\dots} argument. If nothing is specified,
#' all available data sets will be returned as a \code{data.frame}.
#' Filtering for any of the variables contained in this \code{data.frame}
#' is possible. Classical usecase:
#'
#' Return all data sets where \code{mode == "historical"}:
#' * \code{gs_datasets(mode = "historical")}
#'
#' Return all data sets where \code{type == "grid"}:
#' * \code{gs_datasets(type = "grid")}
#'
#' Return all data sets of \code{type == "timeseries"} where one of the
#' possible response formats is \code{response_format == "geojson")}:
#' * \code{gs_datasets(type = "timeseries", response_formats = "geojson")}
#'
#' This can of couse also be done using \code{subset()} or other subsetting
#' methods in \emph{R}, however, when specified as input arguments on the \code{\dots}
#' argument this job is taken care of by the API. Unknown arguments will be ignored
#' (e.g., \code{country}), if filtering does not work the API will responde
#' with an error which will be thrown (e.g., when using \code{type = "spatialdata"})
#' and a list of allowed options will be shown as returned by the API.
#'
#' @importFrom httr GET content status_code
#' @author Reto Stauffer
#' @export
gs_datasets <- function(..., version = 1L, config = list(), verbose = FALSE) {

    # Parsing 'query' arguments (if any)
    query <- as.list(match.call(expand.dots = TRUE))[-1]
    query <- query[!grepl("^(version|config|verbose)$", names(query))]

    # Get base URL; performs version sanity check
    baseurl <- gs_baseurl(version)
    URL     <- paste(baseurl, "datasets", sep = "/")

    # Verbosity
    if (verbose)
        message(sprintf("Calling: %s%s%s", URL,
                        if (!length(query)) "" else "?",
                        if (!length(query)) "" else paste(paste(names(query), query, sep = "="), collapse = "&")))

    # Requesting data
    req  <- GET(URL, config = config, query = query)
    if (!status_code(req) == 200) {
        tmp <- try(content(req))
        if (is.list(tmp) && !is.null(tmp$detail[[1]]$msg)) {
            stop(tmp$detail[[1]]$msg)
        } else {
            stop("HTTP request error: got status code", status_code(req))
        }
    }
    content <- content(req)
    keys    <- unique(unlist(lapply(content, names)))

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

    # Return data.frame
    return(res)
}
