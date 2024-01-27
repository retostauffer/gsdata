

#' Getting Available Datasets
#'
#' The GeoSphere Austria (formerly ZAMG) datahub API provides an endpoint to get available
#' datasets. This function returns a (possibly pre-filtered) \code{data.frame}
#' containing the dataset \code{type}, \code{mode} and \code{resource_id} needed
#' to perform the data requests (see e.g., \code{gs_stationdata()}) amongst some
#' additional information.
#'
#' @param type \code{NULL} or character of length \code{1} to filter the request.
#' @param mode \code{NULL} or character of length \code{1} to filter the request.
#'        Currently defaults to \code{mode = "station"}.
#' @param config empty list by default; can be a named list to be fowrarded
#'        to the \code{httr::GET} request if needed.
#' @param verbose logical, if set \code{TRUE} some more output will be produced.
#'
#' @return Returns a \code{data.frame} with all available data types and
#' API endpoints. When both \code{type} and \code{mode} are equal to \code{NULL}
#' all data available via the API will be returned.
#' The most important information of this return is the \code{type}, \code{mode},
#' as well as \code{resource_id} which is used to perform data requests.
#'
#' @seealso gs_stationdata
#'
#' @details
#' The API provides an enpoint to get all available data sets which
#' can be filtered using the arguments \code{type} and/or \code{mode}.
#' Classical usecase:
#'
#' Return all data sets where \code{mode == "historical"}:
#' * \code{gs_datasets(mode = "historical")}
#'
#' Return all data sets where \code{type == "grid"}:
#' * \code{gs_datasets(type = "grid")}
#'
#' Can be combined (setting both \code{type} and \code{mode}).
#'
#' @author Reto Stauffer
#' @export
gs_datasets <- function(type = NULL, mode = NULL, config = list(), verbose = FALSE) {
    stopifnot("argument 'verbose' must be logical TRUE or FALSE" =
              isTRUE(verbose) || isFALSE(verbose))
    stopifnot("argument 'config' must be a (named) list" = is.list(config))
    stopifnot("argument 'type' must be NULL or a character string" =
              is.null(type) || (is.character(type) & length(type) == 1L))
    stopifnot("argument 'mode' must be NULL or a character string" =
              is.null(mode) || (is.character(mode) & length(mode) == 1L))

    # Get base URL
    baseurl <- gs_baseurl()
    URL     <- paste(baseurl, "datasets", sep = "/")

    # Query args
    query <- list(); query$type <- type; query$mode <- mode

    # Calling API
    res  <- API_GET(URL, config = config, query = query, verbose = verbose)
    keys <- unique(unlist(lapply(res, names)))

    # Splitting up the path to get the information
    tmp <- strsplit(sub("^\\/", "", names(res)), "/")
    if (!all(sapply(tmp, length)))
        stop("problems decoding the path argument (expected three-part-path)")

    # Else setting up results data.frame
    rval <- data.frame(type        = sapply(tmp, function(x) x[1]),
                       mode        = sapply(tmp, function(x) x[2]),
                       resource_id = sapply(tmp, function(x) x[3]))

    fn <- function(x, k) if (is.character(x[[k]])) x[[k]] else paste(x[[k]], collapse = "|")
    for (k in keys) {
        kn <- if (k == "type") "data_format" else k # New name to not overwrite 'type'
        rval[[kn]] <- unname(sapply(res, fn, k = k))
    }

    # Return data.frame
    return(rval)
}
