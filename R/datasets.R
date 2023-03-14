

#' Getting Available Datasets
#'
#' The Geosphere (ZAMG) datahub API provides an endpoint to get all
#' available datasets. This function returns 
#'
#' @param type \code{NULL} or character of length \code{1} to filter the request.
#' @param mode \code{NULL} or character of length \code{1} to filter the request.
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
#' can be filtered using the arguments \code{type} and/or \code{mode}.
#' Classical usecase:
#'
#' Return all data sets where \code{mode == "historical"}:
#' * \code{gs_datasets(mode = "historical")}
#'
#' Return all data sets where \code{type == "grid"}:
#' * \code{gs_datasets(type = "grid")}
#'
#' Can be combined (settinb both \code{type} and \code{mode}).
#'
#' @importFrom httr GET content status_code
#' @author Reto Stauffer
#' @export
gs_datasets <- function(type = NULL, mode = NULL, version = 1L, config = list(), verbose = FALSE) {
    stopifnot("argument 'verbose' must be logical TRUE or FALSE" = isTRUE(verbose) || isFALSE(verbose))
    stopifnot("argument 'config' must be a (named) list" = is.list(config))
    stopifnot("argument 'type' must be NULL or a character string" = is.null(type) || (is.character(type) & length(type) == 1L))
    stopifnot("argument 'mode' must be NULL or a character string" = is.null(mode) || (is.character(mode) & length(mode) == 1L))

    # Parsing 'query' arguments (if any)
    # Get base URL; performs version sanity check
    baseurl <- gs_baseurl(version)
    URL     <- paste(baseurl, "datasets", sep = "/")

    # Query args
    query <- list(); query$type <- type; query$mode <- mode

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
