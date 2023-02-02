

#' Getting Available Datasets
#'
#' The Geosphere (ZAMG) datahub API provides an endpoint to get all
#' available datasets. This function returns 
#'
#' @param version integer, API version (defaults to 1).
#' @param config empty list, forwarded to \code{\link[httr]{GET}} if needed.
#'
#' @return Returns a \code{data.frame} with all available data types and
#' API endpoints.
#'
#' @importFrom httr GET content status_code
#' @author Reto Stauffer
gs_datasets <- function(version = 1L, config = list()) {

    # Get base URL; performs version sanity check
    baseurl <- gs_baseurl(version)

    URL <- paste(baseurl, "datasets", sep = "/")

    req <- GET(URL, config = config)
    if (!status_code(req) == 200)
        stop("HTTP request error: got status code", status_code(req))
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
