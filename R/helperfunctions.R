


#' Getting API Base URL
#'
#' Just returns the base URL for API requests. Package developed
#' for API version 1; can technically be overruled by setting
#' \code{options("gsdata.apiversion" = 2L)}.
#'
#' @return String, base URL for the API.
#'
#' @author Reto Stauffer
gs_baseurl <- function() {
    version <- getOption("gsdata.apiversion", default = 1L)
    stopifnot("gsdata.apiversion must be numeric" = is.numeric(version),
              "gsdata.apiversion must have length 1" = length(version) == 1)
    version <- as.integer(version)
    stopifnot("gsdata.apiversion must be larger or equal to 1" = version >= 1)

    return(sprintf("https://dataset.api.hub.geosphere.at/v%d", version))
}

#' Extract/Calculate Temporal Interval
#'
#' Helper function to extract and return the temporal interval
#' (in seconds) for different data sets based on the \code{resource_id}
#' identifier. Used to estimate the number of expected values to be
#' retrieved as there is per-request limit.
#'
#' @param resource_id character, name of the \code{resource_id}
#'
#' @return Returns an integer vector of the same length as the input
#' vector \code{resource_id} with the temporal resolution of the data
#' set in seconds or \code{NA} in case the string could not have been decoded.
#'
#' @examples
#' gs_temporal_interval("test-1d-string")
#' ds <- gs_datasets()
#' gs_temporal_interval(ds$resource_id)
#'
#' @author Reto Stauffer
#' @export
gs_temporal_interval <- function(resource_id) {
    stopifnot("argument 'resource_id' must be of class character" = is.character(resource_id),
              "argument 'resource_id' must be of length >= 1"     = length(resource_id) >= 1)
    # Convert extracted string to seconds
    str2sec <- function(x) {
        n <- as.numeric(regmatches(x, regexpr("^[0-9]+", x)))
        # min = minutes, h = hours, d = days, m = months, y = years
        if (grepl("min$", x)) {
            n * 60L
        } else if (grepl("h$", x)) {
            n * 3600L
        } else if (grepl("d$", x)) {
            n * 86400L
        } else if (grepl("m$", x)) {
            n * 31 * 86400L   # guess for longest months
        } else if (grepl("y$", x)) {
            n * 366 * 86400L  # guess for longest years
        } else {
            NA_integer_
        }
    }
    # Step 1 search pattern
    pattern  <- "-v[0-9]+-[0-9]+[a-z]+"
    # Find matching elements
    idx      <- grep(pattern, resource_id, perl = TRUE)
    # Empty return vector
    res      <- rep(NA_integer_, length(resource_id))
    # Extract interval and insert; or leave it NA
    tmp      <- regmatches(resource_id, regexpr(pattern, resource_id, perl = TRUE))
    tmp      <- regmatches(tmp, regexpr("[0-9]+[a-z]+$", tmp))
    res[idx] <- as.integer(sapply(tmp, str2sec))
    if (any(is.na(res)))
        warning("not able to extract temporal interval from ",
                paste(resource_id[is.na(res)], collapse = ", "), ", returning NA.", sep = "")
    return(res)
}




#' Show HTTP Error Status and Terminate
#'
#' This function is called whenever \code{httr::GET} returns an
#' http status code out of the \code{200} range (success).
#' Shows \code{\link[httr]{http_status}} code information alongside
#' with additional messages returned by the API (if any).
#'
#' @param scode numeric, http status code.
#' @param xtra \code{NULL} or named list with additional information.
#'
#' @return No return, will terminate R.
#'
#' @author Reto Stauffer
# Show http_status message if possible.
show_http_status_and_terminate <- function(scode, xtra = NULL) {

    stopifnot(is.numeric(scode), length(scode) == 1)
    if (scode %/% 100 == 2) return(NULL)
    cat('---\n')

    info <- tryCatch(http_status(scode),
                     error = function(x) NULL)

    # Depending on the status code these are somewhat redundant
    if (!is.null(xtra))
        xtra <- paste(c("  status returned by API:",
                      sprintf("    %-20s %s", sprintf("%s:", names(xtra)), xtra)),
                      collapse = "\n")
    if (!is.null(info))
        info <- paste(c("  http_status description:",
                      sprintf("    %-20s %s", sprintf("%s:", names(info)), info)),
                      collapse = "\n")

    if (is.null(info) & is.null(xtra)) {
        stop("HTTP request error: server returned status code ", scode)
    } else {
        stop(paste("HTTP request error", xtra, info, sep = "\n"))
    }
}

#' Handling API Calls
#' 
#' Small helper function to handle http requests to the API.
#'
#' @param URL the URL to be called.
#' @param config \code{NULL} or \code{list}, forwarded to \code{httr::GET}.
#' @param query \code{NULL} or \code{list}, forwarded to \code{httr::GET}.
#' @param expected_class \code{NULL} or character vector. If set, it is checked
#'        if the returned object is of this class. If not, a warning will be thrown
#'        (no error).
#' @param verbose logical, shows some additional information if \code{TRUE}.
#'
#' @return Returns the object we get from \code{httr::content()} after a successful
#' API call. If an error is detected, an error with additional details will be displayed.
#'
#' @importFrom httr GET status_code content http_status
#' @author Reto Stauffer
API_GET <- function(URL, config = NULL, query = NULL,
                    expected_class = NULL, verbose = FALSE) {
    # Checking URL
    URL <- as.character(URL)[[1]]                    
    stopifnot("argument `URL` is invalid" =
              grepl("^https:\\/\\/dataset.api.hub.geosphere.at", URL))

    stopifnot("argument `config` must be NULL or a list" =
              is.null(config) || is.list(config))
    stopifnot("argument `query` must be NULL or a list" =
              is.null(query) || is.list(query))
    stopifnot("argument `expected` must be NULL or character" =
              is.null(expected_class) || is.character(expected_class))
    stopifnot("argument `verbose` must be logical TRUE or FALSE" =
              isTRUE(verbose) || isFALSE(verbose))


    # Checking and executing cooldown
    gsdata_lastcall <- getOption("gsdata.lastcall", default = Sys.time() - 1); # 1s ago
    gsdata_cooldown <- getOption("gsdata.cooldown", default = .1) # cooldown in secs
    # Ensure positive numeric, if there is an error or any warning we reset to default .1
    gsdata_cooldown <- tryCatch(max(0, as.numeric(gsdata_cooldown)),
                                warning = function(x) .1,
                                error = function(x) .1)
    sleep_time <- gsdata_cooldown - as.numeric(Sys.time() - gsdata_lastcall, units = "secs")
    if (sleep_time > 0.1) {
        if (verbose) message(sprintf("Cooldown, waiting for %.3f seconds", sleep_time))
        Sys.sleep(sleep_time)
    }


    if (verbose) {
        msgq <- if (is.list(query)) {
            paste0("?", paste(paste(names(query), query, sep = "="), collapse = "&"))
        } else { "" }
        message(sprintf("Calling: %s%s", URL, msgq))
    }

    # Requesting data
    req  <- GET(URL, config = config, query = query)
    options("gsdata.lastcall" = Sys.time()) # Updating last call

    if (!status_code(req) %/% 100 == 2) {
        # Trying to read the response and see if the API answered
        # with an error message (error details). If so, that will be
        # shown, else a more generic error will be displayed.
        tmp <- tryCatch(content(req), error = function(x) NULL)
        show_http_status_and_terminate(status_code(req), tmp)
    }

    # Else extracting the content
    content <- content(req)

    # If a certain class is expected check and throw a warning
    # if the object is not of the correct class.
    if (!is.null(expected_class)) {
        if (!inherits(content, expected_class)) {
            warning("expected returned object from HTTP request to be of class ",
                    paste(expected_class, collapse = ", "), " but it is of ",
                    paste(class(content), collapse = ", "))
        }
    }

    return(content)
}


# Helper function to check that the user input for 'start' and 'end'
# is correct (convertable and end > start)
#
# Returns a list with $start and $end if no errors are found.
check_start_end_date <- function(start, end, format) {
    stopifnot("argument 'start' of wrong type" = inherits(start, c("character", "Date", "POSIXt")), length(start) == 1L)
    stopifnot("argument 'end' of wrong type"   = inherits(end, c("character", "Date", "POSIXt")), length(end) == 1L)

    stopifnot(inherits(format, c("NULL", "character")), is.null(format) || length(format) == 1L)
    if (is.character(start))
       start <- if (is.null(format)) as.POSIXct(start, tz = "UTC") else as.POSIXct(start, format = format, tz = "UTC")
    start <- as.POSIXct(start, tz = "UTC") # If is Date or POSIXlt
    if (is.character(end))
       end <- if (is.null(format)) as.POSIXct(end, tz = "UTC") else as.POSIXct(end, format = format, tz = "UTC")
    end <- as.POSIXct(end, tz = "UTC") # If is Date or POSIXlt
    stopifnot("end date must be greater than start date" = end > start)

    return(list(start = start, end = end))
}
