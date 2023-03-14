


#' Getting API Base URL
#'
#' Just returns the base URL for API requests
#'
#' @param version integer, defaults to 1.
#' @return String, base URL for the API.
#'
#' @author Reto Stauffer
gs_baseurl <- function(version = 1L) {
    stopifnot("version must be numeric" = is.numeric(version),
              "version must have length 1" = length(version) == 1)
    version <- as.integer(version)
    stopifnot("version must be larger or equal to 1" = version >= 1)

    sprintf("https://dataset.api.hub.zamg.ac.at/v%d", version)
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

