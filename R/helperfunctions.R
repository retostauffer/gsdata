


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

