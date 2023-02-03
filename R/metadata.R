
#' Downloading Dataset Meta Data
#'
#' @param mode character, specify mode of data.
#' @param resource_id character, specify resource identifier of data.
#' @param version integer, API version (defaults to \code{1L}).
#'
#' @return Named list with a series of information about the dataset
#' including the URL called, available parameters with description and unit,
#' available stations with unit etc.
#'
#' @author Reto Stauffer
#' @export
#' @importFrom httr GET status_code content
#' @importFrom sf st_as_sf
gs_metadata <- function(mode, resource_id, version = 1L) {

    # Getting available dataset dynamically
    dataset <- gs_datasets(version)

    # Sanity checks
    mode <- match.arg(mode, unique(dataset$mode))
    resource_id <- match.arg(resource_id, unique(dataset$resource_id))

    # Checking available resource ids. Enforcing one of the types defined below
    idx <- which(dataset$mode == mode & dataset$resource_id == resource_id)
    if (!length(idx) == 1)
        stop("Could not find data set for station with mode = ", mode, "and resource_id =", resource_id)
    dataset <- as.list(dataset[idx, ])

    # Downloading meta data
    URL <- paste(dataset$url, "metadata", sep = "/")
    req <- GET(URL)
    if (!status_code(req) == 200) stop("problems downloading meta data from ", URL)
    res <- content(req)

    # Evaluate result
    res$parameters <- do.call(rbind, lapply(res$parameters, as.data.frame))
    res$stations   <- do.call(rbind, lapply(res$stations, function(x) as.data.frame(lapply(x, function(x) ifelse(is.null(x), NA, x)))))
    # Double-check if my format specification below is OK
    stopifnot("unexpected time format" = all(grepl("\\+00:00$", res$stations$valid_from)))
    stopifnot("unexpected time format" = all(grepl("\\+00:00$", res$stations$valid_to)))
    res$stations <- transform(res$stations,
                              id         = as.integer(id),
                              valid_from = as.POSIXct(valid_from, format = "%Y-%m-%dT%H:%M+00:00"),
                              valid_to   = as.POSIXct(valid_to,   format = "%Y-%m-%dT%H:%M+00:00"))
    res$stations <- st_as_sf(res$stations, coords = c("lon", "lat"), crs = 4326)
    res$url <- URL
    return(res)

}
