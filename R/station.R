

#' Downloading Station Data
#'
#' Accessing the API endpoint \code{v<version>/station},
#' see \code{\url{https://dataset.api.hub.zamg.ac.at/v1/docs/quickstart.html}}.
#'
#' @param mode character, specify mode of data.
#' @param resource_id character, specify resource identifier of data.
#' @param parameters character vector to define which parameters to process.
#' @param start,end object of class \code{Date}, \code{POSIXt}, or \code{character}.
#'        In case of character in a non-ISO format \code{format} can be used (see below).
#' @param station_ids integer vector with the station IDs to be processed.
#' @param expert logical, defaults to \code{FALSE}. If \code{TRUE} the script will not
#'        check if the input arguments are valid. May result in unsuccessful requests
#'        but increases the speed as \code{gs_datasets()} and \code{gs_metadata()}
#'        do not have to be evaluated.
#' @param version integer, API version (defaults to \code{1L}).
#' @param format \code{NULL} (default) or character string, used if \code{start}/\code{end}
#'        are characters in a specific (non ISO) format.
#'
#'
#' @details This function is a convenience function for downloading different sets of
#' station data from the ZAMG data hub (now Geosphere). The API may change and additional
#' resources may be added, for details see \code{\url{https://dataset.api.hub.zamg.ac.at/v1/docs/daten.html#available-datasets}}.
#'
#' To see what's available call \code{subset(gs_datasets(), type == "station")}.
#'
#' @author Reto Stauffer
#' @export
#' @importFrom httr GET status_code content
#' @importFrom zoo zoo
gs_stationdata <- function(mode, resource_id, parameters, start, end, station_ids, expert = FALSE, version = 1L, format = NULL) {

    stopifnot("argument expert must be logical TRUE or FALSE" = isTRUE(expert) || isFALSE(expert))

    # Getting available dataset dynamically
    if (expert) {
        stopifnot(is.character(mode),        length(mode) == 1L)
        stopifnot(is.character(resource_id), length(mode) == 1L)
        # Manually create URL for API endpoint
        dataset <- list(url = paste(gs_baseurl(), "station", mode, resource_id, sep = "/"))
    } else {
        # Check if the combination is valid and what the URL is
        dataset <- gs_datasets(version)
        dataset <- subset(dataset, type == "station")

        # Sanity checks
        mode        <- match.arg(mode, unique(dataset$mode))
        resource_id <- match.arg(resource_id, unique(dataset$resource_id))

        # Checking available resource ids. Enforcing one of the types defined below
        idx <- which(dataset$mode == mode & dataset$resource_id == resource_id)
        if (!length(idx) == 1)
            stop("Could not find data set for station with mode = ", mode, "and resource_id =", resource_id)
        dataset <- as.list(dataset[idx, ])

        # Loading meta data
        meta <- gs_metadata(mode, resource_id, version)

        # Checking parameters argument
        stopifnot(is.character(parameters), length(parameters) >= 1)
        idx <- which(!parameters %in% unique(meta$parameters$name))
        if (length(idx) > 0)
            stop("parameter(s) ", paste(parameters[idx], collapse = ", "), " not allowed. Available are:",
                 paste(unique(meta$parameters$name), collapse = ", "))

        # Checking stations argument
        stopifnot(is.numeric(station_ids),  length(station_ids) >= 1)
        station_ids <- as.integer(station_ids)
        idx <- which(!station_ids %in% unique(meta$stations$id))
        if (length(idx) > 0)
            stop("stations(s) with the ids ", paste(station_ids[idx], collapse = ", "), " do not exist")

    }

    # Forcing start/end date to POSIXt/Date
    stopifnot(inherits(start, c("character", "Date", "POSIXt")), length(start) == 1L)
    stopifnot(inherits(end, c("character", "Date", "POSIXt")), length(end) == 1L)
    stopifnot(inherits(format, c("NULL", "character")), is.null(format) || length(format) == 1L)
    if (is.character(start))
       start <- if (is.null(format)) as.POSIXct(start) else as.POSIXct(start, format = format)
    if (is.character(end))
       end <- if (is.null(format)) as.POSIXct(end) else as.POSIXct(end, format = format)


    # Getting data
    query <- list(parameters  = paste(parameters, collapse = ","),
                  start       = format(start, "%Y-%m-%dT%H:%M"),
                  end         = format(end, "%Y-%m-%dT%H:%M"),
                  station_ids = paste(station_ids, collapse = ","))
    req <- GET(dataset$url, query = query)
    if (!status_code(req) == 200) stop("data request not successful (status code != 200)")

    # Evaluate content
    content <- content(req)
    N     <- length(content$timestamps)
    index <- as.POSIXct(unlist(content$timestamps), format = "%Y-%m-%dT%H:%M")

    # Extracting data (lists)
    final <- list()
    for (rec in content$features) {
        stn <- rec$properties$station
        tmp <- lapply(rec$properties$parameters, function(x) unlist(x$data))
        final[[stn]] <- c(final[[stn]], tmp)
    }

    # convert to list of zoo objects.
    # If there is only one station (length(stations_id) == 1)
    # we only return a zoo, else list of zoo where the name of the list
    # elements itself is the number of the station.
    fn <- function(x, index) zoo(as.data.frame(x), unname(index))
    final <- lapply(final, fn, index = index)
    return(if (length(station_ids) == 1) final[[1]] else final)
}



