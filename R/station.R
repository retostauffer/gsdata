

#' Downloading Station Data
#'
#' Accessing the API endpoint \code{v<version>/station},
#' see <https://dataset.api.hub.zamg.ac.at/v1/docs/quickstart.html>.
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
#' @param limitcheck logical, defaults to \code{TRUE}. Will guess the number of elements
#'        to be retrieved and stops if this exceeds the API limits; see 'Details'.
#' @param version integer, API version (defaults to \code{1L}).
#' @param format \code{NULL} (default) or character string, used if \code{start}/\code{end}
#'        are characters in a specific (non ISO) format.
#' @param verbose logical, if set \code{TRUE} some more output will be produced.
#'
#' @details This function is a convenience function for downloading different sets of
#' station data from the ZAMG data hub (now Geosphere). The API may change and additional
#' resources may be added, for details see <https://dataset.api.hub.zamg.ac.at/v1/docs/daten.html#available-datasets>.
#'
#' To see what's available call \code{gs_datasets("station")}.
#'
#' The flag \code{limitcheck} guesses the number of elements to be retrieved as the API has a
#' limit set. Trying to avoid to send a too large request and only waiting for the API to fail
#' after a decent amount of time (limits exceeded). See
#' <https://dataset.api.hub.zamg.ac.at/v1/docs/daten.html#limitationen-beim-datendownload>.
#'
#' @return If only data for one single station (\code{length(station_ids) == 1}) is requested,
#' a \code{zoo} object will be returned if data is available. If no data is available,
#' \code{NULL} will be returned.
#'
#' When multiple stations are requested a list of \code{zoo} object (or \code{NULL} if no data
#' is available) is returned. The name of the list corresponds to the station id requested.
#'
#' @examples
#' ######################################################################
#' ## Example for synop data
#' 
#' ## Loading meta information
#' meta <- gs_metadata(mode = "historical", resource_id = "synop-v1-1h")
#' ## For station information check
#' head(meta$stations)
#' ## For available parameters (for this mode/resource_id) check
#' head(meta$parameters)
#' 
#' ## Getting data over 48 hours for one single station
#' ## Note: If expert = FALSE (default) gs_stationdata()
#' ## will internally call gs_metadata() once more to check
#' ## if the requested station_ids as well as the parameters
#' ## exist for the data set specified (mode/resource_id).
#' mayrhofen <- gs_stationdata(mode        = "historical",
#'                             resource_id = "synop-v1-1h",
#'                             start       = "2020-01-01",
#'                             end         = "2020-01-03",
#'                             parameters  = c("T", "Td", "ff"),
#'                             station_ids = 11330,
#'                             expert      = TRUE)
#'
#' library("zoo")
#' plot(mayrhofen, screen = c(1, 1, 2), col = c(2, 3, 4))
#'
#' ## Getting data over 48 hours for three stations simultanously
#' ## Mayrhofen Tirol, Achenkirch Tirol (no data), and Innsbruck Airport Tirol
#' x <- gs_stationdata(mode        = "historical",
#'                     resource_id = "synop-v1-1h",
#'                     start       = "2020-01-01",
#'                     end         = "2020-01-03",
#'                     parameters  = c("T", "Td", "ff"),
#'                     station_ids = c(11330, 11328, 11120),
#'                     expert      = TRUE)
#' plot(x[["11330"]], screen = c(1, 1, 2), col = c(2, 3, 4))
#' is.null(x[["11328"]])
#' plot(x[["11120"]], screen = c(1, 1, 2), col = c(2, 3, 4))
#'
#' ######################################################################
#' ## Example for 10min TAWES data
#' # meta$parameter contains available parameters,
#' # meta$stations  available stations
#' meta <- gs_metadata("historical", "tawes-v1-10min")
#' uibk <- gs_stationdata(mode = "current",
#'                resource_id = "tawes-v1-10min",
#'                start = "2023-01-01",
#'                end   = "2023-02-01",
#'                parameters = c("TL", "TP", "FFAM", "FFX"),
#'                station_ids = 11121,
#'                expert      = TRUE)
#' plot(uibk,
#'      screens = c(1, 1, 2, 2),
#'      col = c(2, 3, 4, 8),
#'      ylab = c("temperature\nand dewpoint", "mean wind\nand gusts"))
#' 
#'
#' ######################################################################
#' ## Example for annual histalp data
#' gs_metadata("historical", "histalp-v1-1y")
#' bregenz <- gs_stationdata(mode        = "historical",
#'                           resource_id = "histalp-v1-1y",
#'                           start       = "1854-01-01",
#'                           end         = "2022-01-01",
#'                           parameters  = c("R01", "T01"),
#'                           station_ids = 23)
#' plot(bregenz, col = c(4, 2))
#'
#' ######################################################################
#' ## Example for daily climatological records
#' meta <- gs_metadata("historical", "klima-v1-1d")
#' achenkirch <- gs_stationdata(mode        = "historical",
#'                              resource_id = "klima-v1-1d",
#'                              start       = "2020-06-01",
#'                              end         = "2022-12-31",
#'                              parameters  = c("nied", "nied07", "nied19", "sonne"),
#'                              station_ids = 8807,
#'                              expert      = TRUE)
#' head(achenkirch)
#' plot(achenkirch, type = "h")
#'
#' @author Reto Stauffer
#' @export
#' @importFrom sf st_point
#' @importFrom httr GET status_code content
#' @importFrom zoo zoo
gs_stationdata <- function(mode, resource_id, parameters, start, end, station_ids, expert = FALSE,
                           limitcheck = TRUE, version = 1L, format = NULL, verbose = FALSE) {

    stopifnot("argument expert must be logical TRUE or FALSE" = isTRUE(expert) || isFALSE(expert))
    stopifnot("argument verbose must be logical TRUE or FALSE" = isTRUE(verbose) || isFALSE(verbose))
    stopifnot("argument limitcheck must be logical TRUE or FALSE" = isTRUE(limitcheck) || isFALSE(limitcheck))

    # Getting available dataset dynamically
    if (expert) {
        stopifnot(is.character(mode),        length(mode) == 1L)
        stopifnot(is.character(resource_id), length(mode) == 1L)
        # Manually create URL for API endpoint
        dataset <- list(url = paste(gs_baseurl(), "station", mode, resource_id, sep = "/"))
    } else {
        # Check if the combination is valid and what the URL is
        dataset <- gs_datasets("station", version)

        # Sanity checks
        mode        <- match.arg(mode, unique(dataset$mode))
        resource_id <- match.arg(resource_id, unique(dataset$resource_id))

        # Checking available resource ids. Enforcing one of the types defined below
        idx <- which(dataset$mode == mode & dataset$resource_id == resource_id)
        if (!length(idx) == 1)
            stop("Could not find data set for station with `mode = \"", mode, "\"` and `resource_id = \"", resource_id, "\" `")
        dataset <- as.list(dataset[idx, ])

        # Loading meta data
        meta <- gs_metadata(mode, resource_id, version)

        # Checking parameters argument
        stopifnot(is.character(parameters), length(parameters) >= 1)
        idx <- which(!parameters %in% unique(meta$parameters$name))
        if (length(idx) > 0)
            stop(sprintf("Parameter%s ", ifelse(length(idx) > 1, "s", "")),
                 paste(parameters[idx], collapse = ", "), " do do not exist.\n",
                 sprintf("Check `gs_metadata(\"%s\", \"%s\", version = %d)$parameters`",
                         mode, resource_id, version),
                 " to get a list of all availalbe parameters for this data set.")

        # Checking stations argument
        stopifnot(is.numeric(station_ids),  length(station_ids) >= 1)
        station_ids <- as.integer(station_ids)
        idx <- which(!station_ids %in% unique(meta$stations$id))
        if (length(idx) > 0)
            stop(sprintf("Stations%s ", ifelse(length(idx) > 1, "s", "")),
                 paste(station_ids[idx], collapse = ", "), " do do not exist.\n",
                 sprintf("Check `gs_metadata(\"%s\", \"%s\", version = %d)$stations`",
                         mode, resource_id, version),
                 " to get a list of all availalbe stations for this data set.")

    }

    # Forcing start/end date to POSIXt/Date
    stopifnot(inherits(start, c("character", "Date", "POSIXt")), length(start) == 1L)
    stopifnot(inherits(end, c("character", "Date", "POSIXt")), length(end) == 1L)
    stopifnot(inherits(format, c("NULL", "character")), is.null(format) || length(format) == 1L)
    if (is.character(start))
       start <- if (is.null(format)) as.POSIXct(start) else as.POSIXct(start, format = format)
    if (is.character(end))
       end <- if (is.null(format)) as.POSIXct(end) else as.POSIXct(end, format = format)

    # Guessing number of elements to be retrieved and checking against the current
    # limits of the API
    if (limitcheck) {
        nsecs  <- gs_temporal_interval(resource_id)
        nsteps <- ceiling(as.double(end - start, units = "secs") / nsecs)
        nguess <- length(station_ids) * nsteps * length(parameters)
        if (verbose) message("Guessing number of elements to be retrieved\n",
                             sprintf(" %d stations * %d timesteps (%d second interval) * %d parameter = %d",
                                     length(station_ids), nsteps, nsecs, length(parameters), nguess))
        if (tolower(format) == "netcdf" && nguess > 1e7) {
            stop("Number of estimated data points/elements to be retrieved exceeds the limit of 1e7 (NetCDF format). Not sending request.")
        } else if (nguess >= 1e6) {
            stop("Number of estimated data points/elements to be retrieved exceeds the limit of 1e6. Not sending request.")
        }
    }

    # Getting data
    query <- list(parameters  = paste(parameters, collapse = ","),
                  start       = format(start, "%Y-%m-%dT%H:%M"),
                  end         = format(end, "%Y-%m-%dT%H:%M"),
                  station_ids = paste(station_ids, collapse = ","))

    if (verbose) {
        tmp <- lapply(query, function(x) paste(as.character(x), collapse = ","))
        tmp <- paste(names(tmp), unname(tmp), sep = "=", collapse = "&")
        message("Full call: ", dataset$url, "?", tmp, "\n", sep = "")
    }
    req <- GET(dataset$url, query = query)
    if (!status_code(req) == 200) {
        tmp <- content(req)
        if (is.list(tmp) && "message" %in% names(tmp) && grepl("timing out", tmp$message)) {
            stop("Request most likely too big. Server responded with: ", tmp$message)
        } else {
            stop(tmp, "\nData request not successful (status code != 200)")
        }
    }

    # Evaluate content
    content <- content(req)
    N     <- length(content$timestamps)
    index <- as.POSIXct(unlist(content$timestamps), format = "%Y-%m-%dT%H:%M")

    # Extracting data (lists)
    final <- list()
    for (rec in content$features) {
        stn <- rec$properties$station
        tmp <- lapply(rec$properties$parameters, function(x) sapply(x$data, function(y) ifelse(is.null(y), NA_real_, y)))
        tmp <- tmp[sapply(tmp, function(x) sum(!is.na(x))) > 0]
        final[[stn]] <- c(final[[stn]], tmp)
    }

    # convert to list of zoo objects.
    # If there is only one station (length(stations_id) == 1)
    # we only return a zoo, else list of zoo where the name of the list
    # elements itself is the number of the station.
    fn <- function(x, index) {
        idx <- which(!sapply(x, is.null))
        if (length(idx) == 0) {
            warning("No observations provided for one station; returning NULL")
            res <- NULL
        } else {
            res <- zoo(as.data.frame(x[idx]), unname(index))
        }
        return(res)
    }
    final <- lapply(final, fn, index = index)

    # Extracting parameter info for attribute
    get_param <- function(x) {
        tmp <- lapply(x$properties$parameters, function(x) data.frame(name = x$name, unit = x$unit))
        do.call(rbind, tmp)
    }
    for (i in seq_along(final)) {
	if (is.null(final[[i]])) next ## no data available
        coord <- st_point(unlist(content$features[[i]]$geometry$coordinates))
        id    <- as.integer(content$features[[i]]$properties$station)
        attr(final[[i]], "station") <- list(id = id, coord = coord)
        attr(final[[i]], "parameter") <- get_param(content$features[[i]])
        class(final[[i]]) <- c("gs_stationdata", class(final[[i]]))
    }
    return(if (length(station_ids) == 1) final[[1]] else final)
}



