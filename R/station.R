

#' Downloading Station Data
#'
#' Accessing the API endpoint \code{v<version>/station},
#' see <https://dataset.api.hub.geosphere.at/v1/docs/getting-started.html>.
#'
#' @param mode character, specify mode of data.
#' @param resource_id character, specify resource identifier of data.
#' @param parameters character vector to define which parameters to process.
#' @param start,end object of class \code{Date}, \code{POSIXt}, or \code{character}.
#'        In case of character in a non-ISO format \code{format} can be used (see below).
#'        Not needed (ignored) when \code{mode = "current"}.
#' @param station_ids integer vector with the station IDs to be processed.
#' @param expert logical, defaults to \code{FALSE}. If \code{TRUE} the script will not
#'        check if the input arguments are valid. May result in unsuccessful requests
#'        but increases the speed as \code{gs_datasets()} and \code{gs_metadata()}
#'        do not have to be evaluated.
#' @param version integer, API version (defaults to \code{1L}).
#' @param drop logical, if \code{TRUE} parameters and times with no data are removed
#'        before returning the data.
#' @param verbose logical, if set \code{TRUE} some more output will be produced.
#' @param format \code{NULL} (default) or character string, used if \code{start}/\code{end}
#'        are characters in a specific (non ISO) format.
#' @param limit integer, API data request limit. If the request sent by the user
#'        exceeds this limit, the request will be split into batches automatically.
#'        Set to 2e5 as the limit stated on the API documentation (1e6) will not be accepted.
#' @param config empty list by default; can be a named list to be fowrarded
#'        to the \code{httr::GET} request if needed.
#'
#' @details This function is a convenience function for downloading different sets of
#' station data from the GeoSphere data hub (formerly ZAMG). The API may change and additional
#' resources may be added, for details see
#' <https://dataset.api.hub.geosphere.at/v1/docs/user-guide/endpoints.html>.
#'
#' To see what's available call \code{gs_datasets("station")}.
#'
#' The API has a limit for the number of elements for one request. The calculation
#' is based on the number of expecte elements (i.e., number of stations times number
#' of parameters times number of time steps). This function will pre-calculate the number
#' of expected elements and split the request into batches along the time dimension - if needed.
#' For current limits see
#' <https://dataset.api.hub.geosphere.at/v1/docs/user-guide/request_size_limit.html>.
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
#' ## Latest observations for two tawes stations in Innsbruck.
#' ## Parameters TL (air temperature 2m above ground), TS (air temperature 5cm
#' ## above ground) and RR (amount of rain past 10 minutes).
#' innsbruck <- gs_stationdata(mode        = "current",
#'                             resource_id = "tawes-v1-10min",
#'                             parameters  = c("TL", "TS", "RR"),
#'                             station_ids = c(11121, 11320),
#'                             expert      = TRUE)
#' # Air temp
#' sapply(innsbruck, function(x) x$TL)
#' # Precipitation (rain)
#' sapply(innsbruck, function(x) x$RR)
#'
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
#'                             station_ids = 11330, verbose = TRUE)
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
#'
#' ######################################################################
#' ## Example for 10min KLIMA data
#' # meta$parameter contains available parameters,
#' # meta$stations  available stations
#' meta <- gs_metadata("historical", "klima-v1-10min")
#' uibk <-  gs_stationdata(mode        = "historical",
#'                         resource_id = "klima-v1-10min",
#'                         start       = "2010-11-01",
#'                         end         = "2011-02-01",
#'                         parameters  = c("TL", "FFAM", "FFX"),
#'                         station_ids = 11803,
#'                         expert      = TRUE)
#' plot(uibk,
#'      screens = c(1, 2, 2),
#'      col = c(2, 4, 8),
#'      ylab = c("temperature", "mean wind\nand gusts"))
#' 
#'
#' ######################################################################
#' ## Example for 10min TAWES data
#' ## NOTE/WARNING:
#' ##   ! "tawes" is not quality controlled and provides limited
#' ##   ! amount of data. Consider to use the "klima-v1-10min" data set which
#' ##   ! provides long-term historical data for the same stations with the
#' ##   ! same temporal resolution, however, the station IDs and
#' ##   ! parameter names (as well as avilavle parameters) will differ
#' ##   ! (check meta data).
#' # meta$parameter contains available parameters,
#' # meta$stations  available stations
#' meta <- gs_metadata("historical", "tawes-v1-10min")
#' uibk <- gs_stationdata(mode         = "historical",
#'                        resource_id  = "tawes-v1-10min",
#'                        start        = Sys.Date() - 30,
#'                        end          = Sys.Date(),
#'                        parameters   = c("TL", "TP", "FFAM", "FFX"),
#'                        station_ids  = 11320,
#'                        expert       = TRUE)
#' plot(uibk,
#'      screens = c(1, 1, 2, 2),
#'      col = c(2, 3, 4, 8),
#'      ylab = c("temperature\nand dewpoint", "mean wind\nand gusts"))
#' 
#'
#' ######################################################################
#' ## Example for annual histalp data
#' ## Requires login; will result in an error for now (todo)
#' \dontrun{
#'     gs_metadata("historical", "histalp-v1-1y")
#'     bregenz <- gs_stationdata(mode        = "historical",
#'                               resource_id = "histalp-v1-1y",
#'                               start       = "1854-01-01",
#'                               end         = "2022-01-01",
#'                               parameters  = c("R01", "T01"),
#'                               station_ids = 23,
#'                               expert      = TRUE)
#'     plot(bregenz, col = c(4, 2))
#' }
#'
#' @author Reto Stauffer
#' @export
#' @importFrom utils head tail
#' @importFrom sf st_point
#' @importFrom parsedate parse_iso_8601
#' @importFrom httr GET status_code content
#' @importFrom zoo zoo index
gs_stationdata <- function(mode, resource_id, parameters = NULL, start = NULL,
                           end = NULL, station_ids, expert = FALSE,
                           version = 1L, drop = TRUE, verbose = FALSE, format = NULL,
                           limit = 2e5, config = list()) {

    stopifnot("argument 'mode' must be character of length 1" = is.character(mode) & length(mode) == 1L)
    stopifnot("argument 'resource_id' must be character of length 1" = is.character(resource_id) & length(resource_id) == 1L)
    stopifnot("argument 'parameters' must be NULL or a character vector of length > 0" =
              is.null(parameters) || (is.character(parameters) & length(parameters) > 0))

    stopifnot("argument expert must be logical TRUE or FALSE" = isTRUE(expert) || isFALSE(expert))
    stopifnot("argument drop must be logical TRUE or FALSE" = isTRUE(drop) || isFALSE(drop))
    stopifnot("argument verbose must be logical TRUE or FALSE" = isTRUE(verbose) || isFALSE(verbose))

    # Matching 'mode'.
    match_mode <- function(mode, known = c("current", "historical")) {
        res <- tryCatch(match.arg(mode, known), error = function(x) FALSE)
        if (isFALSE(res)) match.arg(mode, gs_datasets()$mode) else res
    }
    mode <- match_mode(mode)

    # These cause an API error and will be excluded from the get-all-parameters
    # requests and a warning will be thrown if the user specifies them manually
    parameters_to_ignore = c("corr", "rest")

    # Getting available dataset dynamically
    if (expert && !is.null(parameters)) {
        if (any(parameters %in% parameters_to_ignore)) {
            idx <- which(parameters_to_ignore %in% parameters)
            warning("WARNING: You have specified to retrieve parameter(s) ",
                    paste(sprintf("\"%s\"", parameters_to_ignore[idx]), collapse = ", "), " ",
                    "which may likely cause an error (API error; not float). ",
                    "If so, try to not retrieve either of: ",
                    paste(sprintf("\"%s\"", parameters_to_ignore), collapse = ", "))
        }
        # Manually create URL for API endpoint
        dataset <- list(url = paste(gs_baseurl(), "station", mode, resource_id, sep = "/"))
    } else {
        # Check if the combination is valid and what the URL is
        dataset <- gs_datasets(mode = mode, type = "station", version = version)

        # Sanity checks
        mode        <- match.arg(mode, unique(dataset$mode))
        resource_id <- match.arg(resource_id, unique(dataset$resource_id))

        # Checking available resource ids. Enforcing one of the types defined below
        idx <- which(dataset$mode == mode & dataset$resource_id == resource_id)
        if (!length(idx) == 1)
            stop("Could not find data set for station with `mode = \"", mode, "\"` and `resource_id = \"", resource_id, "\" `")
        dataset <- as.list(dataset[idx, ])

        # Loading meta data
        meta <- gs_metadata(mode, resource_id, version = version)

        # Checking parameters argument
        if (is.null(parameters)) parameters <- meta$parameters$name[!meta$parameters$name %in% parameters_to_ignore]
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
    # If mode == "current" end will be set to the current date,
    # start to current date - 1 day ignoring user inputs.
    if (mode == "current") {
        if ((!is.null(start) | !is.null(end)) & verbose)
            message("Argument 'start' and 'end' will be ignored for mode = '", mode, "'", sep = "")
        start <- Sys.Date() - 1
        end   <- Sys.Date()
    } else {
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
    }

    # To avoid running into the API data request limitation we calculate
    # batches along the time axis. Will return a list of start/end date and time
    # (one if only one is needed) used for the request later.
    calc_batches <- function(resource_id, station_ids, parameters, start, end, limit, verbose) {
        # Calculate number of expected data points for one single time steps
        per_step  <- length(station_ids) * (length(parameters) + 1) # +1 for time
        n_steps   <- ceiling(as.double(end - start, units = "secs") / gs_temporal_interval(resource_id)) + 1
        n_total   <- n_steps * per_step 
        if (verbose) message(sprintf("Estimated number of elements to be retrieved: %d (%d x %d x %d)",
                                     n_total, length(station_ids), length(parameters) + 1, n_steps))
        n_batches <- ceiling((n_steps * per_step) / limit)
        if (n_batches == 1) {
            res <- list(list(start = start, end = end))
        } else {
            tmp <- seq(start, end, length.out = n_batches + 1, tz = "UTC")
            res <- list()
            for (i in head(seq_len(n_batches + 1), -1)) {
                res[[paste("batch", i, sep = "_")]] <- if (i == 1) list(start = tmp[i], end = tmp[i + 1]) else list(start = tmp[i] + 60, end = tmp[i + 1])
            }
        }
        return(res)
    }

    # Calculating batches
    batches <- calc_batches(resource_id, station_ids, parameters, start, end, limit, verbose)
    if (verbose) message("Number of requests to be performed: ", length(batches), " (limit set to ", limit, ")", sep = "")

    # Extracting parameters from last batch
    get_param <- function(x) {
        tmp <- lapply(x$properties$parameters, function(x) data.frame(name = x$name, unit = x$unit))
        do.call(rbind, tmp)
    }

    # Getting data
    get_batch <- function(timeinfo) {
        query <- list(parameters  = paste(parameters, collapse = ","),
                      start       = format(timeinfo$start, "%Y-%m-%dT%H:%M"),
                      end         = format(timeinfo$end,   "%Y-%m-%dT%H:%M"),
                      station_ids = paste(station_ids, collapse = ","))
    
        if (verbose) {
            tmp <- lapply(query, function(x) paste(as.character(x), collapse = ","))
            tmp <- paste(names(tmp), unname(tmp), sep = "=", collapse = "&")
            message("Calling: ", dataset$url, "?", tmp, sep = "")
        }
        req     <- GET(dataset$url, query = query, config = config)
        content <- content(req)
        if (!status_code(req) == 200) {
            if (is.list(content) && "message" %in% names(content) && grepl("timing out", content$message)) {
                stop("Request most likely too big. Server responded with: ", content$message)
            } else {
                stop(content, "\nData request not successful (status code != 200)")
            }
        }
    
        # Extracting data (lists)
        final      <- list() # To store data
        attributes <- list() # Store additional information:w

        for (rec in content$features) {
            stn <- rec$properties$station
            tmp <- lapply(rec$properties$parameters, function(x) sapply(x$data, function(y) ifelse(is.null(y), NA_real_, y)))
            final[[stn]] <- c(final[[stn]], tmp)
            # Keep attributes
            coord <- st_point(unlist(rec$geometry$coordinates))
            id    <- as.integer(rec$properties$station)
            attributes[[stn]] <- list(station = list(id = id, coord = coord),
                                      parameter = get_param(rec))
        }
    
        # convert to list of zoo objects.
        fn <- function(x, index) {
            idx <- which(!sapply(x, is.null))
            res <- if (length(idx) == 0) NULL else zoo(as.data.frame(x[idx]), unname(index))
            # Keep attributes
            for (n in c("parameter", "station")) attr(res, n) <- attr(x, n)
            return(res)
        }
        observations <- lapply(final, fn, index = parse_iso_8601(content$timestamp))
        return(list(attributes   = attributes,
                    observations = observations))
    }
    # Function for combining the data
    ####comb <- function(s) do.call(rbind, lapply(data, function(x, s) x[[s]], s = s))
    data <- lapply(batches, get_batch)

    # Remove overlapping indices as sometimes returned by the API
    # (seems to round star/end which sometimes yields data starting
    # before the start date defined by the user/in the batch).
    fixindex <- function(x, ...) {
        if (length(x) > 1)
            for (i in seq(2L, length(x))) x[[i]] <- x[[i]][index(x[[i]]) > max(index(x[[i - 1]])), ]
        return(x)
    }

    # Combine (row-bind) data from all batches if there have been multiple
    res <- list()
    for (s in as.character(station_ids)) {
        res[[s]] <- do.call(rbind, fixindex(lapply(data, function(x, s) x$observations[[s]], s = s)))
        # Convert index to Date if all observations belong to 00:00
        if (!is.null(res[[s]])) {
            if (drop) res[[s]] <- res[[s]][rowSums(!is.na(coredata(res[[s]]))) > 0,
                                           colSums(!is.na(coredata(res[[s]]))) > 0,
                                           drop = FALSE]
            if (all(format(index(res[[s]]), "%H%M") == "0000"))
                index(res[[s]]) <- as.Date(index(res[[s]]))
            # Appending attributes; take them from the first batch;
            # must/should be the same in all batches (if there are multiple)
            for (n in names(data[[1]]$attributes[[s]]))
                attr(res[[s]], n) <- data[[1]]$attributes[[s]][[n]]
            # Appending new class
            class(res[[s]]) <- c("gs_stationdata", class(res[[s]]))
        } else {
            warning("no observations available for station ", s, "; returning NA")
            res[[s]] <- NA
        }
    }

    # Return
    return(if (length(station_ids) == 1) res[[1]] else res)
}



