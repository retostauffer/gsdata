
#' Downloading Dataset Meta Data
#'
#' @param mode character, specify mode of data.
#' @param resource_id character, specify resource identifier of data.
#' @param type \code{NULL} or character. Only required if a data set
#'        is available in more than one type (e.g., \"grid\" and \"timeseries\").
#' @param config empty list by default; can be a named list to be fowrarded
#'        to the \code{httr::GET} request if needed.
#' @param expert logical, defaults to \code{FALSE}. If \code{TRUE} the script will not
#'        check if the input arguments are valid. May result in unsuccessful requests
#'        but increases the speed as \code{gs_datasets()} does not have to be
#'        called (one less API request).
#' @param verbose logical, if set \code{TRUE} some more output will be produced.
#'
#' @return Named list with a series of information about the dataset.
#' Most importantly this function returns information about the stations
#' for which data is available as well as what parameters are available.
#' Note that the availability of data depends on the station; the meta information
#' only provides an overview of what is possibliy avialable.
#'
#' * \code{$stations}: an \code{sf} object (spatial feature data frame) containing information
#'      of all stations belonging to this dataset including geographical location,
#'      name, and \code{id} (the station identifier) wihch is used when retrieving
#'      data (see e.g., \code{gs_stationdata()}).
#' * \code{$parameters}: a \code{data.frame} containing the \code{name} of the parameters
#'      used when retrieving the data (see e.g., \code{gs_stationdata()}) as well as a 
#'      parameter and description. Only available in German, tough.
#'
#' In addition, the following information will be returned as separate entries in the list:
#'
#' * \code{$title}/\code{$id_type}: title/id type of the dataset
#' * \code{$frequency}: observation frequency/temporal interval
#'                      (see also \code{gs_temporal_interval()})
#' * \code{$type}: data type (e.g., "station")
#' * \code{$mode}: data set mode (e.g., "historical")
#' * \code{$response_formats}: formats the API provides
#' * \code{$start_time}/\code{$end_time}: date/time range of availability of this data set
#' * \code{$url}: URL; origin of the data set
#'
#' @examples
#' ## Loading meta information for data set with
#' ## mode == "historical" and resource_id = "tawes-v1-10min"
#' tawes <- gs_metadata("historical", "tawes-v1-10min")
#'
#' ## Uses partial matching, thus this short form can be used in case
#' ## there is only one match (one specific data set). With verbose = TRUE
#' ## a message will tell which meta data set will be requested.
#' synop <-  gs_metadata("hist", "synop", verbose = TRUE)
#'
#' ## generic sf plotting; variable 'altitude'
#' plot(synop$stations["altitude"], pch = 19, cex = 2)
#'
#' @author Reto Stauffer
#' @export
#' @importFrom sf st_as_sf
#' @importFrom parsedate parse_iso_8601
gs_metadata <- function(mode, resource_id, type = NULL, config = list(), expert = FALSE, verbose = FALSE) {

    # Getting available dataset dynamically; used to perform 'sanity check'
    # (whether or not the defined mode/resource_id is a valid identifier)
    stopifnot("argument 'mode' must be character of length 1" =
              is.character(mode) && length(mode) == 1)
    stopifnot("argument 'resource_id' must be character of length 1" =
              is.character(resource_id) && length(resource_id) == 1)
    stopifnot("argument 'type' must be NULL or character of length 1" =
              is.null(type) || (is.character(type) && length(type) == 1L))
    stopifnot("argument 'expert' must be logical TRUE or FALSE" =
              isTRUE(expert) || isFALSE(expert))
    stopifnot("argument 'verbose' must be logical TRUE or FALSE" =
              isTRUE(verbose) || isFALSE(verbose))

    # Check if mode/resource_id is a valid combination
    if (!expert) {
        dataset     <- gs_datasets()
        mode        <- match.arg(mode, unique(dataset$mode))
        resource_id <- match.arg(resource_id, unique(dataset$resource_id))
        if (!is.null(type)) type <- match.arg(type, unique(dataset$type))

        # Checking available resource ids. Enforcing one of the types defined below
        dataset <- dataset[dataset$mode == mode & dataset$resource_id == resource_id, ]
        if (!is.null(type)) dataset <- dataset[dataset$type == type, ]

        # More than one match?
        if (nrow(dataset) > 1L && is.null(type)) {
            stop("Found ", nrow(dataset), " datasets matching the input arguments.",
                 "\n  Please specify the 'type' argument (",
                 paste(sprintf("type = \"%s\"", dataset$type), collapse = " or "), ")")
        }
        if (nrow(dataset) == 0) {
            stop("Could not find data set for station with mode = \"", mode, "\" ",
                 "and resource_id = \"", resource_id, "\" and type = \"", type, "\"", sep = "")
        } else if (nrow(dataset) > 1) {
            stop("Multiple matches; should not happen!")
        }
        dataset <- as.list(dataset)
    } else {
        # Ensure mode/resource_id are characters of length 1
        stopifnot("argument `mode` must be character length 1" = 
                  is.character(mode) && length(mode) == 1)
        stopifnot("argument `resource_id` must be character length 1" = 
                  is.character(resource_id) && length(resource_id) == 1)
        # Guess
        dataset <- list(url = paste(gs_baseurl(), "station", mode, resource_id, sep = "/"))
    }

    # Verbosity
    if (verbose)
        message("Requesting data for mode = \"", mode,
                "\" and resource_id = \"", resource_id, "\"", sep = "")

    # Downloading meta data
    res <- API_GET(paste(dataset$url, "metadata", sep = "/"),
                   config = config, query = NULL, verbose = verbose)

    # Prepare metadata (differs between types)
    prep_fun <- get(sprintf("prep_metadata_%s", type))
    res      <- tryCatch(prep_fun(res),
                         error = function(e) stop(sprintf("function prep_metadata_%s not defined", type)))

    return(res)
}


prep_metadata_station <- function(x) {
    x$parameters <- do.call(rbind, lapply(x$parameters, as.data.frame))
    x$stations   <- do.call(rbind, lapply(x$stations,
        function(x) as.data.frame(lapply(x, function(x) ifelse(is.null(x), NA, x)))))

    # Double-check if my format specification below is OK
    stopifnot("unexpected time format" = all(grepl("\\+00:00$", x$stations$valid_from)))
    stopifnot("unexpected time format" = all(grepl("\\+00:00$", x$stations$valid_to)))
    fmt <- "%Y-%m-%dT%H:%M+00:00"
    x$stations <- transform(x$stations,
                              id         = as.integer(id),
                              valid_from = as.POSIXct(valid_from, tz = "UTC", format = fmt),
                              valid_to   = as.POSIXct(valid_to,   tz = "UTC", format = fmt))

    # Convert to sf data.frame
    x$stations <- st_as_sf(x$stations, coords = c("lon", "lat"), crs = 4326)
    return(x)
}


#' @importFrom sf st_bbox st_crs
prep_metadata_grid <- function(x) {
    x$parameters <- do.call(rbind, lapply(x$parameters, as.data.frame))

    stopifnot("unexpected time format" = all(grepl("\\+00:00$", x$start_time)))
    stopifnot("unexpected time format" = all(grepl("\\+00:00$", x$end_time)))
    fmt <- "%Y-%m-%dT%H:%M+00:00"
    x$start_time <- as.POSIXct(x$start_time, tz = "UTC", format = fmt)
    x$end_time   <- as.POSIXct(x$end_time,   tz = "UTC", format = fmt)

    # Convert CRS
    x$crs <- st_crs(x$crs)

    # Create st_bbox for 'bbox', 'bbox_outer' and native grid 'grid_bounds'
    x$bbox       <- st_bbox(c(xmin = x$bbox[[2]], xmax = x$bbox[[4]],
                              ymin = x$bbox[[1]], ymax = x$bbox[[3]]),
                            crs = st_crs(4326))

    x$bbox_outer <- st_bbox(c(xmin = x$bbox_outer[[2]], xmax = x$bbox_outer[[4]],
                              ymin = x$bbox_outer[[1]], ymax = x$bbox_outer[[3]]),
                            crs = st_crs(4326))

    x$grid_bounds <- st_bbox(c(xmin = x$grid_bounds[[2]], xmax = x$grid_bounds[[4]],
                               ymin = x$grid_bounds[[1]], ymax = x$grid_bounds[[3]]),
                             crs = x$crs)
    return(x)
}
