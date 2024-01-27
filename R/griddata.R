

#' Downloading Gridded Data
#'
#' Accessing the API endpoint \code{v1/grid},
#' see <https://dataset.api.hub.geosphere.at/v1/docs/getting-started.html>.
#'
#' @param mode character, specify mode of data.
#' @param resource_id character, specify resource identifier of data.
#' @param parameters character vector to define which parameters to process.
#' @param start,end object of class \code{Date}, \code{POSIXt}, or \code{character}.
#'        In case of character in a non-ISO format \code{format} can be used (see below).
#'        Not needed (ignored) when \code{mode = "current"}.
#' @param what either path to a netcdf file or an object which has a \code{st_bbox()}
#'        method; defines the spatial extent to be downloaded and the object returned
#'        (see section 'What to download/return').
#' @param expert logical, defaults to \code{FALSE}. If \code{TRUE} the script will not
#'        check if the input arguments are valid. May result in unsuccessful requests
#'        but increases the speed as \code{gs_datasets()} and \code{gs_metadata()}
#'        do not have to be called (two API requests less).
#' @param overwrite logical, if \code{FALSE} and \code{what} is a file an error will
#'        be thrown if the file exists. Can be set to \code{TRUE}, in this case
#'        the output file will be overwritten if existing.
#' @param verbose logical, if set \code{TRUE} some more output will be produced.
#' @param format \code{NULL} (default) or character string, used if \code{start}/\code{end}
#'        are characters in a specific (non ISO) format.
#' @param limit integer, API data request limit. If the request sent by the user
#'        exceeds this limit, the request will be split into batches automatically.
#'        Set to 2e5 as the limit stated on the API documentation (1e6) will not be accepted.
#' @param config empty list by default; can be a named list to be fowrarded
#'        to the \code{httr::GET} request if needed.
#'
#' @details This function is a convenience function for downloading different
#' sets of gridded data from the GeoSphere Austria data hub (formerly ZAMG).
#' The API may change and additional resources may be added, for details see
#' <https://dataset.api.hub.geosphere.at/v1/docs/user-guide/endpoints.html>.
#'
#' To see what's available call \code{gs_datasets("grid")}.
#'
#' The API has a limit for the number of elements for one request. The calculation
#' is based on the number of expecte elements (i.e., number of stations times number
#' of parameters times number of time steps). This function will pre-calculate the number
#' of expected elements and split the request into batches along the time dimension - if needed.
#'
#' @section What to download/return:
#'
#' TODO: Explain.
#' The plan:
#' \itemize{
#'      \item if st_bbox -> download grid; return stars
#'      \item if st data.frame -> points? -> extract
#'      \item if character -> file or path. check if directory exists
#'              and save netcdf there.
#'      
#' }
#'
#' @return TODO
#'
#' @examples
#' ######################################################################
#' ## TODO: Write some examples (most dontrun)
#'
#' @author Reto Stauffer
#' @export
#' @importFrom utils head tail
#' @importFrom httr GET write_disk status_code
#' @importFrom sf st_point
#' @importFrom parsedate parse_iso_8601
gs_gridded <- function(mode, resource_id, parameters = NULL, start = NULL,
                        end = NULL, what = NULL, expert = FALSE,
                        overwrite = FALSE, verbose = FALSE, format = NULL,
                        limit = 2e5, config = list()) {

    # Requires NetCDF4 to be installed
    requireNamespace("ncdf4")
    requireNamespace("jsonlite")

    stopifnot("argument 'mode' must be character of length 1" =
              is.character(mode) & length(mode) == 1L)
    stopifnot("argument 'resource_id' must be character of length 1" =
              is.character(resource_id) & length(resource_id) == 1L)
    stopifnot("argument 'parameters' must be NULL or a character vector of length > 0" =
              is.null(parameters) || (is.character(parameters) & length(parameters) > 0))

    stopifnot("argument expert must be logical TRUE or FALSE"  =
              isTRUE(expert)  || isFALSE(expert))
    stopifnot("argument overwrite must be logical TRUE or FALSE" =
              isTRUE(overwrite) || isFALSE(overwrite))
    stopifnot("argument verbose must be logical TRUE or FALSE" =
              isTRUE(verbose) || isFALSE(verbose))

    # Checking argument 'what'
    if (is.character(what) && length(what) == 1L) {
        if (dirname(what) != "." && !dir.exists(dirname(what)))
            stop(sprintf("what = \"%s\" requires directory \"%s\" to exist (not existing)!",
                 what, dirname(what)))
        # If overwrite = FALSE we will exit if the file already exists.
        if (!overwrite && file.exists(what))
            stop(sprintf("file \"%s\" already exists; not allowed to overwrite (overwrite = FALSE)", what))

        # Verbose
        if (verbose) message("Data will be stored to \"", what, "\".", sep = "")
    } else {
        stop("check if has st_bbox")
    }


    # Matching 'mode'.
    match_mode <- function(mode, known = c("forecast", "historical")) {
        res <- tryCatch(match.arg(mode, known), error = function(x) FALSE)
        if (isFALSE(res)) match.arg(mode, gs_datasets(type = "grid")$mode) else res
    }
    mode <- match_mode(mode)

    # Getting available dataset dynamically
    if (expert && !is.null(parameters)) {
        # Manually create URL for API endpoint
        dataset <- list(url = paste(gs_baseurl(), "grid", mode, resource_id, sep = "/"))
        meta    <- NULL # required for get grid size
    } else {
        # Check if the combination is valid and what the URL is
        dataset <- gs_datasets(mode = mode, type = "grid")

        # Sanity checks
        mode        <- match.arg(mode, unique(dataset$mode))
        resource_id <- match.arg(resource_id, unique(dataset$resource_id))

        # Checking available resource ids. Enforcing one of the types defined below
        idx <- which(dataset$mode == mode & dataset$resource_id == resource_id)
        if (!length(idx) == 1)
            stop("Could not find data set for grid with `mode = \"", mode,
                 "\"` and `resource_id = \"", resource_id, "\" `")
        dataset <- as.list(dataset[idx, ])

        # Loading meta data
        meta <- gs_metadata(mode, resource_id, "grid")

        # Checking parameters argument
        if (is.null(parameters))
            parameters <- meta$parameters$name

        idx <- which(!parameters %in% unique(meta$parameters$name))
        if (length(idx) > 0)
            stop(sprintf("Parameter%s ", ifelse(length(idx) > 1, "s", "")),
                 paste(parameters[idx], collapse = ", "), " do do not exist.\n",
                 sprintf("Check `gs_metadata(\"%s\", \"%s\", \"%s\")$parameters`",
                         mode, resource_id, "grid"),
                 " to get a list of all availalbe parameters for this data set.")
    }


    # Forcing start/end date to POSIXt/Date
    # If mode == "current" end will be set to the current date,
    # start to current date - 1 day ignoring user inputs.
    if (mode == "historical") {
        tmp   <- check_start_end_date(start, end, format)
        start <- tmp$start; end <- tmp$end
    } else if (mode == "forecast") {
        if ((!is.null(start) | !is.null(end)) & verbose)
            message("Argument 'start' and 'end' will be ignored for mode = '", mode, "'", sep = "")
        start <- Sys.Date() - 1; end <- Sys.Date()
    } else {
        stop("Whoops, no rule to process mode == \"", mode, "\".", sep = "")
    }

    # File where to store the netcdf file. If `what` is character,
    # that is the name of the file to be written. All we will return
    # at the end is that file name - the user then needs to process
    # it himself/herself.
    # Else we use a temporary file and use st_extract to get the required
    # data before deleting the temporary NetCDF file again.
    ncfile <- if (is.character(what)) what else tempfile(fileext = ".nc")

    q <- list(parameters    = parameters,
              start         = start,
              end           = end,
              # Limits according to API doc, slightly too big on some end
              # bbox          = "45.77,7.1,49.48,17.74", # SWNE
              bbox          = "46.00,8.0,49.0,17.0", # SWNE
              output_format = "netcdf")

    req <- GET(dataset$url, write_disk(ncfile, overwrite = overwrite), query = q)
    if (status_code(req) %/% 100 != 2) {
        err <- jsonlite::read_json(ncfile)
        unlink(ncfile)
        show_http_status_and_terminate(status_code(req), err)
    }

    if (is.character(what)) {
        return(what)
    } else {
        cat("processing data ...\n")
    }

}



