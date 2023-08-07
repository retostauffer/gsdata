




#' @export
#' @exportS3Method print gs_stationdata
print.gs_stationdata <- function(x, ...) {
    stn <- attr(x, "station")
    cat("GeoSphere Austria stationdata\n")
    cat("Station ID: ", stn$id, "\nCoordinates: ", stn$coord, "\n")

    param <- attr(x, "parameter")
    cat("Parameters:", paste(sprintf("\n  %-10s %s (%s)", rownames(param), param$name, param$unit),
                               collapse = ""), "\n\n")

    attr(x, "station") <- NULL
    attr(x, "parameter") <- NULL
    NextMethod()
    attr(x, "station")   <- stn
    attr(x, "parameter") <- param
}

#' @export
#' @exportS3Method plot gs_stationdata
#' @importFrom zoo plot.zoo index
plot.gs_stationdata <- function(x, main = NULL, xlab = NULL, ...) {

    if (is.null(main)) {
        delta <- min(diff(index(x)))
        delta <- sprintf("%.0f %s", as.numeric(delta), attr(delta, "unit"))
        if (grepl("^1 days", delta)) delta <- "daily"
        main <- sprintf("Observations for station %d (%.3fE %.3fN)\n%s temporal resolution",
                        attr(x, "station")$id,
                        attr(x, "station")$coord[[1]],
                        attr(x, "station")$coord[[2]], delta)

    }
    zoo::plot.zoo(x, main = main, xlab = xlab, ...)


}
