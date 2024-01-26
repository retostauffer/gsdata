# -------------------------------------------------------
# Checking function datasets
# -------------------------------------------------------

if (interactive()) library("tinytest")

options("gsdata.cooldown" = 0.5)

# Misuse
expect_error(do.call(gs_stationdata, tmp),
             info = "Stations do not exist")

expect_error(do.call(gs_stationdata, tmp),
             info = "Stations do not exist")

expect_error(gs_stationdata(),
             info = "Required arguments missing")
expect_error(gs_stationdata(mode = "historical", station_ids = 11120),
             info = "Required argument resource_id missing")
expect_error(gs_stationdata(mode = "historical", resource_id = "synop-v1-1h"),
             info = "Required argument station_ids missing")
expect_error(gs_stationdata(mode = "historical", resource_id = "synop-v1-1h",
                            station_ids = 999999),
             info = "Station does not exist")

expect_error(gs_stationdata(mode = character(0), resource_id = "synop-v1-1h",
                            station_ids = 11120),
             info = "mode must be character length 1")
expect_error(gs_stationdata(mode = 1, resource_id = "synop-v1-1h",
                            station_ids = 11120),
             info = "mode must be character")

expect_error(gs_stationdata(mode = "historical", resource_id = character(0),
                            station_ids = 11120),
             info = "resource_id must be character length 1")
expect_error(gs_stationdata(mode = "historical", resource_id = 1,
                            station_ids = 11120),
             info = "resource_id must be character")

expect_error(gs_stationdata(mode = "historical", resource_id = "synop-v1-1h",
                            station_ids = 11120, parameters = character(0)),
             info = "parameters must be character of length > 0")
expect_error(gs_stationdata(mode = "historical", resource_id = "synop-v1-1h",
                            station_ids = 11120, parameters = 1:2),
             info = "parameters must be character")

expect_error(gs_stationdata(mode = "historical", resource_id = "synop-v1-1h",
                            station_ids = 11120, verbose = c(TRUE, TRUE)),
             info = "verbose must be single TRUE or FALSE")
expect_error(gs_stationdata(mode = "historical", resource_id = "synop-v1-1h",
                            station_ids = 11120, verbose = logical(0)),
             info = "verbose must be single TRUE or FALSE")
expect_error(gs_stationdata(mode = "historical", resource_id = "synop-v1-1h",
                            station_ids = 11120, expert = c(TRUE, TRUE)),
             info = "expert must be single TRUE or FALSE")
expect_error(gs_stationdata(mode = "historical", resource_id = "synop-v1-1h",
                            station_ids = 11120, expert = logical(0)),
             info = "expert must be single TRUE or FALSE")
expect_error(gs_stationdata(mode = "historical", resource_id = "synop-v1-1h",
                            station_ids = 11120, drop = c(TRUE, TRUE)),
             info = "drop must be single TRUE or FALSE")
expect_error(gs_stationdata(mode = "historical", resource_id = "synop-v1-1h",
                            station_ids = 11120, drop = logical(0)),
             info = "drop must be single TRUE or FALSE")


# -------------------------------------------------------
# Querying some data (single station)
# -------------------------------------------------------
cnf <- list(mode = "historical",
            resource_id = "synop-v1-1h",
            parameters  = c("RRR", "Tmin", "Tmax", "sonne"),
            start       = "2022-01-01",
            end         = as.Date("2022-01-03"),
            station_ids = 11120,
            verbose     = FALSE,
            expert      = TRUE)

# Example requests
expect_silent(data1 <- do.call(gs_stationdata, cnf),
              info = "Basic small request to synop-v1-1h historical")

# Setting limit; should result in 3 API calls (batching)
tmp <- cnf; tmp$limit <- 100
expect_silent(data2 <- do.call(gs_stationdata, tmp),
              info = "Testing batching (limit = 100)")
expect_identical(data1, data2,
                 info = "Test if result w/ and w/o batching is identical")


# Testing return argument
expect_identical(class(data1), c("gs_stationdata", "zoo"),
                info = "Return must be c(\"gs_stationdata\", \"zoo\")")
expect_inherits(index(data1), "POSIXct",
                info = "Return must contain index of class POSIXct")
expect_inherits(coredata(data1), "matrix",
                info = "Return must be multivariate zoo")
expect_true(is.numeric(coredata(data1)),
            info = "Return must be numeric zoo")

# Attributes
expect_identical(attr(data1, "station")$id, 11120L,
                 info = "Testing attribute station$id")
expect_inherits(attr(data1, "station")$coord, "POINT",
                 info = "Testing attribute station$coord")
expect_true(length(attr(data1, "station")$coord) == 2,
                 info = "Testing attribute station$coord")

expect_inherits(attr(data1, "parameter"), "data.frame",
                 info = "Testing attribute parameter")
expect_identical(dim(attr(data1, "parameter")), c(4L, 2L),
                 info = "Testing attribute parameter")
expect_identical(rownames(attr(data1, "parameter")), c("RRR", "Tmin", "Tmax", "sonne"),
                 info = "Testing attribute parameter")


# -------------------------------------------------------
# Multiple stations
# -------------------------------------------------------
tmp <- cnf; tmp$station_ids <- c(11120, 11019)
expect_silent(data3 <- do.call(gs_stationdata, tmp),
              info = "Querying multiple stations")
expect_inherits(data3, "list",
                info = "Return value when querying multiple stations, class must be list")
expect_identical(length(data3), 2L,
                info = "Return value when querying multiple stations, length must be 2")
expect_identical(names(data3), as.character(tmp$station_ids),
                info = "Return value when querying multiple stations, list names")
expect_identical(class(data3[[1]]), c("gs_stationdata", "zoo"),
                info = "Return must be c(\"gs_stationdata\", \"zoo\")")
expect_identical(class(data3[[2]]), c("gs_stationdata", "zoo"),
                info = "Return must be c(\"gs_stationdata\", \"zoo\")")



