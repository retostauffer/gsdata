# -------------------------------------------------------
# Checking function datasets
# -------------------------------------------------------

if (interactive()) library("tinytest")


expect_silent(ds <- gs_datasets(),
                 info = "Calling dataset endpoint")
expect_inherits(ds, "data.frame",
                 info = "Return must be a data.frame")
expect_identical(c("type", "mode", "resource_id", "data_format", "response_formats", "url"),
                 names(ds),
                 info = "Checking columns of return")
expect_true(all(sapply(ds, is.character)),
                 info = "Expecting all variables to be of class character")


