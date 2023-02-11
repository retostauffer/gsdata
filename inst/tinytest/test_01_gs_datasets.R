# -------------------------------------------------------
# Checking function 'annex_check_config'
# -------------------------------------------------------

if (interactive()) library("tinytest")


expect_silent(x <- gs_datasets(),
              info = "Calling dataset endpoint")
expect_inherits(x, "data.frame",
              info = "Return must be a data.frame")
expect_identical(c("type", "mode", "resource_id", "data_format", "response_formats", "url"),
                 names(x),
                 info = "Checking columns of return")


