# -------------------------------------------------------
# Checking helper functions
# -------------------------------------------------------

if (interactive()) library("tinytest")

# Testing extrating temporal interval from resource ID
expect_error(gs_temporal_interval())
expect_error(gs_temporal_interval(3))
expect_error(gs_temporal_interval(character()))
expect_error(gs_temporal_interval("foo-1h-bar", 3))

# Correct return value and correct decoding
tmp <- c("foo-v1-200min-bar", "foo-v2-8d-bar", "foo-v3-144h-bar", "something")
expect_warning(t <- gs_temporal_interval(tmp),
              pattern = "^not able to extract temporal interval from .*, returning NA\\.$",
              info = "Checking warning message.")
expect_identical(t, c(200L * 60L, 8L * 86400L, 144L * 3600L, NA_integer_),
                 info = "Checking for correct return and correct calculation (1/1)")

tmp <- c("foo-v1-200min-bar", "foo-v100-8d-bar", "foo-v22-144h-bar",
         "foo-v1-200min",     "foo-v100-8d",     "foo-v22-144h")
expect_silent(t <- gs_temporal_interval(tmp),
              info = "Checking correct usage.")
expect_identical(t, rep(c(200L * 60L, 8L * 86400L, 144L * 3600L), 2),
                 info = "Checking for correct return and correct calculation (2/3)")

# Months and years; one month is estimated to have 31 * 86400 seconds,
# one year is estimated to have 366 * 86400 seconcs (both on the max end).
tmp <- c("foo-v1-1y-bar", "foo-v324-1m-bar",
         "foo-v1-1y",     "foo-v324-1m")
expect_silent(t <- gs_temporal_interval(tmp),
              info = "Checking correct usage.")
expect_identical(t, rep(c(366L * 86400L, 31L * 86400L), 2),
                 info = "Checking for correct return and correct calculation (3/3)")
