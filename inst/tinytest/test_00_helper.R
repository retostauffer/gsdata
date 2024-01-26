# -------------------------------------------------------
# Checking helper functions
# -------------------------------------------------------

if (interactive()) library("tinytest")

# -------------------------------------------------------
# Temporal interval 'decoder'
# -------------------------------------------------------
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


# -------------------------------------------------------
# Show http status and terminate
# -------------------------------------------------------
expect_error(gsdata:::show_http_status_and_terminate(),
              info = "Testing usage - no input")
expect_error(gsdata:::show_http_status_and_terminate(1:2),
              info = "Testing usage - wrong input")
expect_error(gsdata:::show_http_status_and_terminate("foo"),
              info = "Testing usage - wrong input")
expect_silent(gsdata:::show_http_status_and_terminate(200),
              info = "Testing behaviour if status code is in range 200")
expect_silent(gsdata:::show_http_status_and_terminate(201),
              info = "Testing behaviour if status code is in range 200")
expect_identical(gsdata:::show_http_status_and_terminate(200), NULL,
              info = "Return NULL if status code is in the 200 range")

expect_error(gsdata:::show_http_status_and_terminate(400),
             pattern = ".*Client error.*Bad Request.*",
             info = "Testing error message for status code 400; no xtra")
expect_error(gsdata:::show_http_status_and_terminate(400),
             pattern = "http_status description.*Client error.*Bad Request.*",
             info = "Testing error message for status code 400; no xtra")
expect_error(gsdata:::show_http_status_and_terminate(400, list(xtra = "custom error")),
             pattern = "HTTP request error.*status returned by API.*xtra:.*custom error.*Client error.*Bad Request.*",
             info = "Testing error message for status code 400; with xtra")
expect_error(gsdata:::show_http_status_and_terminate(99999),
             pattern = "HTTP request error: server returned status code 99999",
             info = "Testing status code not recognized by http_status.")



