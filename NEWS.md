
# gsdata 0.0-6

* Fixed a bug in `gs_stationdata()` when batching due to limit
* Reduced the `limit` to `2e5`. API documentation states that the limit is 1e6,
  but this leads to bad requests (exceeding API limits)
* Adding `drop = TRUE` to `gs_datationdata()`.
* Adding a series of basic tests for `gs_stationdata()`
* Currently facing a strange behaviour of `ve/datasets`, see
    <https://github.com/Geosphere-Austria/dataset-api-docs/issues/11>

# gsdata 0.0-5

* Switching API endpoint to newly released URL (<https://data.hub.geosphere.at>).

# gsdata 0.0-4

* Ignoring `start` and `end` when calling `gs_stationdata()` with `mode = "current"`
* Partial matching for `mode` in `gs_stationdata()`
* `mode = "historical"` `resource_id = "histalp-v1-1y"` requires login; throwing an error for now (todo).

# gsdata 0.0-3

* Reduced limit for data requests in `gs_stationdata()` to `5e5`
* Throwing error if `end <= start` in `gs_stationdata()`

# gsdata 0.0-2

* First working "release" of the `gsdata` package (mainly coded for personal use;
    might undergo further development).
* Allows for downloading different station data sets.
* Implemented the three main functions `gs_datasets()`, `gs_metadata()` and `gs_stationdata().
* Package documentation for the main functionality.


