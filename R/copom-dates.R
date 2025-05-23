#' Historical COPOM dates
#'
#' Historical COPOM dates since 2015.
#'
#' @param date reference date
#' @param n number of dates returned
#'
#' If `date` is `NULL` all dates are returned.
#' Once `date` is set, `n` dates greater than or equal to `date` are returned.
#'
#' @return A vector with `Date` objects.
#' @examples
#' get_copom_dates()
#' get_copom_dates("2022-01-04", 4)
#' @export
get_copom_dates <- function(date = NULL, n = 8) {
  copom_dates <- c(
    "2015-01-21", "2015-03-04", "2015-04-29", "2015-06-03", "2015-07-29",
    "2015-09-02", "2015-10-21", "2015-11-25",
    "2016-01-20", "2016-03-02", "2016-04-27", "2016-06-08", "2016-07-20",
    "2016-08-31", "2016-10-19", "2016-11-29",
    "2017-01-11", "2017-02-22", "2017-04-12", "2017-05-31", "2017-07-26",
    "2017-09-06", "2017-10-25", "2017-12-06",
    "2018-02-07", "2018-03-21", "2018-05-16", "2018-06-20", "2018-08-01",
    "2018-09-19", "2018-10-31", "2018-12-12",
    "2019-02-06", "2019-03-20", "2019-05-08", "2019-06-19", "2019-07-31",
    "2019-09-18", "2019-10-30", "2019-12-11",
    "2020-02-05", "2020-03-18", "2020-05-06", "2020-06-17", "2020-08-05",
    "2020-09-16", "2020-10-28", "2020-12-09",
    "2021-01-20", "2021-03-17", "2021-05-05", "2021-06-16", "2021-08-04",
    "2021-09-22", "2021-10-27", "2021-12-08",
    "2022-02-02", "2022-03-16", "2022-05-04", "2022-06-15", "2022-08-03",
    "2022-09-21", "2022-10-26", "2022-12-07",
    "2023-02-01", "2023-03-22", "2023-05-03", "2023-06-21", "2023-08-02",
    "2023-09-20", "2023-11-01", "2023-12-13",
    "2024-01-31", "2024-03-20", "2024-05-08", "2024-06-19", "2024-07-31",
    "2024-09-18", "2024-11-06", "2024-12-11",
    "2025-01-29", "2025-03-19", "2025-05-07", "2025-06-18", "2025-07-30",
    "2025-09-17", "2025-11-05", "2025-12-10"
  )

  x <- sort(as.Date(copom_dates))
  if (is.null(date)) {
    x
  } else {
    date <- as.Date(date)
    dates <- x[x >= date]
    dates[seq_len(n)]
  }
}
