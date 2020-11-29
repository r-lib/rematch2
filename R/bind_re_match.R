#' Match results from a data frame column and attach results
#'
#' Taking a data frame and a column name as input, this function will run
#' \code{\link{re_match}} and bind the results as new columns to the original
#' table., returning a \code{\link[tibble]{tibble}} if the tibble package is
#' installed, a \code{data.frame} otherwise. This makes it friendly for
#' pipe-oriented programming with \link[magrittr]{magrittr}.
#'
#' @note If named capture groups will result in multiple columns with the same
#'   column name, \code{\link[tibble]{repair_names}} will be called on the
#'   resulting table.
#'
#' @param df A data frame.
#' @param from Name of column to use as input for \code{\link{re_match}}.
#'   \code{\link{bind_re_match}} takes unquoted names, while
#'   \code{\link{bind_re_match_}} takes quoted names.
#' @param ... Arguments (including \code{pattern}) to pass to
#'   \code{\link{re_match}}.
#' @param keep_match Should the column \code{.match} be included in the results?
#'   Defaults to \code{FALSE}, to avoid column name collisions in the case that
#'   \code{\link{bind_re_match}} is called multiple times in succession.
#'
#' @seealso Standard-evaluation version \code{\link{bind_re_match_}} that is
#'   suitable for programming.
#'
#' @examples
#' match_cars <- mtcars
#' match_cars$rowname <- rownames(mtcars)
#' bind_re_match(match_cars, rowname, "^(?<make>\\w+) ?(?<model>.+)?$")
#'
#' @export
bind_re_match <- function(df, from, ..., keep_match = FALSE) {
  bind_re_match_(df = df, from = deparse(substitute(from)), ..., keep_match = keep_match)
}

#' @describeIn bind_re_match Standard-evaluation version that takes a quoted column name.
#' @export
bind_re_match_ <- function(df, from, ..., keep_match = FALSE) {

  stopifnot(is.data.frame(df))
  stopifnot(is_installed("tibble"))

  if (!from %in% names(df))
    stop(from, " is not present in the data frame.")

  res <- re_match(text = df[[from]], ...)

  res <- res[, !names(res) == ".text"]

  if (!keep_match) {
    res <- res[, !names(res) == ".match"]
  }

  tibble::repair_names(cbind(df, res))
}
