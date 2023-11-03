#' Match results from a data frame column and attach results
#'
#' Taking a data frame and a column name as input, this function will run
#' [re_match()] and bind the results as new columns to the original
#' table., returning a [tibble::tibble()]. This makes it friendly for
#' pipe-oriented programming with [magrittr][magrittr::magrittr].
#'
#' @note If named capture groups will result in multiple columns with the same
#'   column name, [tibble::repair_names()] will be called on the
#'   resulting table.
#'
#' @param df A data frame.
#' @param from Name of column to use as input for [re_match()].
#'   [bind_re_match()] takes unquoted names, while
#'   [bind_re_match_()] takes quoted names.
#' @param ... Arguments (including `pattern`) to pass to
#'   [re_match()].
#' @param keep_match Should the column `.match` be included in the results?
#'   Defaults to `FALSE`, to avoid column name collisions in the case that
#'   [bind_re_match()] is called multiple times in succession.
#'
#' @seealso Standard-evaluation version [bind_re_match_()] that is
#'   suitable for programming.
#'
#' @examples
#' match_cars <- tibble::rownames_to_column(mtcars)
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
  if (!tibble::has_name(df, from))
    stop(from, " is not present in the data frame.")

  res <- re_match(text = df[[from]], ...)

  res <- res[, !names(res) == ".text"]

  if (!keep_match) {
    res <- res[, !names(res) == ".match"]
  }

  tibble::repair_names(cbind(df, res))
}
