as_tibble <- function(x) {
  if (is_installed("tibble")) {
    tibble::new_tibble(x, nrow = NROW(x))
  } else {
    x
  }
}

is_installed <- function(pkg) {
  isTRUE(requireNamespace(pkg, quietly = TRUE))
}
