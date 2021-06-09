`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}

list_to_character <- function(x) {
  vapply(x, identity, character(1))
}
