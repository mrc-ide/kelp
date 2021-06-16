`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}

list_to_character <- function(x) {
  vapply(x, identity, character(1))
}

object_to_bin <- function(obj) {
  serialize(obj, NULL, xdr = FALSE)
}

bin_to_object <- function(bin) {
  unserialize(bin)
}
