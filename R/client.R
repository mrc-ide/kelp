#' Create seaweed client, to manage HTTP requests to SeaweedFS
#'
#' @keywords internal
#' @noRd
seaweed_client <- R6::R6Class(
  "seaweed_client",
  cloneable = FALSE,

  public = list(
    seaweed_url = NULL,

    #' @description
    #' Create client object for sending http requests to seaweed
    #'
    #' @param seaweed_url Root URL of Seaweed master
    #'
    #' @return A new `seaweed_client` object
    initialize = function(seaweed_url) {
      self$seaweed_url <- seaweed_url
    },

    #' @description
    #' Send GET request to SeaweedFS master
    #'
    #' @param ... Args passed on to httr
    #'
    #' @return Response from SeaweedFS
    GET = function(...) {
      self$master_request(httr::GET, ...)
    },

    #' @description
    #' Send POST request to SeaweedFS master
    #'
    #' @param ... Args passed on to httr
    #'
    #' @return Response from SeaweedFS
    POST = function(...) {
      self$master_request(httr::POST, ...)
    },

    #' @description
    #' Send DELETE request to SeaweedFS
    #'
    #' @param ... Args passed on to httr
    #'
    #' @return Response from SeaweedFS
    DELETE = function(...) {
      self$master_request(httr::DELETE, ...)
    },

    #' @description
    #' Send an HTTP request
    #'
    #' @param verb A httr function for type of request to send e.g. httr::GET
    #' @param url Request URL
    #' @param ... Additional args passed on to httr
    #'
    #' @return Response from SeaweedFS
    request = function(verb, url, ...) {
      res <- verb(url, ...)
      self$parse_response(res)
    },

    #' @description
    #' Send an HTTP request to SeaweedFS master
    #'
    #' @param verb A httr function for type of request to send e.g. httr::GET
    #' @param path Request path
    #' @param ... Additional args passed on to httr
    #'
    #' @return Response from SeaweedFS
    master_request = function(verb, path, ...) {
      url <- paste(self$seaweed_url, path, sep = "/")
      res <- verb(url, ...)
      self$parse_response(res)
    },

    parse_response = function(res) {
      code <- httr::status_code(res)
      if (code >= 400 && code < 600) {
        stop(kelp_error(res))
      }
      ## Can be JSON or plain text response
      httr::content(res, encoding = "UTF-8")
    }
  )
)

kelp_error <- function(res) {
  dat <- httr::content(res, type = "application/json", encoding = "UTF-8")
  if (is.null(dat)) {
    error_message <- "Empty error message"
  } else {
    error_message <- list_to_character(dat$error)
  }
  err <- list(
    code = httr::status_code(res),
    message = error_message
  )
  class(err) <- c("kelp_error", "error", "condition")
  err
}
