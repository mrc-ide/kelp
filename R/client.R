#' Create seaweed client, to manage HTTP requests to SeaweedFS
#'
#' @keywords internal
#' @noRd
seaweed_client <- R6::R6Class(
  "seaweed_client",
  cloneable = FALSE,

  #' @description
  #' Create client object for sending http requests to seaweed
  #'
  #' @param seaweed_url Root URL of Seaweed
  #'
  #' @param auth Authentication data as returned by the
  #' \code{$get_auth_data()} method
  #'
  #' @return A new `sharepoint_client` object
  public = list(
    seaweed_url = NULL,

    initialize = function(seaweed_url) {
      self$seaweed_url <- seaweed_url
    },

    #' @description
    #' Send GET request to SeaweedFS
    #'
    #' @param ... Args passed on to httr
    #'
    #' @return HTTP response
    GET = function(...) {
      private$request(httr::GET, ...)
    },

    #' @description
    #' Send POST request to SeaweedFS
    #'
    #' @param ... Args passed on to httr
    #'
    #' @return HTTP response
    POST = function(...) {
      private$request(httr::POST, ...)
    },

    #' @description
    #' Send DELETE request to SeaweedFS
    #'
    #' @param ... Args passed on to httr
    #'
    #' @return HTTP response
    DELETE = function(...) {
      private$request(httr::DELETE, ...)
    }
  ),

  private = list(
    #' @description
    #' Send an HTTP request to SeaweedFS
    #'
    #' @param verb A httr function for type of request to send e.g. httr::GET
    #' @param path Request path
    #' @param ... Additional args passed on to httr
    #'
    #' @return HTTP response
    request = function(verb, path, ...) {
      url <- paste(self$seaweed_url, path, sep = "/")
      verb(url, ...)
    }
  )
)
