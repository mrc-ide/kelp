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
    #' @param seaweed_url Root URL of Seaweed
    #'
    #' @return A new `seaweed_client` object
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
      private$master_request(httr::GET, ...)
    },

    #' @description
    #' Send POST request to SeaweedFS
    #'
    #' @param ... Args passed on to httr
    #'
    #' @return HTTP response
    POST = function(...) {
      private$master_request(httr::POST, ...)
    },

    #' @description
    #' Send DELETE request to SeaweedFS
    #'
    #' @param ... Args passed on to httr
    #'
    #' @return HTTP response
    DELETE = function(...) {
      private$master_request(httr::DELETE, ...)
    },

    #' @description
    #' Send an HTTP request
    #'
    #' @param verb A httr function for type of request to send e.g. httr::GET
    #' @param url Request URL
    #' @param ... Additional args passed on to httr
    #'
    #' @return HTTP response
    request = function(verb, url, ...) {
      verb(url, ...)
    }
  ),

  #' @description
  #' Send an HTTP request to SeaweedFS master
  #'
  #' @param verb A httr function for type of request to send e.g. httr::GET
  #' @param path Request path
  #' @param ... Additional args passed on to httr
  #'
  #' @return HTTP response
  private = list(
    master_request = function(verb, path, ...) {
        url <- paste(self$seaweed_url, path, sep = "/")
        verb(url, ...)
    }
  )
)
