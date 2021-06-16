#' Create seaweed client, to manage HTTP requests to SeaweedFS
#'
#' @keywords internal
#' @noRd
seaweed_client <- R6::R6Class(
  "seaweed_client",
  cloneable = FALSE,

  public = list(
    #' @field client The base URL for sending requests to SeaweedFS.
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
    #' @param path Request path
    #' @param as Type of response, json, text or raw
    #' @param ... Args passed on to httr
    #'
    #' @return Response from SeaweedFS
    GET = function(path, as = "json", ...) {
      self$request(httr::GET, path, as, ...)
    },

    #' @description
    #' Send POST request to SeaweedFS master
    #'
    #' @param path Request path
    #' @param as Type of response, json, text or raw
    #' @param ... Args passed on to httr
    #'
    #' @return Response from SeaweedFS
    POST = function(path, as = "json", ...) {
      self$request(httr::POST, path, as, ...)
    },

    #' @description
    #' Send DELETE request to SeaweedFS
    #'
    #' @param path Request path
    #' @param as Type of response, json, text or raw
    #' @param ... Args passed on to httr
    #'
    #' @return Response from SeaweedFS
    DELETE = function(path, as = "json", ...) {
      self$request(httr::DELETE, path, as, ...)
    },

    #' @description
    #' Send an HTTP request to SeaweedFS master
    #'
    #' @param verb A httr function for type of request to send e.g. httr::GET
    #' @param path Request path
    #' @param as Type of response, json, text or raw
    #' @param ... Additional args passed on to httr
    #'
    #' @return Response from SeaweedFS
    request = function(verb, path, as = "json", ...) {
      url <- paste0(self$seaweed_url, "/", path)
      res <- verb(url, scheme = "http", ...)
      private$parse_response(res, as)
    }
  ),

  private = list(
    parse_response = function(res, as = "json") {
      code <- httr::status_code(res)
      if (code >= 400 && code < 600) {
        stop(kelp_error(res))
      }
      httr_as <- as
      ## If JSON read as text and convert afterwards
      if (httr_as == "json") {
        httr_as <- "text"
      }
      content <- httr::content(res, as = httr_as, encoding = "UTF-8")
      if (as == "json") {
        content <- jsonlite::fromJSON(content, simplifyDataFrame = FALSE,
                                      simplifyMatrix = FALSE)
      }
      content
    }
  )
)

kelp_error <- function(res) {
  dat <- httr::content(res, type = "application/json", encoding = "UTF-8")
  if (is.null(dat)) {
    error_message <- httr::http_status(res)$message
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
