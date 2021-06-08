#' Create a kelp object for interacting with SeaweedFS
#'
#' @keywords export
kelp <- R6::R6Class(
  "kelp",
  cloneable = FALSE,

  public = list(
    seaweed_client = NULL,

    #' @description
    #' Create client object for sending http requests to seaweed
    #'
    #' @param seaweed_url Root URL of Seaweed
    #'
    #' @return A new `kelp` object
    initialize = function(seaweed_url) {
      self$seaweed_client <- seaweed_client$new(seaweed_url)
    },

    #' @description
    #' Upload file to SeaweedFS
    #'
    #' @param path Path to file to be uploaded
    #'
    #' @return The uploaded location, name, URL and size
    create = function(path) {
      res <- self$seaweed_client$POST("submit", body = list(
        file = httr::upload_file(path)))
    },

    #' @description
    #' Read file from SeaweedFS
    #'
    #' @param url SeaweedFS fileUrl to read
    #'
    #' @return The file contents
    read = function(url) {
      self$seaweed_client$request(httr::GET, url)
    },

    #' @description
    #' Delete file from SeaweedFS
    #'
    #' @param url SeaweedFS fileUrl to delete
    #'
    #' @return The uploaded location, name, URL and size
    delete = function(url) {
      self$seaweed_client$request(httr::DELETE, url)
    }
  )
)
