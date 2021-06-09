#' Create a kelp object for interacting with SeaweedFS
#'
#' @keywords export
kelp <- R6::R6Class(
  "kelp",
  cloneable = FALSE,

  public = list(
    #' @field seaweed_client A `seaweed_client` object for sending requests
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
    upload = function(path) {
      if (!file.exists(path)) {
        stop(sprintf("File at %s doesn't exist. Cannot upload.", path))
      }
      self$seaweed_client$POST("submit", body = list(
        file = httr::upload_file(path)))
    },

    #' @description
    #' Get file URL from file ID.
    #'
    #' This gets URL that can be used to read, update or delete a file. We
    #' don't just use the URL returned on file creation as the volume may
    #' have moved.
    #'
    #' @param fid SeaweedFS file ID
    #'
    #' @return URL which can be used to read, update or delete a file
    file_url = function(fid) {
      res <- self$seaweed_client$GET(paste0("/dir/lookup?fileId=", fid))
      ## There can be multiple - always return the first (maybe something
      ## more clever in future)
      url <- res$locations[[1]]$publicUrl
      paste0(url, "/", fid)
    },

    #' @description
    #' Read file from SeaweedFS
    #'
    #' @param fid SeaweedFS file ID to read
    #'
    #' @return The file contents
    read = function(fid) {
      url <- self$file_url(fid)
      self$seaweed_client$request(httr::GET, url)
    },

    #' @description
    #' Delete file from SeaweedFS
    #'
    #' @param fid SeaweedFS file ID to delete
    #'
    #' @return Response from SeaweedFS, the deleted size
    delete = function(fid) {
      url <- self$file_url(fid)
      self$seaweed_client$request(httr::DELETE, url)
    }
  )
)
