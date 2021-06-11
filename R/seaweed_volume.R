#' Create a seaweed_volume object for interacting with SeaweedFS master
#'
#' @keywords export
seaweed_volume <- R6::R6Class(
  "seaweed_volume",
  cloneable = FALSE,

  public = list(
    #' @field client A `seaweed_client` object for sending requests
    client = NULL,

    #' @description
    #' Create client object for sending http requests to seaweed volume
    #'
    #' @param seaweed_url Root URL of Seaweed volume
    #'
    #' @return A new `seaweed_volume` object
    initialize = function(seaweed_url) {
      self$client <- seaweed_client$new(seaweed_url)
    }
  )
)
