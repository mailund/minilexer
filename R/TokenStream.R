

#-----------------------------------------------------------------------------
#' A class for manipulating/interrogating a stream of tokens
#'
#' @importFrom R6 R6Class
#' @importFrom glue glue
#' @export
#-----------------------------------------------------------------------------
TokenStream <- R6::R6Class(
  "TokenStream",
  public = list(
    tokens        = NULL,
    position      = 1L,
    initialize = function(tokens) {
      self$tokens <- tokens
      invisible(self)
    },


    get_token_value = function(offset = 0) {
      unname(self$tokens[self$position + offset])
    },


    get_token_type = function(offset = 0) {
      names(self$tokens[self$position + offset])
    },


    expect_type = function(type) {
      if (is.null(type) || identical(self$get_token_type(), type)) {
        return(invisible(TRUE))
      } else {
        message <- glue("Expected [{type}] at position {self$position} but found [{self$get_token_type()}]: {dQuote(self$get_token_value())}")
        stop(message, call. = FALSE)
      }
    },


    expect_value = function(value) {
      if (is.null(value) || identical(self$get_token_value(), value)) {
        return(invisible(TRUE))
      } else {
        message <- glue("Expected [{value}] at position {self$position} but found [{self$get_token_type()}]: {dQuote(self$get_token_value())}")
        stop(message, call. = FALSE)
      }
    },


    current_type = function() {
      self$get_token_type(offset = 0)
    },


    current_value = function() {
      self$get_token_value(offset = 0)
    },


    consume_token = function(type=NULL, value=NULL) {
      self$expect_value(value)
      self$expect_type(type)
      token <- self$get_token_value()
      self$position <- self$position + 1
      token
    }
  )
)