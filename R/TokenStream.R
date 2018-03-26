

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
    values        = NULL,
    types         = NULL,
    position      = 1L,
    end_of_stream = FALSE,
    initialize = function(tokens) {
      self$tokens <- tokens
      self$types  <- names(tokens)
      self$values <- unname(tokens)
      invisible(self)
    },


    get_token_value = function(offset = 0) {
      self$values[self$position + offset]
    },


    get_token_type = function(offset = 0) {
      self$types[self$position + offset]
    },


    expect_type = function(type) {
      if (is.null(type) || identical(self$current_type(), type)) {
        return(invisible(TRUE))
      } else {
        message <- glue("Expected [{type}] at position {self$position} but found [{self$current_type()}]: {dQuote(self$current_value())}")
        stop(message, call. = FALSE)
      }
    },


    expect_value = function(value) {
      if (is.null(value) || identical(self$current_value(), value)) {
        return(invisible(TRUE))
      } else {
        message <- glue("Expected [{value}] at position {self$position} but found [{self$current_type()}]: {dQuote(self$current_value())}")
        stop(message, call. = FALSE)
      }
    },


    current_type = function() {
      self$types[self$position]
    },


    current_value = function() {
      self$values[self$position]
    },


    consume_token = function(type=NULL, value=NULL) {
      self$expect_value(value)
      self$expect_type(type)
      value <- self$current_value()
      self$position <- self$position + 1
      if (self$position > length(self$tokens)) {
        self$end_of_stream <- TRUE
      }
      value
    }
  )
)