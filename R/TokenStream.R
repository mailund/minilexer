

#-----------------------------------------------------------------------------
#' An R6 class for manipulating/interrogating a stream of tokens.
#'
#' @section Usage:
#' \preformatted{stream = TokenStream$new(tokens)
#'
#' stream$current_value()
#'
#' }
#'
#' @importFrom R6 R6Class
#' @importFrom glue glue
#' @export
#'
#' @format \code{\link{R6Class}} object.
#'
#' @section Arguments:
#' \code{tokens} The named character vector of tokens output by \code{lex()}.
#'
#' @section Methods:
#' \code{$new()} Initialize a token stream with a vector of tokens.
#'
#'
#' \code{$get_token_value(offset = 0)} Get the value of the token at the given
#'      offset from the current position.
#'
#' \code{$get_token_type(offset = 0)} Get the type of the token at the given
#'      offset from the current position.
#'
#'
#' \code{$current_value()} Get the value of the token at the current position.
#'
#' \code{$current_type()} Get the type of the token at the current position.
#'
#'
#' \code{$expect_value(value)} Check that the token at the current position has the
#'       specified value. Silently return TRUE if it does, otherwise throw
#'       an error.
#'
#' \code{$expect_type(type)} Check that the token at the current position has the
#'       specified type. Silently return TRUE if it does, otherwise throw
#'       an error.
#'
#'
#' \code{$consume_token{type=NULL, value=NULL}} Return the token at the current
#'      position and advance the stream to the next token. If \code{value} or
#'      \code{type} are specified, then \code{expect_value()} or \code{expect_type()}
#'      are called to verify the token before returning.
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


    expect_value = function(value) {
      if (identical(self$current_value(), value)) {
        return(invisible(TRUE))
      } else {
        message <- glue("Expected [{value}] at position {self$position} but found [{self$current_type()}]: {dQuote(self$current_value())}")
        stop(message, call. = FALSE)
      }
    },


    expect_type = function(type) {
      if (identical(self$current_type(), type)) {
        return(invisible(TRUE))
      } else {
        message <- glue("Expected [{type}] at position {self$position} but found [{self$current_type()}]: {dQuote(self$current_value())}")
        stop(message, call. = FALSE)
      }
    },


    current_value = function() {
      self$values[self$position]
    },


    current_type = function() {
      self$types[self$position]
    },


    consume_token = function(type=NULL, value=NULL) {
      if (!is.null(value)) { self$expect_value(value) }
      if (!is.null(type))  { self$expect_type(type)   }
      value <- self$current_value()
      self$position <- self$position + 1
      if (self$position > length(self$tokens)) {
        self$end_of_stream <- TRUE
      }
      value
    }
  )
)