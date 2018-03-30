

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
#'       an error. Value may be scalar or vector.
#'
#' \code{$expect_type(type)} Check that the token at the current position has the
#'       specified type. Silently return TRUE if it does, otherwise throw
#'       an error. Type may be scalar or vector.
#'
#'
#' \code{$consume_token{type=NULL, value=NULL}} Return the token at the current
#'      position and advance the stream to the next token. If \code{value} or
#'      \code{type} are specified, then \code{expect_value()} or \code{expect_type()}
#'      are called to verify the token before returning.  Type and value may be scalar or vector.
#'
#' \code{$consume_tokens_of_type{type, n=Inf}} Consume 'n' tokens of the given type.
#'      Consumption stops when 'n' is reached, or tokens are no longer of the
#'      specified type - whichever comes first. n' may be a vector of values e.g. c(3, 4) and tokens will be
#'      consumed until the maximum value is reached. The length of the returned
#'      values is checked to be one of the specified 'n'.  n' defaults to 'Inf'.
#'      Returns a vector of values of the consumed tokens. Type may be scalar or vector.
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
      if (self$current_value() %in% value) {
        return(invisible(TRUE))
      } else {
        message <- glue("Expected [{deparse(value)}] at position {self$position} but found [{self$current_type()}]: {dQuote(self$current_value())}")
        stop(message, call. = FALSE)
      }
    },


    expect_type = function(type) {
      if (self$current_type() %in% type) {
        return(invisible(TRUE))
      } else {
        message <- glue("Expected [{deparse(type)}] at position {self$position} but found [{self$current_type()}]: {dQuote(self$current_value())}")
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
    },


    consume_tokens_of_type = function(type, n=Inf) {
      start_position <- self$position
      values <- c()
      while ( (self$current_type() %in% type) && (length(values) < max(n)) ) {
        values <- c(values, self$consume_token(type=type))
      }
      if (!is.infinite(n) && !(length(values) %in% n)) {
        message <- glue("Expected {deparse(n)} values of type [{deparse(type)}] at position {start_position} but found {length(values)}")
        stop(message, call. = FALSE)
      }
      values
    }


  )
)






