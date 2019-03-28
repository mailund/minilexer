

suppressPackageStartupMessages({
  library(testthat)
  library(minilexer)
})

context("TokenParser")

R_patterns <- c(
  number      = "-?\\d*\\.?\\d+",
  name        = "\\w+",
  equals      = "==",
  assign      = "<-|=",
  plus        = "\\+",
  lbracket    = "\\(",
  rbracket    = "\\)",
  newline     = "\n",
  whitespace  = "\\s+"
)

R_code <- "x <- 3 + 4.2 + rnorm(1)"

R_tokens <- lex(R_code, R_patterns)


test_that("TokenParser works", {
  # create the stream to handle the tokens
  stream <- minilexer::TokenStream$new(R_tokens)

  # What position are we at?
  expect_equal(stream$position, 1)

  # Assert that the first token is a name and has the value 'x'
  expect_true(stream$expect_value('x'))
  expect_true(stream$expect_type('name'))

  # Show what happens if the current token isn't what we expect
  expect_error(stream$expect_value('+'), "Expected")

  # Try and consume this token and move onto the next one, but
  # because the 'type' is incorrect, will result in failure
  expect_error(stream$consume_token(type='number'), "Expected")

  # Unconditionally consume this token without regard to
  # its value or type. This returns the value at the
  # current position, and then increments the position
  expect_equal(stream$consume_token(), 'x')

  # Stream position should have moved on to the second value
  expect_equal(stream$position, 2)

  # Get the current value, but without advancing the position
  expect_equal(stream$current_value(), ' ')

  # consume it. i.e. return current value and increment position
  expect_equal(stream$consume_token(type='whitespace'), ' ')

  # Stream position should have moved on to the third value
  expect_equal(stream$position, 3)

  # Get the current value
  expect_equal(stream$current_value(), "<-")
  expect_equal(stream$current_type() , "assign")
})


test_that("consume_tokens_of_type() works 1", {
  text <- "hello 1 2 3 4 5 goodbye now"
  patterns <- c(
    number      = "-?\\d*\\.?\\d+",
    name        = "\\w+",
    newline     = "\n",
    whitespace  = "\\s+"
  )
  tokens <- lex(text, patterns)
  tokens <- tokens[!(names(tokens) %in% c('whitespace', 'newline', 'comment'))]
  stream <- TokenStream$new(tokens)
  t1     <- stream$consume_tokens_of_type('name')    # grap all names. in this case, just 1
  t2     <- stream$consume_tokens_of_type('number')  # grab all numbers until no more
  t3     <- stream$consume_tokens_of_type('name')    # Should keep reading until the end of file

  expect_equal(t1, 'hello')
  expect_equal(t2, c('1', '2', '3', '4', '5'))
  expect_equal(t3, c('goodbye', 'now'))

  expect_true(stream$end_of_stream)
})


test_that("consume_tokens_of_type() works 2", {
  text <- "hello 1 2 3 4 5 goodbye now"
  patterns <- c(
    number      = "-?\\d*\\.?\\d+",
    name        = "\\w+",
    newline     = "\n",
    whitespace  = "\\s+"
  )
  tokens <- lex(text, patterns)
  tokens <- tokens[!(names(tokens) %in% c('whitespace', 'newline', 'comment'))]
  stream <- TokenStream$new(tokens)
  t1     <- stream$consume_tokens_of_type('name')
  t2     <- stream$consume_tokens_of_type('number', n=c(1, 3))

  expect_equal(t1, 'hello')
  expect_equal(t2, c('1', '2', '3'))

  expect_true(stream$current_value() == '4')
  expect_false(stream$end_of_stream)
})


test_that("consume_tokens_of_type() works 3", {
  text <- "hello 1 2 3 XX 4 5 goodbye now 99"
  patterns <- c(
    breaker     = "XX",
    number      = "-?\\d*\\.?\\d+",
    name        = "\\w+",
    newline     = "\n",
    whitespace  = "\\s+"
  )
  tokens <- lex(text, patterns)
  tokens <- tokens[!(names(tokens) %in% c('whitespace', 'newline', 'comment'))]
  stream <- TokenStream$new(tokens)


  t1     <- stream$consume_tokens_of_type(c('name', 'number'))
  t2     <- stream$consume_token('breaker')
  t3     <- stream$consume_tokens_of_type('number', n=1:3)
  t4     <- stream$consume_tokens_of_type(c('name', 'number'))

  expect_equal(t1, c('hello', '1', '2', '3'))
  expect_equal(t2, 'XX')
  expect_equal(t3, c('4', '5'))
  expect_equal(t4, c('goodbye', 'now', '99'))

  expect_true(is.na(stream$current_value()))
  expect_true(stream$end_of_stream)
})



