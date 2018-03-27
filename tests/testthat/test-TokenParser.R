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
