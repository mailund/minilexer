context("disallow-multiple-pattern-capture")

test_that("mutiple capture groups per pattern not allowed", {

  patterns <- c(single = 'a', multi = '(b)|(c)')
  text     <- "abc"
  expect_error(lex(text, patterns), "Multiple capture")

})
