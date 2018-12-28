context("test-debug")

test_that("debug works", {

  patterns <- c('a' = 1, 'b' = 2)

  expect_output(lex("12", patterns, debug = TRUE), ".missing")
})




test_that(".missing handled works", {

  patterns <- c('a' = 1, 'b' = 2)

  expect_warning(res <- lex("123", patterns), "were not captured")

  expect_identical(names(res), c('a', 'b', '.missing'))
})
