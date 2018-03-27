context("lex")

test_that("lex() works", {
  sentence_patterns <- c(
    word        = "\\w+",
    whitespace  = "\\s+",
    fullstop    = "\\.",
    comma       = ","
  )

  sentence = "Hello there, Rstats."

  res <- lex(sentence, sentence_patterns)

  expect_equal(names(res) , c( 'word', 'whitespace',  'word', 'comma', 'whitespace',   'word', 'fullstop'))
  expect_equal(unname(res), c('Hello',         ' ',  'there',     ',',          ' ', 'Rstats',        '.'))

})
