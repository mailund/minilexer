
`minilexer`: A Simple Lexer in R
==============================================================================





`minilexer` provides some tools for simple tokenising/lexing and parsing text files.

Note: For complicated parsing (especially of computer programs) you'll probably want to use the more formally correct lexing/parsing provided by [the `rly` package on CRAN](https://cran.r-project.org/package=rly).

Installation
-----------------------------------------------------------------------------

```r
devtools::install_bitbucket('coolbutuseless/minilexer')
```



Package Overview
-----------------------------------------------------------------------------

Current there is just one function provided: `minilexer::lex(text, patterns)`.

This function uses the user-defined regular expressions (`patterns`) to split 
`text` into a character vector of tokens.

The `patterns` argument is a named vector of character strings representing regular
expressions for elements to match within the text.  


ToDo
-----------------------------------------------------------------------------

* Add some parsing helpers
* Add vignettes for parsing some data formats, e.g.
    * chess games `.pgn`
    * scrabble games `.gcg`



Example: Lex a sentence into tokens
-----------------------------------------------------------------------------

```r
sentence_patterns <- c(
  word        = "\\w+", 
  whitespace  = "\\s+",
  fullstop    = "\\.",
  comma       = "\\,"
)

sentence = "Hello there, Rstats."

lex(sentence, sentence_patterns)
```

```
##       word whitespace       word      comma whitespace       word 
##    "Hello"        " "    "there"        ","        " "   "Rstats" 
##   fullstop 
##        "."
```

Example: Lex an SVG `points` attribute into tokens
-----------------------------------------------------------------------------

* The SVG `points` attribute is a single string containing numbers, spaces and commas
* SVG is very forgiving on whitespace and leading zeros, so the following are all valid pairs of numbers
    * `0.2,0.3`
    * `.2.3` 
    * `.2-.3`


```r
svg_points_patterns <- c(
  number      = "-?\\d*\\.?\\d+",
  whitespace  = "\\s+",
  sep         = ","
)

svg_points <- "1-2 3-4,.5.3"

lex(svg_points, svg_points_patterns)  
```

```
##     number     number whitespace     number     number        sep 
##        "1"       "-2"        " "        "3"       "-4"        "," 
##     number     number 
##       ".5"       ".3"
```


Example: Lex some simplified R code into tokens
-----------------------------------------------------------------------------


```r
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

lex(R_code, R_patterns)
```

```
##       name whitespace     assign whitespace     number whitespace 
##        "x"        " "       "<-"        " "        "3"        " " 
##       plus whitespace     number whitespace       plus whitespace 
##        "+"        " "      "4.2"        " "        "+"        " " 
##       name   lbracket     number   rbracket 
##    "rnorm"        "("        "1"        ")"
```


