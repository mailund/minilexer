
*minilexer*: A Simple Lexer in R
==============================================================================





`minilexer` provides some tools for simple tokenising/lexing and parsing text files.  

`minilexer` aims to be great at helping to get unsupported text data formats into R *fast*. 

For complicated parsing (especially of computer programs) you'll want to use the more formally correct lexing/parsing provided by the [`rly` package](https://cran.r-project.org/package=rly) or the [`dparser` package](https://cran.r-project.org/package=dparser).

Installation
-----------------------------------------------------------------------------

```r
devtools::install_bitbucket('coolbutuseless/minilexer')
```



Package Overview
-----------------------------------------------------------------------------


Current the package provides one function, and one R6 class: 

* `minilexer::lex(text, patterns)` for splitting the text into tokens.
    * This function uses the user-defined regular expressions (`patterns`) to split 
      `text` into a character vector of tokens.
    * The `patterns` argument is a named vector of character strings representing regular
      expressions for elements to match within the text.  
* `minilexer::TokenStream` is a class to handle manipulation/interrogation of the stream of tokens to
  make it easier to write parsers.




Introducing the `minilexer` package
-----------------------------------------------------------------------------

`minilexer` provides some tools for simple tokenising/lexing and parsing text files.

I will emphasise the **mini** in `minilexer` as this is not a rigorous or formally complete lexer, but it
suits 90% of my needs for turning data text formats into tokens.

For complicated parsing (especially of computer programs) you'll probably want to use the more formally correct lexing/parsing provided by the [`rly` package](https://cran.r-project.org/package=rly) or the [`dparser` package](https://cran.r-project.org/package=dparser).



Installation
-----------------------------------------------------------------------------

```r
devtools::install_bitbucket('coolbutuseless/minilexer')
```



Package Overview
-----------------------------------------------------------------------------

Current the package provides just one function and one R6 class: 

* `minilexer::lex(text, patterns)` for splitting the text into tokens.
    * This function uses the user-defined regular expressions (`patterns`) to split 
      `text` into a character vector of tokens.
    * The `patterns` argument is a named vector of character strings representing regular
      expressions for elements to match within the text.  
* `minilexer::TokenStream` is a class to handle manipulation/interrogation of the stream of tokens to
  make it easier to write parsers.


Example: Use `lex()` to split sentence into tokens
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



Example: Use `lex()` to split some simplified R code into tokens
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

R_tokens <- lex(R_code, R_patterns)
R_tokens
```

```
##       name whitespace     assign whitespace     number whitespace 
##        "x"        " "       "<-"        " "        "3"        " " 
##       plus whitespace     number whitespace       plus whitespace 
##        "+"        " "      "4.2"        " "        "+"        " " 
##       name   lbracket     number   rbracket 
##    "rnorm"        "("        "1"        ")"
```


Example: Use `TokenStream` to interrogate/manipulate the tokens 
-----------------------------------------------------------------------------

The `TokenStream` class is a way of manipulating a stream of tokens to make it 
easier(*) to write parsers.  It is a way of keeping track of which token we are 
currently looking at, and making assertions about the current token's value and type.

In the following examples, I'll be using the `R_tokens` I extracted above.



```r
# create the stream to handle the tokens
stream <- minilexer::TokenStream$new(R_tokens)

# What position are we at?
stream$position
```

```
## [1] 1
```

```r
# Assert that the first token is a name and has the value 'x'
stream$expect_value('x')
stream$expect_type('name')

# Show what happens if the current token isn't what we expect
stream$expect_value('+')
```

```
## Error: Expected [+] at position 1 but found [name]: "x"
```

```r
# Try and consume this token and move onto the next one, but
# because the 'type' is incorrect, will result in failure
stream$consume_token(type='number')
```

```
## Error: Expected [number] at position 1 but found [name]: "x"
```

```r
# Unconditionally consume this token without regard to 
# its value or type. This returns the value at the 
# current position, and then increments the position
stream$consume_token()
```

```
## [1] "x"
```

```r
# Stream position should have moved on to the second value
stream$position
```

```
## [1] 2
```

```r
# Get the current value, but without advancing the position
stream$current_value()
```

```
## [1] " "
```

```r
# consume it. i.e. return current value and increment position
stream$consume_token(type='whitespace')
```

```
## [1] " "
```

```r
# Stream position should have moved on to the third value
stream$position
```

```
## [1] 3
```

```r
# Get the current value
stream$current_value()
```

```
## [1] "<-"
```

```r
stream$current_type()
```

```
## [1] "assign"
```




