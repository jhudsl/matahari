# Create a matahari-esque data frame from a file.

Create a matahari-esque data frame from a file.

## Usage

``` r
dance_recital(code, evaluate = TRUE)
```

## Arguments

- code:

  A string or the path to a file containing R code.

- evaluate:

  Logical, indicating whether to evaluate the code, default is `TRUE`

## Examples

``` r
if (FALSE) { # \dontrun{

library(knitr)

# Evaluate a string of R code
kable(dance_recital("x <- 4; x *7"))

## |expr   |value |error |output |warnings     |messages     |
## |:------|:-----|:-----|:------|:------------|:------------|
## |x <- 4 |4     |NULL  |       |character(0) |character(0) |
## |x * 7  |28    |NULL  |       |character(0) |character(0) |

# Evaluate an R script. We have provided an R script for testing purposes.
code_file <- system.file("test", "sample_code.R", package = "matahari")
kable(dance_recital(code_file)[,1:3])

## |expr                |value    |error                                    |
## |:-------------------|:--------|:----------------------------------------|
## |4 + 4               |8        |NULL                                     |
## |wow!                |wow!     |NULL                                     |
## |mean(1:10)          |5.5      |NULL                                     |
## |stop("Error!")      |NULL     |list(message = "Error!", call = .f(...)) |
## |warning("Warning!") |Warning! |NULL                                     |
## |message("Hello?")   |NULL     |NULL                                     |
## |cat("Welcome!")     |NULL     |NULL   
} # }                                  |
```
