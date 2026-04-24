# Stop Spying

Pause the current logging session.

## Usage

``` r
dance_stop()
```

## Value

`TRUE` if logging was taking place, otherwise `FALSE` (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
# Begin recording your R session.
dance_start()

# Each expression adds a row to the tibble that tracks the execution of
# your R code.
x <- 7
x * 4

# This stops recording your R session.
dance_stop()
} # }
```
