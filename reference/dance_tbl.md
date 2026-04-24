# Get the log as a tibble

Get the log as a tibble

## Usage

``` r
dance_tbl()
```

## Value

Either a
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
containing your logged history or `NULL`. if there is no log.

## Examples

``` r
if (FALSE) { # \dontrun{

library(knitr)

# Start recording
dance_start(value = TRUE)

# Execute some expressions
"Hello!"
4 + 4

# Stop recording
dance_stop()

# Access log of your session
dance_tbl()

# Display the log nicely
kable(dance_tbl()[3:4, 1:5])

## |expr   |value  |path |contents |selection |
## |:------|:------|:----|:--------|:---------|
## |Hello! |Hello! |NA   |NA       |NA        |
## |4 + 4  |8      |NA   |NA       |NA        |

} # }
```
