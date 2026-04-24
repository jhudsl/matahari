# Remove your logging history.

Remove your logging history.

## Usage

``` r
dance_remove()
```

## Value

Either `TRUE` if the log was removed or `FALSE` if the log does not
exist (invisibly).

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
kable(dance_tbl()[3:4, 1:5])

## |expr   |value  |path |contents |selection |
## |:------|:------|:----|:--------|:---------|
## |Hello! |Hello! |NA   |NA       |NA        |
## |4 + 4  |8      |NA   |NA       |NA        |

# Deletes the lgo of your session
dance_remove()

# With no log, dance_tbl() returns NULL invisibly.
withVisible(dance_tbl())

## $value
## NULL
##
## $visible
## FALSE

} # }
```
