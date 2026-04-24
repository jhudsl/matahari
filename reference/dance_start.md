# Start Spying

Start logging your R console activity. If you're using RStudio the
contents of your current editor tab will also be tracked. All logging
takes place when R code is executed in the R console.

## Usage

``` r
dance_start(
  expr = TRUE,
  value = FALSE,
  path = FALSE,
  contents = FALSE,
  selection = FALSE
)
```

## Arguments

- expr:

  The R expressions typed into the R console will be logged unless this
  is set to `FALSE`.

- value:

  Values that are computed on the R console will be logged unless this
  is set to `FALSE`.

- path:

  The path to the file in focus on the RStudio editor will be logged
  unless this is set to `FALSE`.

- contents:

  The file contents of the RStudio editor tab in focus will be logged
  unless this is set to `FALSE`.

- selection:

  The text that is highlighted in the RStudio editor tab in focus will
  be logged unless this is set to `FALSE`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Begin recording your R session.
dance_start()

# Each of the following expressions adds a row to the tibble that tracks
# the execution of your R code.
"Hello!"
4 + 4
x <- 7
x^2
rm(x)
x

# This stops recording your R session.
dance_stop()
} # }
```
