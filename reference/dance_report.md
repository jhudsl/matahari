# Copy the log to your clipboard.

Copy the log to your clipboard.

## Usage

``` r
dance_report(...)
```

## Arguments

- ...:

  Developer options.

## Examples

``` r
if (FALSE) { # \dontrun{

# Start recording
dance_start(value = TRUE)

# Execute some expressions
"Hello!"
4 + 4

# Stop recording
dance_stop()

# Assign a base64 encoded tibble of your log to a variable
report_code <- dance_report()
substr(report_code, 1, 10)

## "WAoAAAADAA"

nchar(report_code)

## 397432
} # }
```
