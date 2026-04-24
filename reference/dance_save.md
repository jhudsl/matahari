# Save the log as an rds file.

Save the log as an rds file.

## Usage

``` r
dance_save(path)
```

## Arguments

- path:

  The path to the rds file.

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

# Save your log locally
dance_save("session.rds")
} # }
```
