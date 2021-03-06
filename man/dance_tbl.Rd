% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dance.R
\name{dance_tbl}
\alias{dance_tbl}
\title{Get the log as a tibble}
\usage{
dance_tbl()
}
\value{
Either a \code{\link[tibble:tibble]{tibble::tibble()}} containing your logged history or \code{NULL}.
if there is no log.
}
\description{
Get the log as a tibble
}
\examples{
\dontrun{

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

}
}
