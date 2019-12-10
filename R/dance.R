#' Start Spying
#'
#' Start logging your R console activity. If you're using RStudio the contents
#' of your current editor tab will also be tracked. All logging takes place when
#' R code is executed in the R console.
#'
#' @param expr The R expressions typed into the R console will be logged unless
#' this is set to `FALSE`.
#' @param value Values that are computed on the R console will be logged unless
#' this is set to `FALSE`.
#' @param path The path to the file in focus on the RStudio editor will be logged
#' unless this is set to `FALSE`.
#' @param contents The file contents of the RStudio editor tab in focus will be
#' logged unless this is set to `FALSE`.
#' @param selection The text that is highlighted in the RStudio editor tab in
#' focus will be logged unless this is set to `FALSE`.
#' @importFrom rstudioapi isAvailable getSourceEditorContext
#' @importFrom tibble tibble add_row
#' @importFrom rlang abort
#' @export
#' @examples
#' \dontrun{
#' # Begin recording your R session.
#' dance_start()
#'
#' # Each of the following expressions adds a row to the tibble that tracks
#' # the execution of your R code.
#' "Hello!"
#' 4 + 4
#' x <- 7
#' x^2
#' rm(x)
#' x
#'
#' # This stops recording your R session.
#' dance_stop()
#' }
dance_start <- function(expr = TRUE, value = FALSE, path = FALSE, contents = FALSE,
                        selection = FALSE) {
  if (there_is_a_dance()) {
    abort("Unable to start new dance while a dance is in progress.")
  }

  cb <- function(expr_, value_, ok, visible) {
    editorIsOpen <- tryCatch({
      getSourceEditorContext()
      TRUE
    },
    error = function(e) FALSE
    )

    if (editorIsOpen && isAvailable()) {
      ed <- getSourceEditorContext()
    } else {
      ed <- list(path = NA, contents = NA, selection = NA)
    }

    if (!there_is_a_dance()) {
      choreograph_dance()
      add_session_info(
        path_ = ie(path, ed$path, NA),
        contents_ = ie(contents, ed$contents, NA),
        selection_ = ie(selection, ed$selection, NA)
      )
    }

    d <- get(".dance", envir = env)
    assign(".dance", add_row(d,
      expr = list(ie(expr, expr_, NA)),
      value = list(ie(value, value_, NA)),
      path = list(ie(path, ed$path, NA)),
      contents = list(ie(contents, ed$contents, NA)),
      selection = ie(selection, ed$selection, NA),
      dt = Sys.time()
    ), envir = env)
    TRUE
  }

  invisible(addTaskCallback(cb, name = "mh"))
}

#' Stop Spying
#'
#' Pause the current logging session.
#'
#' @export
#' @examples
#' \dontrun{
#' # Begin recording your R session.
#' dance_start()
#'
#' # Each expression adds a row to the tibble that tracks the execution of
#' # your R code.
#' x <- 7
#' x * 4
#'
#' # This stops recording your R session.
#' dance_stop()
#' }
#' @return \code{TRUE} if logging was taking place, otherwise \code{FALSE} (invisibly).
dance_stop <- function() {
  add_session_info()
  invisible(removeTaskCallback("mh"))
}

#' Remove your logging history.
#'
#' @export
#' @examples
#' \dontrun{
#' library(knitr)
#'
#' # Start recording
#' dance_start(value = TRUE)
#'
#' # Execute some expressions
#' "Hello!"
#' 4 + 4
#'
#' # Stop recording
#' dance_stop()
#'
#' # Access log of your session
#' kable(dance_tbl()[3:4, 1:5])
#'
#' ## |expr   |value  |path |contents |selection |
#' ## |:------|:------|:----|:--------|:---------|
#' ## |Hello! |Hello! |NA   |NA       |NA        |
#' ## |4 + 4  |8      |NA   |NA       |NA        |
#'
#' # Deletes the lgo of your session
#' dance_remove()
#'
#' # With no log, dance_tbl() returns NULL invisibly.
#' withVisible(dance_tbl())
#'
#' ## $value
#' ## NULL
#' ##
#' ## $visible
#' ## FALSE
#'
#' }
#' @return Either \code{TRUE} if the log was removed or \code{FALSE} if the log
#' does not exist (invisibly).
dance_remove <- function() {
  result <- FALSE
  if (exists(".dance", envir = env)) {
    result <- rm(".dance", envir = env)
  }
  invisible(is.null(result))
}

#' Get the log as a tibble
#'
#' @export
#' @examples
#' \dontrun{
#'
#' library(knitr)
#'
#' # Start recording
#' dance_start(value = TRUE)
#'
#' # Execute some expressions
#' "Hello!"
#' 4 + 4
#'
#' # Stop recording
#' dance_stop()
#'
#' # Access log of your session
#' dance_tbl()
#'
#' # Display the log nicely
#' kable(dance_tbl()[3:4, 1:5])
#'
#' ## |expr   |value  |path |contents |selection |
#' ## |:------|:------|:----|:--------|:---------|
#' ## |Hello! |Hello! |NA   |NA       |NA        |
#' ## |4 + 4  |8      |NA   |NA       |NA        |
#'
#' }
#' @return Either a [tibble::tibble()] containing your logged history or \code{NULL}.
#' if there is no log.
dance_tbl <- function() {
  if (exists(".dance", envir = env)) {
    get(".dance", envir = env)
  }
}

#' Save the log as an rds file.
#'
#' @param path The path to the rds file.
#' @importFrom readr write_rds
#' @export
#' @examples
#' \dontrun{
#'
#' # Start recording
#' dance_start(value = TRUE)
#'
#' # Execute some expressions
#' "Hello!"
#' 4 + 4
#'
#' # Stop recording
#' dance_stop()
#'
#' # Save your log locally
#' dance_save("session.rds")
#' }
dance_save <- function(path) {
  add_session_info()

  tbl <- dance_tbl()
  write_rds(tbl, path)
}

ie <- function(cond, t, f) {
  if (cond) {
    t
  } else {
    f
  }
}

#' Copy the log to your clipboard.
#'
#' @param ... Developer options.
#' @export
#' @examples
#' \dontrun{
#'
#' # Start recording
#' dance_start(value = TRUE)
#'
#' # Execute some expressions
#' "Hello!"
#' 4 + 4
#'
#' # Stop recording
#' dance_stop()
#'
#' # Assign a base64 encoded tibble of your log to a variable
#' report_code <- dance_report()
#' substr(report_code, 1, 10)
#'
#' ## "WAoAAAADAA"
#'
#' nchar(report_code)
#'
#' ## 397432
#' }
dance_report <- function(...) {
  ellipsis <- list(...)

  if (!is.null(ellipsis$input)) {
    invisible(base64_to_df(ellipsis$input))
  } else {
    add_session_info()
    invisible(copy_base64(clip = ellipsis$clip))
  }
}

#' Create a matahari-esque data frame from a file.
#'
#' @param code A string or the path to a file containing R code.
#' @param evaluate Logical, indicating whether to evaluate the code, default is `TRUE`
#' @importFrom readr read_file
#' @importFrom rlang is_scalar_character abort parse_exprs .data warn
#' @importFrom tibble tibble as_tibble add_column
#' @importFrom purrr map safely quietly transpose "%>%"
#' @export
#' @examples
#'
#' library(knitr)
#'
#' # Evaluate a string of R code
#' kable(dance_recital("x <- 4; x *7"))
#'
#' ## |expr   |value |error |output |warnings     |messages     |
#' ## |:------|:-----|:-----|:------|:------------|:------------|
#' ## |x <- 4 |4     |NULL  |       |character(0) |character(0) |
#' ## |x * 7  |28    |NULL  |       |character(0) |character(0) |
#'
#' # Evaluate an R script. We have provided an R script for testing purposes.
#' code_file <- system.file("test", "sample_code.R", package = "matahari")
#' kable(dance_recital(code_file)[,1:3])
#'
#' ## |expr                |value    |error                                    |
#' ## |:-------------------|:--------|:----------------------------------------|
#' ## |4 + 4               |8        |NULL                                     |
#' ## |wow!                |wow!     |NULL                                     |
#' ## |mean(1:10)          |5.5      |NULL                                     |
#' ## |stop("Error!")      |NULL     |list(message = "Error!", call = .f(...)) |
#' ## |warning("Warning!") |Warning! |NULL                                     |
#' ## |message("Hello?")   |NULL     |NULL                                     |
#' ## |cat("Welcome!")     |NULL     |NULL                                     |
dance_recital <- function(code, evaluate = TRUE) {
  file_exists <- file.exists(code)

  if (file_exists) {
    code <- read_file(code)
  } else if (grepl("\\.[R|r]$", code)) {
    warn(paste("R code file", code, "does not exist."))
  }

  if (!is_scalar_character(code)) {
    abort("`code` must be a file or a string containing R code")
  }

  if (!evaluate) {
    return(tibble(
      expr = parse_exprs(code),
      value = list(NULL),
      error = list(NULL),
      output = list(NULL),
      warnings = list(NULL),
      message = list(NULL)
    ))
  }

  e <- new.env()

  r <- parse_exprs(code) %>%
    map(~ safely(quietly(eval))(.x, envir = e)) %>%
    transpose() %>%
    as_tibble()

  tibble(expr = parse_exprs(code)) %>%
    add_column(value = map(r$result, ~ .x$result)) %>%
    add_column(error = r$error) %>%
    add_column(output = map(r$result, ~ .x$output)) %>%
    add_column(warnings = map(r$result, ~ .x$warnings)) %>%
    add_column(messages = map(r$result, ~ .x$messages))
}

#' @importFrom jsonlite base64_enc
#' @importFrom clipr write_clip
copy_base64 <- function(clip) {
  enc_string <- base64_enc(serialize(dance_tbl(), NULL))
  if (!is.null(clip) && clip) {
    write_clip(enc_string)
  } else {
    enc_string
  }
}

#' @importFrom jsonlite base64_dec
base64_to_df <- function(string) {
  unserialize(base64_dec(string))
}

env <- new.env(parent = emptyenv())

there_is_a_dance <- function() {
  ".dance" %in% ls(all.names = TRUE, envir = env)
}

add_session_info <- function(path_ = NA, contents_ = NA, selection_ = NA) {
  if (is.na(selection_)) {
    selection_ <- list(NA)
  }

  if (there_is_a_dance()) {
    d <- get(".dance", envir = env)
    assign(".dance", add_row(d,
      expr = list(quote(sessionInfo())),
      value = list(sessionInfo()),
      path = list(path_),
      contents = list(contents_),
      selection = selection_,
      dt = Sys.time()
    ), envir = env)
  }
}

choreograph_dance <- function() {
  assign(".dance",
    tibble(
      expr = list(), value = list(), path = list(),
      contents = list(), selection = list(),
      dt = structure(0, class = c("POSIXct", "POSIXt"), tzone = "")
    ),
    envir = env
  )
}
