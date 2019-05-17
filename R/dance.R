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
#' @export
#' @examples
#' \dontrun{
#'
#' dance_start()
#' "Hello!"
#' 4 + 4
#' x <- 7
#' x^2
#' rm(x)
#' x
#' dance_stop()
#' }
dance_start <- function(expr = TRUE, value = FALSE, path = FALSE, contents = FALSE,
                        selection = FALSE) {
  cb <- function(expr_, value_, ok, visible){
    editorIsOpen <- tryCatch({getSourceEditorContext();TRUE},
                             error = function(e) FALSE)

    if(editorIsOpen && isAvailable()){
      ed <- getSourceEditorContext()
    } else {
      ed <- list(path = NA, contents = NA, selection = NA)
    }

    if(!(".dance" %in% ls(all.names = TRUE, envir = env))) {
      setup_tbl <- tibble(expr = list(quote(sessionInfo())),
                          value = list(sessionInfo()),
                          path = list(ie(path, ed$path, NA)),
                          contents = list(ie(contents, ed$contents, NA)),
                          selection = ie(selection, ed$selection, NA),
                          dt = Sys.time())
      setup_tbl <- add_row(setup_tbl,
                           expr = list(ie(expr, expr_, NA)),
                           value = list(ie(value, value_, NA)),
                           path = list(ie(path, ed$path, NA)),
                           contents = list(ie(contents, ed$contents, NA)),
                           selection = ie(selection, ed$selection, NA),
                           dt = Sys.time())
      assign(".dance",
             setup_tbl,
             envir = env)
    } else {
      d <- get(".dance", envir = env)
      assign(".dance", add_row(d,
                               expr = list(ie(expr, expr_, NA)),
                               value = list(ie(value, value_, NA)),
                               path = list(ie(path, ed$path, NA)),
                               contents = list(ie(contents, ed$contents, NA)),
                               selection = ie(selection, ed$selection, NA),
                               dt = Sys.time()
      ), envir = env)
    }
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
#'
#' dance_start()
#' "Hello!"
#' 4 + 4
#' x <- 7
#' x^2
#' rm(x)
#' x
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
#'
#' dance_start()
#' "Hello!"
#' 4 + 4
#' dance_stop()
#' dance_tbl()
#' dance_remove()
#' dance_tbl()
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

#' Get the log as a data frame
#'
#' @export
#' @examples
#' \dontrun{
#'
#' dance_start()
#' "Hello!"
#' 4 + 4
#' dance_stop()
#' dance_tbl()
#' }
#' @return Either a data frame containing your logged history or \code{NULL}
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
#' dance_start()
#' "Hello!"
#' 4 + 4
#' dance_stop()
#' dance_save("session.rds")
#' }
dance_save <- function(path) {
  add_session_info()

  tbl <- dance_tbl()
  write_rds(tbl, path)
}

ie <- function(cond, t, f){
  if(cond){
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
#' dance_start()
#' "Hello!"
#' 4 + 4
#' dance_stop()
#' dance_report()
#' }
dance_report <- function(...) {
  add_session_info()

  ellipsis <- list(...)

  if(!is.null(ellipsis$input)) {
    invisible(base64_to_df(ellipsis$input))
  } else {
    invisible(copy_base64(clip = ellipsis$clip))
  }
}

#' Create a matahari-esque data frame from a file.
#'
#' @param code A string or the path to a file containing R code.
#' @param evaluate Logical, indicating whether to evaluate the code, default is `TRUE`
#' @importFrom readr read_file
#' @importFrom rlang is_scalar_character abort parse_exprs .data
#' @importFrom tibble tibble as_tibble add_column
#' @importFrom purrr map safely quietly transpose "%>%"
#' @export
#' @examples
#' code_file <- system.file("test", "sample_code.R", package = "matahari")
#' dance_recital(code_file)
dance_recital <- function(code, evaluate = TRUE) {
  if (file.exists(code)) {
    code <- read_file(code)
  }

  if(!is_scalar_character(code)) {
    abort("`code` must be a file or a string containing R code")
  }

  if (!evaluate) {
    return(tibble(expr = parse_exprs(code),
                  value = list(NULL),
                  error = list(NULL),
                  output = list(NULL),
                  warnings = list(NULL),
                  message = list(NULL)))
  }

  e <- new.env()

  r <- parse_exprs(code) %>%
    map(~safely(quietly(eval))(.x, envir = e)) %>%
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

add_session_info <- function() {
  if (there_is_a_dance()) {
    d <- get(".dance", envir = env)
    assign(".dance", add_row(d,
                             expr = list(quote(sessionInfo())),
                             value = list(sessionInfo()),
                             path = list(NA),
                             contents = list(NA),
                             selection = list(NA),
                             dt = Sys.time()
    ), envir = env)
  }
}
