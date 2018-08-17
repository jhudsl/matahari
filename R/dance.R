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
#' @importFrom tibble data_frame add_row
#' @export
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

    if(!(".dance" %in% ls(all.names = TRUE, envir = .GlobalEnv))) {
      setup_tbl <- data_frame(expr = list(quote(sessionInfo())),
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
             envir = get_env())
    } else {
      d <- get(".dance", envir = .GlobalEnv)
      assign(".dance", add_row(d,
                               expr = list(ie(expr, expr_, NA)),
                               value = list(ie(value, value_, NA)),
                               path = list(ie(path, ed$path, NA)),
                               contents = list(ie(contents, ed$contents, NA)),
                               selection = ie(selection, ed$selection, NA),
                               dt = Sys.time()
                               ), envir = get_env())
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
#' @return \code{TRUE} if logging was taking place, otherwise \code{FALSE} (invisibly).
dance_stop <- function() {
  add_session_info()
  invisible(removeTaskCallback("mh"))
}

#' Remove your logging history.
#'
#' @export
#' @return Either \code{TRUE} if the log was removed or \code{FALSE} if the log
#' does not exist (invisibly).
dance_remove <- function() {
  result <- tryCatch(rm(".dance", envir = .GlobalEnv),
                     warning = function(e){FALSE})
  invisible(is.null(result))
}

#' Get the log as a data frame
#'
#' @export
#' @return Either a data frame containing your logged history or \code{NULL}
#' if there is no log.
dance_tbl <- function() {
  result <- tryCatch(get(".dance", envir = .GlobalEnv),
           error = function(e){NULL})
  result
}

#' Save the log as an rds file.
#'
#' @param path The path to the rds file.
#' @importFrom readr write_rds
#' @export
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
dance_report <- function(...) {
  add_session_info()

  ellipsis <- list(...)

  if(!is.null(ellipsis$input)) {
    base64_to_df(ellipsis$input)
  } else {
    copy_base64(clip = ellipsis$clip)
  }
}

#' Create a matahari-esque data frame from a file.
#'
#' @param code A string or the path to a file containing R code.
#' @param eval Logical, indicating whether to evaluate the code, default is `TRUE`                     
#' @importFrom readr read_file
#' @importFrom rlang is_scalar_character abort parse_exprs .data
#' @importFrom dplyr data_frame "%>%" bind_cols mutate as_data_frame
#' @importFrom purrr map safely quietly transpose
#' @export
dance_recital <- function(code, eval = TRUE) {
  if (file.exists(code)) {
    code <- read_file(code)
  }

  if(!is_scalar_character(code)) {
    abort("`code` must be a file or a string containing R code")
  }
  
  if (!eval) { 
    return(data_frame(expr = parse_exprs(code)))
  }

  e <- new.env()

  data_frame(expr = parse_exprs(code)) %>%
    bind_cols(
      parse_exprs(code) %>%
        map(~safely(quietly(eval))(.x, envir = e)) %>%
        transpose() %>%
        as_data_frame() %>%
        mutate(output = map(.data$result, ~ .x$output)) %>%
        mutate(warnings = map(.data$result, ~ .x$warnings)) %>%
        mutate(messages = map(.data$result, ~ .x$messages)) %>%
        mutate(result = map(.data$result, ~ .x$result))
    )
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

get_env <- function(){
  .GlobalEnv
}

there_is_a_dance <- function() {
  ".dance" %in% ls(all.names = TRUE, envir = .GlobalEnv)
}

add_session_info <- function() {
  if (there_is_a_dance()) {
    d <- get(".dance", envir = .GlobalEnv)
    assign(".dance", add_row(d,
                             expr = list(quote(sessionInfo())),
                             value = list(sessionInfo()),
                             path = list(NA),
                             contents = list(NA),
                             selection = list(NA),
                             dt = Sys.time()
    ), envir = get_env())
  }
}
