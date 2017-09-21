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
dance_start <- function(expr = TRUE, value = TRUE, path = TRUE, contents = TRUE,
                        selection = TRUE) {
  cb <- function(expr_, value_, ok, visible){
    if(isAvailable()){
      ed <- getSourceEditorContext()
    } else {
      ed <- list(path = NA, contents = NA, selection = NA)
    }

    if(!(".dance" %in% ls(all.names = TRUE, envir = .GlobalEnv))) {
      assign(".dance",
             data_frame(expr = list(ie(expr, expr_, NA)),
                        value = list(ie(value, value_, NA)),
                        path = list(ie(path, ed$path, NA)),
                        contents = list(ie(contents, ed$contents, NA)),
                        selection = ie(selection, ed$selection, NA),
                        dt = Sys.time()),
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

get_env <- function(){
  .GlobalEnv
}
