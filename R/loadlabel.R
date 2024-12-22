
#' Load easylabel saved state
#' 
#' Loads a file or easylab object containing saved state with specified labels
#' and their positions.
#' 
#' @param object Either a character value specifying an rds file to be loaded,
#'   or an object of class 'easylab'.
#' @param data Dataset (data.frame or data.table) to use for plot. If not
#'   specified, the function will first use embedded data included in `object`;
#'   if no data is embedded it will attempt to load the original data object as
#'   it was specified in the original call to [easylabel()].
#' @param ... Additional arguments passed to [easylabel()]. Can be used to
#'   overwrite original arguments.
#' @returns By default no return value. Calls [easylabel()] which will open a
#'   shiny interactive session based on previous settings stored in an 'easylab'
#'   class object or rds file of such an object.
#' @export
#'
loadlabel <- function(object, data = NULL, ...) {
  if (is.character(object)) object <- readRDS(object)
  if (!inherits(object, "easylab")) stop("not an 'easylab' class object")
  
  dots <- list(...)
  args <- object$evalcall
  if (!is.null(data)) {
    args$data <- quote(data)
  } else {
    if (!is.null(object$data)) {
      args$data <- object$data
    } else if (exists(object$call$data)) {
      args$data <- object$call$data
      dataname <- as.character(object$call$data)
      message("Dataset: ", dataname)
      if (!inherits(eval(parse(text = dataname)), c("matrix", "data.frame")))
                    stop(object$call$data, " is not a dataframe or matrix")
    } else stop(object$call$data, " is missing")
  }
  args$startLabels <- object$startLabels
  args$start_xy <- object$start_xy
  if (length(dots)) args[names(dots)] <- dots
  
  do.call("easylabel", args)
}


#' Summarise easylabel saved state object
#' 
#' Prints summary information on an easylabel saved state object.
#' 
#' @param object Object of class 'easylab'
#' @param ... Optional arguments for compatibility
#' @returns No return value. Prints information on the original easylabel call
#'   and saved labels.
#' @export
summary.easylab <- function(object, ...) {
  obcall <- deparse(object$call)
  cat("Call:", paste(obcall, collapse = "\n"), "\n")
  cat("Labels:", paste(object$labelnames, collapse = ", "), "\n")
  if (is.null(object$data)) {
    cat("No embedded data\n")
  } else {
    cat("Embedded data:", nrow(object$data), "rows\n")
  }
}
