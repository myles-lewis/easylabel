
#' Add labels to a plotly scatter plot
#' 
#' Adds labels to a plotly 2d or 3d scatter plot. The labels can be dragged.
#' 
#' Labels are identified by searching the rownames of the embedded marker data
#' within the plotly object. The plotly mode bar camera icon is set to export to
#' svg by default.
#' 
#' @param p A plotly scatter plot object
#' @param labs Character vector of labels to match
#' @returns A plotly plot with added labels
#' @export

add_labels <- function(p, labs) {
  getmode <- vapply(p$x$attr, "[[", character(1), "mode")
  mlayer <- which(getmode == "markers")
  if (length(mlayer) == 0) stop("no markers")
  if (length(mlayer) > 1) stop("more than 1 marker layer")
  
  dat <- p$x$visdat[[mlayer]]()
  x <- p$x$attrs[[mlayer]]$x
  x <- as.character(x)[2]
  x <- gsub("`", "", x)
  y <- p$x$attrs[[mlayer]]$y
  y <- as.character(y)[2]
  y <- gsub("`", "", y)
  z <- p$x$attrs[[mlayer]]$z
  
  if (any(miss <- !labs %in% rownames(dat))) {
    labs <- labs[!miss]
    if (length(labs) == 0) stop("no labels found", call. = FALSE)
    message("Labels not found: ", paste(labs[miss], collapse = ", "))
  }
  
  ind <- match(labs, rownames(dat))
  sx <- dat[ind, x]
  sy <- dat[ind, y]
  
  if (!is.null(z)) {
    # 3d plot
    z <- as.character(z)[2]
    z <- gsub("`", "", z)
    sz <- dat[ind, z]
    annot <- annot_labs(labs, sx, sy, sz)
    p <- p %>%
      layout(scene = list(annotations = annot)) %>%
      config(edits = list(annotationTail = TRUE),
             toImageButtonOptions = list(format = "svg"))
    return(p)
  } 
  # 2d plot
  annot <- annot_labs(labs, sx, sy)
  p <- p %>%
    layout(annotations = annot) %>%
    config(edits = list(annotationTail = TRUE),
           toImageButtonOptions = list(format = "svg"))
  p
}


annot_labs <- function(labs, sx, sy, sz = NULL) {
  lapply(seq_along(labs), function(i) {
    list(x = sx[i], y = sy[i], z = sz[i],
         text = labs[i], ax = 75, ay = 0,
         font = list(color = "black"),
         arrowwidth = 1, arrowhead = 0, arrowsize = 1.5,
         xanchor = "auto", yanchor = "auto")
  })
}


#' Change plotly font to Arial
#' 
#' @param p A plotly object
#' @param size Font size
#' @returns A plotly object
#' @export

arial <- function(p, size = 14) {
  p %>% layout(font = list(family = "arial", size = size))
} 
