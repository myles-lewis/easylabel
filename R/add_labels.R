
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
#' @param plotGlPixelRatio Integer passed to plotly.js config which controls
#'   pixel resolution of webGl rendering. Larger values increase resolution of
#'   points as well as file size.
#' @returns A plotly plot with added labels
#' @examples
#' library(plotly)
#' data(mtcars)
#' 
#' p <- plot_ly(mtcars, x = ~mpg, y = ~wt, color = ~cyl,
#'              type = 'scatter', mode = 'markers')
#' p %>% add_labels(c("Mazda RX4", "Fiat 128"))
#' @export

add_labels <- function(p, labs,
                       plotGlPixelRatio = 8) {
  getmode <- vapply(p$x$attr, "[[", character(1), "mode")
  mlayer <- names(getmode)[which(getmode == "markers")]
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
    
    # check for existing annotation
    if ("annotations" %in% names(p$x$layoutAttrs[[mlayer]]$scene)) {
      annot1 <- p$x$layoutAttrs[[mlayer]]$scene$annotations
      p$x$layoutAttrs[[mlayer]]$scene$annotations <- NULL
      annot <- c(annot1, annot)
    }
    
    p <- p %>%
      layout(scene = list(annotations = annot)) %>%
      config(edits = list(annotationTail = TRUE),
             plotGlPixelRatio = plotGlPixelRatio,
             toImageButtonOptions = list(format = "svg"))
    return(p)
  }
  # 2d plot
  annot <- annot_labs(labs, sx, sy)
  
  p %>%
    layout(annotations = annot) %>%
    config(edits = list(annotationTail = TRUE),
           plotGlPixelRatio = plotGlPixelRatio,
           toImageButtonOptions = list(format = "svg"))
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
#' @examples
#' library(plotly)
#' data(mtcars)
#' 
#' p <- plot_ly(mtcars, x = ~mpg, y = ~wt, color = ~cyl,
#'              type = 'scatter', mode = 'markers')
#' p %>% arial
#' p %>% arial(18)
#' @export

arial <- function(p, size = 14) {
  p %>% layout(font = list(family = "arial", size = size, color = "black"))
} 
