
#' Add labels to a plotly scatter plot
#' 
#' @param p A plotly scatter plot object
#' @param labs Character vector of labels to match
#' @returns A plotly plot with added labels
#' @export

add_labels <- function(p, labs) {
  dat <- p$x$visdat[[1]]()
  x <- p$x$attrs[[1]]$x
  x <- as.character(x)[2]
  x <- gsub("`", "", x)
  y <- p$x$attrs[[1]]$y
  y <- as.character(y)[2]
  y <- gsub("`", "", y)
  z <- p$x$attrs[[1]]$z
  
  if (any(miss <- !labs %in% rownames(dat))) {
    labs <- labs[!miss]
    if (length(labs) == 0) stop("No labels found", call. = FALSE)
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


#' Change plotly font to arial
#' 
#' @param p A plotly object
#' @param size Font size
#' @returns A plotly object
#' @export

arial <- function(p, size = 14) {
  p %>% layout(font = list(family = "arial", size = size))
} 
