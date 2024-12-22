
main_plotly <- function(plotly_data, x, y, 
                        col, colScheme,
                        shape, psymbols, outlier_psymbol,
                        sizeSwitch, size, sizeRange,
                        showOutliers, pt,
                        pmarker,
                        hovertext, pkey,
                        width, height,
                        args,
                        pxaxis, pyaxis,
                        annot,
                        legendxy, showLegend,
                        pshapes) {
  switch(showOutliers,
         # no outliers
         plot_ly(data = plotly_data, x = as.formula(paste0('~', x)),
                 y = as.formula(paste0('~', y)),
                 type = switch(pt, 'scattergl', 'scatter'),
                 mode = 'markers',
                 color = if (!is.null(col)) {
                   as.formula(paste0('~', col))
                 } else I(colScheme),
                 colors = colScheme,
                 size = switch(sizeSwitch, I(size), ~plotly_size),
                 marker = pmarker,
                 sizes = sizeRange,
                 symbol = if (!is.null(shape)) {
                   as.formula(paste0('~', shape))
                 } else I(psymbols),
                 symbols = psymbols,
                 text = hovertext, hoverinfo = 'text',
                 key = pkey, source = 'lab_plotly',
                 width = width, height = height) %>%
           layout(
             title = args$main,
             xaxis = pxaxis,
             yaxis = pyaxis),
         # with outliers
         plot_ly(data = plotly_data[!plotly_data$outlier,],
                 x = as.formula(paste0('~', x)),
                 y = as.formula(paste0('~', y)),
                 type = switch(pt, 'scattergl', 'scatter'),
                 mode = 'markers',
                 color = if (!is.null(col)) {
                   as.formula(paste0('~', col))
                 } else I(colScheme),
                 colors = colScheme,
                 size = switch(sizeSwitch, I(size), ~plotly_size),
                 marker = pmarker,
                 symbol = if (!is.null(shape)) {
                   ~comb_symbol
                 } else I(psymbols),
                 symbols = psymbols,
                 text = hovertext[!plotly_data$outlier], hoverinfo = 'text',
                 key = pkey[!plotly_data$outlier], source = 'lab_plotly',
                 legendgroup = 'Main',
                 width = width, height = height) %>%
           add_markers(data = plotly_data[plotly_data$outlier, ],
                       x = as.formula(paste0('~', x)),
                       y = as.formula(paste0('~', y)),
                       type = switch(pt, 'scattergl', 'scatter'),
                       color = as.formula(paste0('~', col)),
                       colors = colScheme,
                       marker = pmarker,
                       symbol = if (!is.null(shape)) {
                         ~comb_symbol
                       } else I(outlier_psymbol),
                       symbols = psymbols,
                       inherit = F,
                       text = hovertext[plotly_data$outlier],
                       hoverinfo = 'text',
                       key = pkey[plotly_data$outlier],
                       legendgroup = 'outlier', name = 'outlier') %>%
           layout(
             title = args$main,
             xaxis = pxaxis,
             yaxis = pyaxis)
  ) %>%
    layout(annotations = annot,
           hovermode = 'closest',
           legend = list(font = list(color = 'black'),
                         x = legendxy[1], y = legendxy[2]),
           showlegend = showLegend,
           shapes = pshapes) %>%
    config(edits = list(annotationTail = TRUE),
           toImageButtonOptions = list(format = "svg")) %>%
    event_register(event = 'plotly_click')
}


labDir_choices <- c('radial', 'origin', 'horiz', 'vert', 'xellipse',
                    'yellipse', 'rect', 'x', 'oct')
names(labDir_choices) <- c('Radial centre', 'Radial origin',
                           'Horizontal', 'Vertical',
                           'Horizontal ellipse', 'Vertical ellipse',
                           'Rectilinear', 'Diagonal', 'Octagonal')

# Annotate gene labels
annotation <- function(labels, data, x, y, current_xy = NULL,
                       labelchoices,
                       labSize, labelDir = "radial",
                       labCentre = c(0, 0), xyspan = c(1, 1),
                       lineLength,
                       col, colScheme, text_col = 'black', line_col = 'black') {
  if (length(labels) == 0) return(list())
  row <- data[as.numeric(labels), ]
  if (!is.null(col)) datcol <- colScheme[row[, col]]
  sx <- row[, x]
  sy <- row[, y]
  dx <- (sx - labCentre[1]) / xyspan[1]
  dy <- (sy - labCentre[2]) / xyspan[2]
  z <- sqrt(dx^2 + dy^2)
  if (labelDir == 'radial') {
    ax <- dx/z * lineLength
    ay <- -dy/z * lineLength
  } else if (labelDir == 'origin') {
    ox <- sx / xyspan[1]
    oy <- sy / xyspan[2]
    z <- sqrt(ox^2 + oy^2)
    ax <- ox/z * lineLength
    ay <- -oy/z * lineLength
  } else if (labelDir == 'xellipse') {
    dy <- dy / 4
    z <- sqrt(dx^2 + dy^2)
    ax <- dx/z * lineLength
    ay <- -dy/z * lineLength
  } else if (labelDir == 'yellipse') {
    dx <- dx / 5
    z <- sqrt(dx^2 + dy^2)
    ax <- dx/z * lineLength
    ay <- -dy/z * lineLength
  } else if (labelDir == 'horiz') {
    ax <- sign(dx) * lineLength
    ay <- rep.int(0, length(labels))
  } else if (labelDir == 'vert') {
    ax <- rep.int(0, length(labels))
    ay <- -sign(dy) * lineLength
  } else if (labelDir == 'x') {
    ax <- sign(dx) * lineLength
    ay <- -sign(dy) * lineLength
  } else if (labelDir == 'rect') {
    ax <- ifelse(abs(dx) > abs(dy), sign(dx) * lineLength, 0)
    ay <- ifelse(abs(dx) > abs(dy), 0, -sign(dy) * lineLength)
  } else if (labelDir == 'oct') {
    ang <- atan2(dy, dx)
    ang <- round(ang * 4 / pi)
    ax <- cospi(ang / 4) * lineLength
    ay <- -sinpi(ang / 4) * lineLength
  }
  lapply(seq_along(labels), function(j) {
    i <- labels[j]
    if (j <= length(current_xy)) {
      if (!is.null(current_xy[[j]])) {
        ax[j] <- current_xy[[j]]$ax
        ay[j] <- current_xy[[j]]$ay
      }
    }
    list(x = sx[j], y = sy[j], ax = ax[j], ay = ay[j],
         text = labelchoices[as.numeric(i)], textangle = 0,
         font = list(color = if (text_col == "match") datcol[j] else text_col,
                     size = labSize),
         arrowcolor = if (line_col == "match") datcol[j] else line_col,
         arrowwidth = 1, arrowhead = 0, arrowsize = 1.5,
         xanchor = "auto", yanchor = "auto")
  })
}

# Plot shorter label lines that avoid hitting text
linerect <- function(df, line_col = 'black') {
  df$dx <- df$ax - df$x
  df$dy <- df$ay - df$y
  df$topbot <- abs(df$dy / df$dx) > df$texth / df$textw
  df$dx2 <- with(df, ifelse(topbot, abs(0.5 * texth * dx / dy) * sign(dx),
                            0.5 * textw * sign(dx)))
  df$dy2 <- with(df, ifelse(topbot, 0.5 * texth * sign(dy),
                            abs(0.5 * textw * dy / dx) * sign(dy)))
  df$ax2 <- df$ax - df$dx2
  df$ay2 <- df$ay - df$dy2
  # x,y inside rectangle => no line
  inside <- df$x >= df$ax - df$textw/2 & df$x <= df$ax + df$textw/2 &
    df$y >= df$ay - df$texth/2 & df$y <= df$ay + df$texth/2
  df$ax2[inside] <- NA
  if (length(line_col) < nrow(df)) line_col <- rep_len(line_col, nrow(df))
  for (i in 1:nrow(df)) {
    lines(c(df$x[i], df$ax2[i]), c(df$y[i], df$ay2[i]),
          col = line_col[i], xpd = NA)
  }
}

# Allow plotly to show expressions as axis titles
exprToHtml <- function(x) {
  if (!is.expression(x)) return(x)
  x <- as.character(x)
  x <- gsub("\"|~", "", x)
  x <- gsub("\\[", "<sub>", x)
  x <- gsub("\\]", "</sub>", x)
  x <- gsub("symbol\\(.*?\\)", "", x)
  x <- gsub(" +", " ", x)
  x
}

# Plots rounded rectangles for labels
roundRect <- function(xleft, ybottom, xright, ytop,
                      padding,
                      col = 'white', border = 'black',
                      border_radius = 8, n = 20, ...) {
  if (border_radius == 0) {
    return(rect(xleft, ybottom, xright, ytop, col = col, border = border, ...))
  }
  # convert pixels to y axis units
  figheight <- par("pin")[2]  # inches
  border_radius <- border_radius * diff(par("usr")[3:4]) / (figheight * 100)
  # assumes textbox, i.e. width > height
  yheight <- abs(ytop - ybottom)
  border_radius <- min(c(border_radius, yheight / 2))
  yi <- border_radius
  xi <- border_radius * diff(par("usr")[1:2]) / diff(par("usr")[3:4])
  xi <- xi * figheight / par("pin")[1]
  pxy <- pixelToXY(padding)
  if (xi > pxy[1]) {
    xleft <- xleft - xi + pxy[1]
    xright <- xright + xi - pxy[1]
  }
  if (length(col) < length(xleft)) col <- rep_len(col, length(xleft))
  if (length(border) < length(xleft)) border <- rep_len(border, length(xleft))
  for (i in 1:length(xleft)) {
    x <- c(xright[i] - xi + xi * cx(0, pi/2, n),        # corner TR
           xleft[i] + xi + xi * cx(pi/2, pi, n),        # corner TL
           xleft[i] + xi + xi * cx(pi, 3*pi/2, n),      # corner BL
           xright[i] - xi + xi * cx(3*pi/2, 2*pi, n))   # corner BR
    y <- c(ytop[i] - yi + yi * cy(0, pi/2, n),          # corner TR
           ytop[i] - yi + yi * cy(pi/2, pi, n),         # corner TL
           ybottom[i] + yi + yi * cy(pi, 3*pi/2, n),    # corner BL
           ybottom[i] + yi + yi * cy(3*pi/2, 2*pi, n))  # corner BR
    polygon(x, y, col = col[i], border = border[i], ...)
  }
}

# corner arc functions
cx <- function(from, to, n) cos(seq(from, to, length.out = n))
cy <- function(from, to, n) sin(seq(from, to, length.out = n))

# convert pixels to xy axis units
pixelToXY <- function(pix) {
  figheight <- par("pin")[2]  # inches
  yi <- pix * diff(par("usr")[3:4]) / (figheight * 100)
  xi <- yi * diff(par("usr")[1:2]) / diff(par("usr")[3:4])
  xi <- xi * figheight / par("pin")[1]
  c(xi, yi)
}

# convert shapeScheme to plotly symbol
# offset by 1 since shapeScheme starts from 0
pch2symbol <- c('square-open', 'circle-open',
            'arrow-up-open', 'cross-thin-open',
            'x-thin-open', 'diamond-open',
            'arrow-down-open', 'square-x-open',
            'asterisk-open', 'diamond-x-open',
            'circle-x-open', 'hexagram-open',
            'square-cross-open', 'circle-x-open',
            'hourglass-open', 'square',
            'circle', 'arrow-up',
            'diamond', 'circle',
            'circle', 'circle',
            'square', 'diamond',
            'arrow-up', 'arrow-down')

col2hex <- function (cname) {
  colMat <- col2rgb(cname)
  rgb(red = colMat[1, ]/255, green = colMat[2, ]/255, blue = colMat[3, ]/255)
}


plot_points <- function(data, x, y, xaxt, yaxt, xlim, ylim, xlab, ylab,
                        showgrid, xgrid, ygrid, zeroline,
                        shape, shapeScheme, col, colScheme2,
                        outline_col, outline_lwd, outlier_shape,
                        size, sizeSwitch, do_raster = FALSE, no_points = FALSE, ...) {
  if (do_raster) {
    plot(data[!data$.outlier, x], data[!data$.outlier, y],
         type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         xlim = xlim, ylim = ylim,  ...)
  } else {
  plot(data[!data$.outlier, x], data[!data$.outlier, y],
       type = "n",
       xaxt = xaxt, yaxt = yaxt,
       xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ...,
       panel.first = {
         if (showgrid != "") {
           if (grepl("x", showgrid, ignore.case = TRUE)) {
             abline(v = xgrid, col = 'grey80', lwd = 0.5)
           }
           if (grepl("y", showgrid, ignore.case = TRUE)) {
             abline(h = ygrid, col = 'grey80', lwd = 0.5)
           }
         }
         if (zeroline) abline(h = 0, v = 0)
       })
  }
  
  if (!no_points) {
    points(data[!data$.outlier, x], data[!data$.outlier, y],
           pch = if (is.null(shape)) shapeScheme else shapeScheme[data[!data$.outlier, shape]],
           bg = if (is.null(col)) colScheme2 else colScheme2[data[!data$.outlier, col]],
           col = if (!is.null(col)) {
             if (all(shapeScheme > 20)) {
               outline_col
             } else {
               colScheme2[data[!data$.outlier, col]]
             }
           } else {colScheme2},
           cex = switch(sizeSwitch, size / 8,
                        data[!data$.outlier, 'plotly_size'] / 8),
           lwd = outline_lwd)
    
    # add outliers
    if (any(data$.outlier)) {
      points(data[data$.outlier, x], data[data$.outlier, y],
             pch = outlier_shape,
             col = if (!is.null(col)) {
               colScheme2[data[data$.outlier, col]]} else colScheme2,
             cex = size / 8)
    }
  }
}

#' @importFrom grDevices as.raster
insert_image <- function(temp_image, res) {
  # need to extract coords of plot window then crop the png
  pix <- par("din") * res
  plt <- par("plt")
  width <- pix[1] * diff(plt[1:2]) * 0.99
  height <- pix[2] * diff(plt[3:4]) * 0.99
  x_off <- pix[1] * (plt[1] + diff(plt[1:2]) * 0.005)
  y_off <- pix[2] * (1-plt[4] + diff(plt[3:4]) * 0.005)
  
  if (!requireNamespace("magick", quietly = TRUE))
    stop("magick package is not installed", call. = FALSE)
  geom <- magick::geometry_area(width, height, x_off, y_off)
  image <- magick::image_read(temp_image)
  image <- magick::image_crop(image, geom)
  image <- as.raster(image)
  
  usr <- par("usr")  # x1, x2, y1, y2
  xd <- diff(usr[1:2]) * 0.005
  yd <- diff(usr[3:4]) * 0.005
  rasterImage(image, usr[1] +xd, usr[3] +yd, usr[2] -xd, usr[4] -yd,
              interpolate = FALSE)
}
