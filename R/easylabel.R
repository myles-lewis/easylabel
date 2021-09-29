# Easy label volcano plots and scatter plots
# By Myles Lewis

#' Interactive scatter plot labels
#'
#' Interactive labelling of scatter plots using shiny/plotly interface.
#'
#' @details
#' Instructions:
#' * Hover over and click on/off genes which you want to label.
#' * When you have selected all your chosen genes, then drag gene names to
#'   move label positions.
#' * Click the save button to export a PDF in base graphics.
#' * The Table tab shows a table view of the dataset to help with annotation.
#'
#' To export an SVG from plotly:
#' * You can move the legend as well.
#' * Switch to SVG when finalised (only do this at last moment as otherwise
#'   editing is very slow).
#' * Press camera button in modebar to save image as SVG.
#' @param data Dataset to use for plot.
#' @param x specifies column of x coordinates in `data`.
#' @param y specifies column of y coordinates in `data`.
#' @param col specifies the column (ideally a factor) in `data` with which
#' to colour points.
#' @param labs specifies the column in `data` with label names for points.
#' If `NULL` defaults to `rownames(data)`.
#' @param scheme vector of colours for points.
#' @param xlab x axis title. Accepts expressions when exporting base graphics.
#' @param ylab y axis title. Accepts expressions when exporting base graphics.
#' @param filename Filename for saving plots to pdf.
#' @param startLabels vector of initial labels.
#' @param xlim the x limits (x1, x2) of the plot.
#' @param ylim the y limits of the plot.
#' @param showOutliers Logical whether to show outliers on the margins of the
#' plot.
#' @param width Width of the plot in pixels. Saving to pdf scales 100 pixels to
#' 1 inch.
#' @param height Height of the plot in pixels.
#' @param showgrid Logical whether to show gridlines.
#' @param zeroline Logical whether to show lines at x = 0 and y = 0.
#' @param hline Adds horizontal lines at values of y.
#' @param vline Adds vertical lines at values of x.
#' @param alpha Alpha value for transparency of points.
#' @param pch Main point symbol. See [points()].
#' @param outlier_pch Symbol for outliers.
#' @param outline_col Colour of symbol outlines. Set to `NA` for no outlines.
#' @param outline_lwd Line width of symbol outlines.
#' @param cex Size of points. Default 1.
#' @param cex.text Font size for labels. Default 0.72 to match plotly font size.
#' See [text()].
#' @param mgp The margin line for the axis title, axis labels and axis line.
#' See [par()].
#' @param Ltitle a character or expression (see [plotmath]) value specifying
#' text for left side title. Size of font can be changed using `cex.lab`.
#' @param Rtitle a character or expression value specifying text for right side
#' title. Size of font can be changed using `cex.lab`.
#' @param LRtitle_side on which side of the plot for `Ltitle` and `Rtitle`
#' (1 = bottom, 3 = top). See [mtext()].
#' @param labelDir Initial label line directions. Options include 'radial'
#' (default) for radial lines around the centre of the plot,
#' 'origin' for radial lines around the origin,
#' 'horiz' for horizontal and 'vert' for vertical,
#' 'xellipse' and 'yellipse' for near-horizontal and near-vertical lines
#' arranged in an elliptical way around the centre,
#' 'rect' for rectilinear lines (a mix of horizontal and vertical),
#' 'x' for diagonal lines,
#' 'oct' for lines in 8 directions around the centre.
#' @param labCentre Coordinates of the central point around which radial labels
#' converge towards. Defaults to the centre of the plot.
#' @param panel.last An expression to be evaluated after plotting has taken
#' place but before the axes, title and box are added. This can be useful for
#' adding extra titles, legends or trend lines. Currently only works when saving
#' plots using base graphics and does not work with plotly. See [plot.default]
#' @param text_col Colour of label text. (Not supported by plotly.)
#' @param line_col Colour of label lines. (Not supported by plotly.)
#' @param rectangles Logical whether to show rectangles around labels.
#' (Not supported by plotly.)
#' @param rect_col Colour for filling rectangles. (Not supported by plotly.)
#' @param border_col Colour of rectangle borders. Use `border_col = NA` to omit
#' borders. (Not supported by plotly.)
#' @param padding Amount of padding in pixels around label text.
#' @param border_radius Amount of roundedness in pixels to apply to label
#' rectangles. (Not supported by plotly.)
#' @param fullGeneNames Logical whether to expand gene symbols using
#' Bioconductor AnnotationDbi package. With multiple matches, returns first
#' value only.
#' See [mapIds()].
#' @param AnnotationDb Annotation database to use when expanding gene symbols.
#' Defaults to human gene database `AnnotationDb = org.Hs.eg.db`.
#' @param custom_annotation List of annotations to be added via
#' [plotly::layout()].
#' @param ... Further graphical parameters passed to `plot()` when saving via
#' base graphics. The most useful for most users are likely to be `cex.lab`
#' which alters axis title font size (default 1, see [par()]), `cex.axis` which
#' alters axis numbering font size (default 1), and `panel.last` which allows
#' additional plotting functions to be called after the main plot has been
#' plotted but before the labels and label lines are drawn, which will allow the
#' addition of trend lines, extra titles or legends for example (see
#' [plot.default()]).
#' @seealso [easyVolcano()], [easyMAplot()],
#' [plot_ly()], [points()], [par()], [plot.default()], [plotmath()]
#' @importFrom shiny fluidPage tabsetPanel tabPanel fluidRow column
#' radioButtons selectizeInput actionButton checkboxGroupInput observe
#' updateSelectizeInput reactiveValues isolate reactive debounce
#' observeEvent modalDialog textAreaInput tagList modalButton showModal
#' removeModal h5 shinyApp downloadButton selectInput br textInput
#' @importFrom plotly plot_ly layout plotlyOutput renderPlotly event_data
#' event_register config plotlyProxy plotlyProxyInvoke add_markers %>%
#' @importFrom RColorBrewer brewer.pal
#' @importFrom DT dataTableOutput datatable formatSignif
#' @export


easylabel <- function(data, x, y,
                      labs = NULL,
                      col = NULL, scheme = NULL,
                      xlab = x, ylab = y,
                      filename = NULL,
                      startLabels = NULL,
                      xlim = NULL, ylim = NULL,
                      showOutliers = TRUE,
                      width = 800, height = 600,
                      showgrid = FALSE, zeroline = TRUE,
                      hline = NULL, vline = NULL,
                      alpha = 1,
                      aes_pch = NULL,
                      pch = 21,
                      outlier_pch = 5,
                      outline_col = 'white',
                      outline_lwd = 0.5,
                      cex = 1,
                      cex.text = 0.72,
                      mgp = c(1.8, 0.5, 0),
                      Ltitle = "", Rtitle = "",
                      LRtitle_side = 1,
                      labelDir = "radial",
                      labCentre = NULL,
                      panel.last = NULL,
                      text_col = 'black', line_col = 'black',
                      rectangles = FALSE,
                      rect_col = 'white', border_col = 'black',
                      padding = 3, border_radius = 5,
                      fullGeneNames = FALSE,
                      AnnotationDb = NULL,
                      custom_annotation = NULL, ...) {
  if (is.null(filename)) filename <- paste0("label_",
                                            deparse(substitute(data)))
  args <- list(...)
  data <- as.data.frame(data)
  # determine outliers
  data$outlier <- FALSE
  xyspan <- c(max(data[, x], na.rm = TRUE) - min(data[, x], na.rm = TRUE),
              max(data[, y], na.rm = TRUE) - min(data[, y], na.rm = TRUE))
  if (!is.null(ylim)) {
    notNA <- !is.na(data[,y])
    data$outlier[notNA & (data[, y] < ylim[1] | data[, y] > ylim[2])] <- TRUE
    data[notNA & data[, y] < ylim[1], y] <- ylim[1]
    data[notNA & data[, y] > ylim[2], y] <- ylim[2]
    xyspan[2] <- ylim[2] - ylim[1]
    ylim[1] <- ylim[1] - xyspan[2] * 0.025
    ylim[2] <- ylim[2] + xyspan[2] * 0.025
  }
  if (!is.null(xlim)) {
    notNA <- !is.na(data[,x])
    data$outlier[notNA & (data[, x] < xlim[1] | data[, x] > xlim[2])] <- TRUE
    data[notNA & data[, x] < xlim[1], x] <- xlim[1]
    data[notNA & data[, x] > xlim[2], x] <- xlim[2]
    xyspan[1] <- xlim[2] - xlim[1]
    xlim[1] <- xlim[1] - xyspan[1] * 0.025
    xlim[2] <- xlim[2] + xyspan[1] * 0.025
  }

  # combine symbols & outlier symbol
  if (!showOutliers) {
    data <- data[!data$outlier, ]
    showOutliers <- 1
  } else {
    if (any(data$outlier)) {
      showOutliers <- 2
      if (!is.null(aes_pch)) {
        data$comb_symbol <- factor(data[, aes_pch])
        levels(data$comb_symbol) <- c(levels(data$comb_symbol), "Outlier")
        data$comb_symbol[data$outlier] <- "Outlier"
        pch <- c(pch, outlier_pch)
      }
    } else showOutliers <- 1
  }

  if (is.null(labCentre)) {
    labCentre <- c(mean(range(data[, x], na.rm = TRUE)),
                   mean(range(data[, y], na.rm = TRUE)))
  }

  if (is.null(scheme)) {
    scheme <- if (!is.null(col)) {
      brewer.pal(nlevels(data[,col]), "Set1")
    } else 'black'
  }
  labelchoices <- if (is.null(labs)) rownames(data) else data[, labs]
  startLabels <- startLabels[startLabels %in% labelchoices]
  start_annot <- annotation(startLabels, data, x, y, labSize = labSize,
                            labelDir = labelDir, labCentre = labCentre,
                            xyspan = xyspan)
  start_xy <- lapply(start_annot, function(i) list(ax = i$ax, ay = i$ay))
  names(start_xy) <- startLabels
  # plotly arguments
  symbols <- pch2symbol[pch + 1]
  outlier_symbol <- pch2symbol[outlier_pch + 1]
  markerSize <- cex * 8
  if (is.na(outline_col)) outline_lwd <- 0  # fix plotly no outlines
  if (all(pch < 15)) outline_lwd <- 1
  markerOutline <- list(width = outline_lwd,
                        color = outline_col)
  marker <- list(size = markerSize,
                 opacity = alpha,
                 line = markerOutline)
  labSize <- cex.text / 0.72 * 12
  LRtitles <- list(
    list(x = 0,
         y = ifelse(LRtitle_side == 3, 1, 0),
         align = 'left',
         text = exprToHtml(Ltitle),
         font = list(color = "black"),
         xref = 'paper', yref = 'paper',
         showarrow = F),
    list(x = 1,
         y = ifelse(LRtitle_side == 3, 1, 0),
         align = 'right',
         text = exprToHtml(Rtitle),
         font = list(color = "black"),
         xref = 'paper', yref = 'paper',
         showarrow = F))
  hovertext <- labelchoices
  if (fullGeneNames) {
    if (!requireNamespace("AnnotationDbi", quietly = TRUE)) {
      stop("Can't find package AnnotationDbi.
           Try BiocManager::install('AnnotationDbi')",
           call. = FALSE)
    }
    if (is.null(AnnotationDb)) {
      if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
        stop("Can't find gene annotation database org.Hs.eg.db.
             Try BiocManager::install('org.Hs.eg.db')",
             call. = FALSE)
      }
      AnnotationDb <- org.Hs.eg.db::org.Hs.eg.db
    }
    data$gene_fullname <- AnnotationDbi::mapIds(AnnotationDb, labelchoices,
                                                "GENENAME", "ALIAS",
                                                multiVals = 'first')
    notNA <- !is.na(data$gene_fullname)
    hovertext[notNA] <- paste0(labelchoices[notNA], "\n",
                               data$gene_fullname[notNA])
  }
  labDir_choices <- c('radial', 'origin', 'horiz', 'vert', 'xellipse',
                      'yellipse', 'rect', 'x', 'oct')
  names(labDir_choices) <- c('Radial centre', 'Radial origin',
                             'Horizontal', 'Vertical',
                             'Horizontal ellipse', 'Vertical ellipse',
                             'Rectilinear', 'Diagonal', 'Octagonal')

  ui <- fluidPage(
    tabsetPanel(
      tabPanel("Plot",
               fluidRow(
                 br(),
                 plotlyOutput("plotly", height = paste0(height, "px")),
                 br()),
               fluidRow(
                 column(3,
                        selectInput("labDir", label = h5("Label direction"),
                                    choices = labDir_choices,
                                    selected = labelDir),
                        radioButtons("ptype", label = h5("Plot type"),
                                     choices = c("WebGL (fast)" = 1,
                                                 "SVG (slow)" = 2),
                                     selected = 1)
                 ),
                 column(4,
                        selectizeInput(
                          'label', h5('Select labels'),
                          choices = NULL,
                          options = list(
                            onInitialize = I('function() { this.setValue(""); }')
                          ),
                          multiple = T)),
                 column(5,
                        textInput("filename", h5("Save pdf filename"), filename),
                        br(),
                        actionButton("add_batch", "Add batch"),
                        actionButton("clear", "Clear all"),
                        downloadButton("save_plot", "Save pdf")
                 )
               )
      ),
      tabPanel("Table",
               fluidRow(
                 column(2,
                        checkboxGroupInput('selgroup', 'Select groups',
                                           levels(data[, col]),
                                           selected = levels(data[, col]))
                 ),
                 column(10,
                        DT::dataTableOutput("table")
                 )
               )
      )
    )
  )

  server <- function(input, output, session) {

    output$plotly <- renderPlotly({
      labs <- startLabels
      isolate(pt <- as.numeric(input$ptype))
      isolate(ldir <- input$labDir)
      annot <- annotation(labs, data, x, y, labSize = labSize, labelDir = ldir,
                          labCentre = labCentre, xyspan = xyspan)
      isolate(labels$annot <- annot)
      if (!is.null(hline)) {
        shapes = lapply(hline, function(i) {
          list(type = "line",
               line = list(width = 1, color = '#AAAAAA', dash = 'dash'),
               x0 = 0, x1 = 1, y0 = i, y1 = i, xref = "paper")
        })
      } else {
        shapes = list()
      }
      if (!is.null(vline)) {
        shapes = c(shapes, lapply(vline, function(i) {
          list(type = "line",
               line = list(width = 1, color = '#AAAAAA', dash = 'dash'),
               y0 = 0, y1 = 1, x0 = i, x1 = i, yref = "paper")
        }))
      }

      switch(showOutliers,
             # no outliers
             plot_ly(data = data, x = as.formula(paste0('~', x)),
                     y = as.formula(paste0('~', y)),
                     type = switch(pt, 'scattergl', 'scatter'),
                     mode = 'markers',
                     color = if (!is.null(col)) {
                       as.formula(paste0('~', col))
                     } else I(scheme),
                     colors = scheme,
                     marker = marker,
                     symbol = if (!is.null(aes_pch)) {
                       as.formula(paste0('~', aes_pch))
                     } else I(symbols),
                     symbols = symbols,
                     text = hovertext, hoverinfo = 'text',
                     key = labelchoices, source = 'lab_plotly',
                     width = width, height = height) %>%
               layout(
                 title = args$main,
                 xaxis = list(title = exprToHtml(xlab),
                              showgrid = showgrid,
                              color = 'black', ticklen = 5,
                              showline = TRUE, zeroline = zeroline),
                 yaxis = list(title = exprToHtml(ylab),
                              showgrid = showgrid,
                              color = 'black', ticklen = 5,
                              showline = TRUE, zeroline = zeroline)),
             # with outliers
             plot_ly(data = data[!data$outlier,],
                     x = as.formula(paste0('~', x)),
                     y = as.formula(paste0('~', y)),
                     type = switch(pt, 'scattergl', 'scatter'),
                     mode = 'markers',
                     color = if (!is.null(col)) {
                       as.formula(paste0('~', col))
                     } else I(scheme),
                     colors = scheme,
                     marker = marker,
                     symbol = if (!is.null(aes_pch)) {
                       ~comb_symbol
                     } else I(symbols),
                     symbols = symbols,
                     text = hovertext[!data$outlier], hoverinfo = 'text',
                     key = labelchoices[!data$outlier], source = 'lab_plotly',
                     legendgroup = 'Main',
                     width = width, height = height) %>%
               add_markers(data = data[data$outlier, ],
                           x = as.formula(paste0('~', x)),
                           y = as.formula(paste0('~', y)),
                           type = switch(pt, 'scattergl', 'scatter'),
                           color = as.formula(paste0('~', col)),
                           colors = scheme,
                           marker = marker,
                           symbol = if (!is.null(aes_pch)) {
                             ~comb_symbol
                           } else I(outlier_symbol),
                           symbols = symbols,
                           inherit = F,
                           text = hovertext[data$outlier],
                           hoverinfo = 'text',
                           key = labelchoices[data$outlier],
                           legendgroup = 'outlier', name = 'outlier') %>%
               layout(
                 title = args$main,
                 xaxis = list(range = as.list(xlim),
                              title = exprToHtml(xlab),
                              showgrid = showgrid, color = 'black',
                              ticklen = 5, showline = TRUE,
                              zeroline = zeroline),
                 yaxis = list(range = as.list(ylim),
                              title = exprToHtml(ylab),
                              showgrid = showgrid, color = 'black',
                              ticklen = 5, showline = TRUE,
                              zeroline = zeroline))
      ) %>%
        layout(annotations = c(annot, LRtitles, custom_annotation),
               hovermode = 'closest',
               legend = list(font = list(color = 'black')),
               shapes = shapes) %>%
        config(edits = list(annotationTail = TRUE, legendPosition = TRUE),
               # plotGlPixelRatio = 6,
               toImageButtonOptions = list(format = "svg")) %>%
        event_register(event = 'plotly_click')

    })

    # download plot using base graphics

    output$save_plot <- downloadHandler(filename = function()
    {paste0(input$filename, ".pdf")}, content = function(file) {

      labs <- labels$list
      current_xy <- labelsxy$list
      xrange <- range(c(data[, x], xlim), na.rm = TRUE)
      yrange <- range(c(data[, y], ylim), na.rm = TRUE)
      scheme2 <- adjustcolor(scheme, alpha.f = alpha)
      if (!is.null(col)) data <- data[order(data[, col]), ]
      legenddist <- max(
        (max(nchar(c(levels(data[, col]), levels(data[, aes_pch]))), na.rm = TRUE)
         + 3) * 0.37, 6)

      pdf(file, width = width/100, height = height/100 + 0.75)
      op <- par(mgp = mgp, mar = c(4, 4, 2, legenddist), tcl = -0.3,
                las = 1, bty = 'l',
                font.main = 1)
      plot(data[!data$outlier, x], data[!data$outlier, y],
           pch = if (is.null(aes_pch)) pch else pch[data[!data$outlier, aes_pch]],
           bg = if (is.null(col)) scheme2 else scheme2[data[!data$outlier, col]],
           col = if (!is.null(col)) {
             if (all(pch > 20)) {
               outline_col
             } else {
               scheme2[data[!data$outlier, col]]
             }
           } else {scheme2},
           lwd = outline_lwd,
           xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, cex = cex, ...,
           panel.first = {
             if (showgrid) {
               abline(h = pretty(data[, y]), v = pretty(data[, x]),
                      col = 'grey80', lwd = 0.5)
               }
             if (zeroline) abline(h = 0, v = 0)
           },
           panel.last = panel.last)
      mtext(Ltitle, LRtitle_side, adj = 0,
            line = ifelse(LRtitle_side <= 2, mgp[1], 0.1),
            cex = args$cex.lab)
      mtext(Rtitle, LRtitle_side, adj = 1,
            line = ifelse(LRtitle_side <= 2, mgp[1], 0.1),
            cex = args$cex.lab)
      # arrange legend features
      legtext <- legcol <- legpch <- legbg <- NULL
      if (!is.null(col)) {
        legtext <- levels(data[, col])
        legbg <- scheme2
        legpch <- rep(pch[1], length(scheme2))
        legcol <- if (any(pch < 21)) scheme2 else rep(outline_col, length(scheme2))
      }
      if (!is.null(aes_pch)) {
        legtext <- c(legtext, levels(data[, aes_pch]))
        legpch <- c(legpch, pch)
        legbg <- c(legbg, rep(scheme2[1], length(pch)))
        legcol <- c(legcol, rep(scheme2[1], length(pch)))
      }
      # special case of col == aes_pch
      if (length(col) > 0 & length(aes_pch) > 0) {
        if (aes_pch == col) {
          legtext <- levels(data[, col])
          legbg <- scheme2
          legcol <- if (any(pch < 21)) scheme2 else outline_col
          legpch <- pch
        }
      }

      pt.lwd <- outline_lwd
      if (any(data$outlier)) {
        points(data[data$outlier, x], data[data$outlier, y],
               pch = outlier_pch,
               col = if (!is.null(col)) {
                 scheme2[data[data$outlier, col]]} else scheme2,
               cex = cex)
        legpch <- c(legpch, outlier_pch)
        legcol <- c(legcol, 'black')
        legbg <- c(legbg, 'black')
        pt.lwd <- c(rep(pt.lwd, length(legtext)), 1)
        legtext <- c(legtext, 'outlier')
      }
      abline(h = hline[hline != 0], v = vline[vline != 0],
             col = '#AAAAAA', lty = 2)
      abline(h = hline[hline == 0], v = vline[vline == 0])
      # add labels
      if (length(labs) > 0) {
        annot <- annotation(labs, data, x, y, current_xy, labSize = labSize)
        annotdf <- data.frame(x = unlist(lapply(annot, '[', 'x')),
                              y = unlist(lapply(annot, '[', 'y')),
                              ax = unlist(lapply(annot, '[', 'ax')),
                              ay = unlist(lapply(annot, '[', 'ay')),
                              text = unlist(lapply(annot, '[', 'text')))
        # convert plotly ax,ay to x,y coords
        annotdf$ax <- annotdf$x +
          annotdf$ax / (width - 150) * (xrange[2] - xrange[1]) * 1.2
        annotdf$ay <- annotdf$y -
          annotdf$ay / height * (yrange[2] - yrange[1]) * 1.2
        # padding
        pxy <- pixelToXY(padding)
        annotdf$texth <- strheight(labs, cex = cex.text) + 2 * pxy[2]
        annotdf$textw <- strwidth(labs, cex = cex.text) + 2 * pxy[1]
        # plot label line
        linerect(annotdf, line_col)
        if (rectangles) {
          roundRect(annotdf$ax - annotdf$textw/2, annotdf$ay - annotdf$texth/2,
                    annotdf$ax + annotdf$textw/2, annotdf$ay + annotdf$texth/2,
                    col = rect_col, border = border_col,
                    border_radius = border_radius, xpd = NA)
        }
        text(annotdf$ax, annotdf$ay, annotdf$text,
             col = text_col, xpd = NA, cex = cex.text)
      }
      if (!is.null(c(col, aes_pch))) {
        legend(x = xrange[2] + (xrange[2] - xrange[1]) * 0.04, y = yrange[2],
               legend = legtext, pt.bg = legbg,
               pt.lwd = pt.lwd, pt.cex = 0.9,
               col = legcol, pch = legpch, bty = 'n',
               cex = 0.75, xjust = 0, yjust = 0.5, xpd = NA)
      }
      if (!is.null(custom_annotation)) {
        custtext <- custom_annotation[[1]]$text
        custtext <- gsub("<br>", "\n", custtext)
        legend(x = xrange[2] + (xrange[2] - xrange[1]) * 0.02, y = yrange[1],
               legend = custtext,
               bty = 'n', cex = 0.65, xjust = 0, yjust = 0, xpd = NA)
      }
      par(op)
      dev.off()
      # print(legtext)
      # print(legcol)
      # print(legbg)
      # print(legpch)

    }, contentType = 'application/pdf')

    updateSelectizeInput(session, 'label',
                         choices = labelchoices, server = TRUE)
    labels <- reactiveValues(list = startLabels)
    labelsxy <- reactiveValues(list = start_xy)

    # with click add label
    observe({
      s <- event_data("plotly_click", source = "lab_plotly", priority = "event")
      req(s)
      s_key <- unlist(s$key)
      isolate(
        if (s_key %in% labels$list) {
          w <- which(labels$list == s_key)
          labels$list <- labels$list[-w]
          labelsxy$list[[w]] <- NULL
        } else {
          labels$list <- c(labels$list, s_key)
        }
      )
    })

    # selectize genes
    input_label <- reactive({input$label}) %>% debounce(200)

    # Update labels from selectize
    observeEvent(input_label(), {
      currentsel <- input_label()
      if (length(currentsel) == 0) {labels$list <- character(0)
      } else if (any(!currentsel %in% labels$list) |
                 any(!labels$list %in% currentsel)) {
        labels$list <- currentsel
        labelsxy$list <- labelsxy$list[currentsel]
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # update plotly labels
    observeEvent(labels$list, {
      labs <- labels$list
      current_xy <- labelsxy$list
      # Annotate gene labels
      annot <- annotation(labs, data, x, y, current_xy, labSize = labSize,
                          labelDir = input$labDir, labCentre = labCentre,
                          xyspan = xyspan)
      labelsxy$list <- lapply(annot, function(i) list(ax = i$ax, ay = i$ay))
      names(labelsxy$list) <- labs
      plotlyProxy('plotly', session) %>%
        plotlyProxyInvoke("relayout",
                          list(annotations = append(annot, custom_annotation)))
      sel_labels <- input$label
      if (any(!sel_labels %in% labs) | any(!labs %in% sel_labels)) {
        updateSelectizeInput(session, 'label', choices = labelchoices,
                             selected = labs, server = TRUE)
      }

    })

    # update plot type without replotting
    observeEvent(input$ptype, {
      pt <- as.numeric(input$ptype)
      plotlyProxy('plotly', session) %>%
        plotlyProxyInvoke("restyle",
                          list(type = switch(pt, 'scattergl', 'scatter')))
    })

    # store the location of moved labels by detecting plotly relayout events
    # of the type `annotations[1].ax`
    observeEvent(event_data("plotly_relayout", source = 'lab_plotly'), {
      s <- event_data("plotly_relayout", source = 'lab_plotly')
      if (grepl("annotations\\[[0-9]+\\]\\.ax", names(s)[[1]])) {
        w <- as.numeric(gsub("annotations\\[|\\]\\.ax", "", names(s)[1])) +1
        labelsxy$list[[w]] <- list(ax = s[[1]], ay = s[[2]])
      }
    })

    # Table tab
    output$table <- DT::renderDataTable({
      showCols <- colnames(data)[!colnames(data) %in%
                                   c('log10P', 'col', 'outlier', 'symbol')]
      df <- data[data[,col] %in% input$selgroup, showCols]
      cols <- colnames(df)[sapply(df, class) == "numeric"]
      rn <- if (is.null(labs)) TRUE else {
        labelchoices[data[,col] %in% input$selgroup, ]
      }
      datatable(df, rownames = rn) %>% formatSignif(cols, digits = 3)
    })

    observeEvent(input$clear, {
      labels$list <- list()
    })

    # Modal for batch gene input
    batchModal <- function() {
      modalDialog(
        textAreaInput("batchlabs", "Type or paste a list of genes",
                      height = '300px'),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("batch_ok", "OK")
        )
      )
    }

    # Open modal when button pressed
    observeEvent(input$add_batch, {
      showModal(batchModal())
    })

    # Add batch genes
    observeEvent(input$batch_ok, {
      if (!is.null(input$batchlabs)) {
        batch_genes <- unlist(strsplit(input$batchlabs, ",| |\n"))
        batch_genes <- batch_genes[batch_genes %in% labelchoices]
        labels$list <- c(labels$list, batch_genes)
        removeModal()
      }
    })

    # update label direction
    observeEvent(input$labDir, {
      labs <- labels$list
      # Redraw gene labels
      annot <- annotation(labs, data, x, y, current_xy = NULL,
                          labSize = labSize,
                          labelDir = input$labDir, labCentre = labCentre,
                          xyspan = xyspan)
      labelsxy$list <- lapply(annot, function(i) list(ax = i$ax, ay = i$ay))
      names(labelsxy$list) <- labs
      plotlyProxy('plotly', session) %>%
        plotlyProxyInvoke("relayout",
                          list(annotations = append(annot, custom_annotation)))
    })


  }

  shinyApp(ui, server)

}


#' Interactive volcano plot labels
#'
#' Interactive labelling of volcano plots using shiny/plotly interface.
#'
#' @param data The dataset for the plot. Automatically attempts to recognises
#' DESeq2 and limma objects.
#' @param x Name of the column containing log fold change. For DESeq2 and limma
#' objects this is automatically set.
#' @param y Name of the column containing p values. For DESeq2 and limma objects
#' this is automatically set.
#' @param padj Name of the column containing adjusted p values. Cannot be NULL
#' when y is not NULL.
#' @param fdrcutoff Cut-off for FDR significance. Defaults to FDR < 0.05
#' @param fccut Optional vector of log fold change cut-offs.
#' @param scheme Colour scheme. If no fold change cut-off is set, 2 colours
#' need to be specified. With a single fold change cut-off, 3 or 5 colours are
#' required, depending on whether the colours are symmetrical about x = 0.
#' Accommodates asymmetric colour schemes with multiple fold change cut-offs
#' (see examples).
#' @param xlab x axis title. Accepts expressions.
#' @param ylab y axis title. Accepts expressions.
#' @param filename Filename for saving to pdf.
#' @param showCounts Logical whether to show legend with number of
#' differentially expressed genes.
#' @param useQ Logical whether to convert nominal P values to q values.
#' Requires the qvalue Bioconductor package.
#' @param ... Other arguments passed to [easylabel()].
#' @seealso [easylabel()] [easyMAplot()]
#' @importFrom qvalue qvalue
#' @export


easyVolcano <- function(data, x = NULL, y = NULL, padj = NULL,
                        fdrcutoff = 0.05, fccut = NULL,
                        scheme = c('darkgrey', 'blue', 'red'),
                        xlab = expression("log"[2] ~ " fold change"),
                        ylab = expression("-log"[10] ~ " P"),
                        filename = NULL,
                        showCounts = TRUE, useQ = FALSE, ...) {
  if (is.null(filename)) filename <- paste0("volcano_",
                                            deparse(substitute(data)))
  if (is.null(x)) {
    if ('log2FoldChange' %in% colnames(data)) x = 'log2FoldChange'  # DESeq2
    if ('logFC' %in% colnames(data)) x = 'logFC'  # limma
  }
  if (is.null(y)) {
    if ('pvalue' %in% colnames(data)) {
      y = 'pvalue'  # DESeq2
      padj = 'padj'
    }
    if ('P.Value' %in% colnames(data)) {
      y = 'P.Value'  # limma
      padj = 'adj.P.Val'
    }
  }
  data[, 'log10P'] <- -log10(data[, y])
  if (useQ) {
    data$qvalue <- NA
    q <- try(qvalue::qvalue(data[!is.na(data[, padj]), y])$qvalues,
             silent = TRUE)
    if (inherits(q, 'try-error')) q <- p.adjust(data[!is.na(data[, padj]), y])
    data$qvalue[!is.na(data[, padj])] <- q
    siggenes <- data$qvalue < fdrcutoff
  } else {
    siggenes <- data[, padj] < fdrcutoff
  }
  siggenes[is.na(siggenes)] <- FALSE
  if (sum(siggenes) >0) {
    fdrline <- min(data[siggenes, 'log10P'])
  } else fdrline <- NULL
  if (showCounts) {
    up <- sum(siggenes & data[, x] > 0)
    down <- sum(siggenes & data[, x] < 0)
    total <- nrow(data)
    custom_annotation <- list(list(x = 1.2, y = 0.02, align = 'left',
                                   text = paste0(up, ' upregulated<br>',
                                               down, ' downregulated<br>',
                                               total, ' total genes'),
                                   font = list(size = 11, color = "black"),
                                   xref = 'paper', yref = 'paper',
                                   showarrow = F))
  } else custom_annotation = NULL

  if (is.null(fccut)) {
    data$col <- factor(siggenes, levels = c(F, T),
                       labels = c('ns', paste0('FDR<', fdrcutoff)))
    scheme <- scheme[1:2]
  } else {
    fccut <- abs(fccut)
    if (!(length(scheme)-1) %in% ((length(fccut)+1) * 1:2)) {
      stop("Number of colours in 'scheme' does not fit with number of cuts in 'fccut'")
    }
    siggenes <- as.numeric(siggenes)
    if (length(fccut) == 1 & fccut == 0) {
      # simple 3 colour version (grey: ns, blue: FC<0, red: FC>0)
      fc <- cut(data[,x], c(-Inf, 0, Inf))
      siggenes[siggenes == 1] <- as.numeric(fc[siggenes == 1])
      data$col <- factor(siggenes, levels = 0:(length(fccut) + 1),
                         labels = c('ns',
                                    paste0('FDR<', fdrcutoff, ', FC<0'),
                                    paste0('FDR<', fdrcutoff, ', FC>0')))
    } else if (length(scheme) - 1 == length(fccut) + 1) {
      # symmetric colours
      fc <- cut(abs(data[, x]), c(-1, fccut, Inf))
      siggenes[siggenes == 1] <- as.numeric(fc[siggenes == 1])
      data$col <- factor(siggenes, levels = 0:(length(fccut) + 1),
                         labels = c('ns',
                                    paste0('FDR<', fdrcutoff,
                                           ', FC<', fccut[1]),
                                    paste0('FDR<', fdrcutoff, ', FC>', fccut)))
    } else {
      # asymmetric colours
      fccut <- sort(unique(c(fccut, 0, -fccut)))
      fc <- cut(data[,x], c(-Inf, fccut, Inf))
      siggenes[siggenes == 1] <- as.numeric(fc[siggenes == 1])
      data$col <- factor(siggenes, levels = 0:(length(fccut)+1),
                         labels = c('ns',
                                    paste0('FDR<', fdrcutoff,
                                           ', FC<', fccut[1]),
                                    paste0('FDR<', fdrcutoff, ', ',
                                           fccut[-length(fccut)],
                                           '<FC<', fccut[-1]),
                                    paste0('FDR<', fdrcutoff, ', FC>',
                                           fccut[length(fccut)])))
    }
  }
  y <- 'log10P'
  # this line removes a few genes which have P value < FDR cutoff but are
  # excluded by DESeq2
  if (!is.null(fdrline)) {
    data <- data[!(is.na(data[, padj]) & data$log10P > fdrline), ]
  }

  easylabel(data, x, y, col = 'col',
            xlab = xlab,
            ylab = ylab,
            filename = filename,
            scheme = scheme, zeroline = FALSE, hline = fdrline,
            custom_annotation = custom_annotation, ...)
}


#' Interactive MA plot labels
#'
#' Interactive labelling of MA plots using shiny/plotly interface.
#'
#' @param data The dataset for the plot. Automatically attempts to recognises
#' DESeq2 and limma objects.
#' @param x Name of the column containing mean expression. For DESeq2 and limma
#' objects this is automatically set.
#' @param y Name of the column containing log fold change. For DESeq2 and limma
#' objects this is automatically set.
#' @param padj Name of the column containing adjusted p values. For DESeq2 and
#' limma objects this is automatically set.
#' @param fdrcutoff Cut-off for FDR significance. Defaults to FDR < 0.05. Can
#' be vector with multiple cut-offs.
#' @param scheme Colour scheme. Length must match either length(fdrcutoff) + 1
#' to allow for non-significant genes, or match length(fdrcutoff) * 2 + 1 to
#' accommodates asymmetric colour schemes for positive & negative fold change.
#' (see examples).
#' @param hline Vector of horizontal lines (default is y = 0).
#' @param labelDir Option for label lines. See [easylabel()].
#' @param xlab x axis title. Accepts expressions.
#' @param ylab y axis title. Accepts expressions.
#' @param filename Filename for saving to pdf.
#' @param showCounts Logical whether to show legend with number of
#' differentially expressed genes.
#' @param useQ Logical whether to convert nominal P values to q values.
#' Requires the qvalue Bioconductor package.
#' @param ... Other arguments passed to [easylabel()].
#' @seealso [easylabel()] [easyVolcano()]
#' @importFrom qvalue qvalue
#' @export


easyMAplot <- function(data, x = NULL, y = NULL, padj = NULL, fdrcutoff = 0.05,
                   scheme = c('darkgrey', 'blue', 'red'),
                   hline = 0,
                   labelDir = 'yellipse',
                   xlab = expression("log"[2] ~ " mean expression"),
                   ylab = expression("log"[2] ~ " fold change"),
                   filename = NULL,
                   showCounts = TRUE, useQ = FALSE, ...) {
  if (is.null(filename)) filename <- paste0("MAplot_",
                                            deparse(substitute(data)))
  if (is.null(y)) {
    if ('log2FoldChange' %in% colnames(data)) y = 'log2FoldChange'  # DESeq2
    if ('logFC' %in% colnames(data)) y = 'logFC'  # limma
  }
  if (is.null(padj)) {
    if ('pvalue' %in% colnames(data)) {
      pv = 'pvalue'  # DESeq2
      padj = 'padj'
    }
    if ('P.Value' %in% colnames(data)) {
      pv = 'P.Value'  # limma
      padj = 'adj.P.Val'
    }
  }
  if (is.null(x)) {
    if ('baseMean' %in% colnames(data)) {
      data[, 'logmean'] <- log2(data[, 'baseMean'])  # DESeq2
      x <- 'logmean'
    }
    if ('AveExpr' %in% colnames(data))
      x <- 'AveExpr'  # limma
  }

  if (useQ) {
    data$qvalue <- NA
    q <- try(qvalue::qvalue(data[!is.na(data[, padj]), pv])$qvalues,
             silent = TRUE)
    if (inherits(q, 'try-error')) q <- p.adjust(data[!is.na(data[, padj]), pv])
    data$qvalue[!is.na(data[, padj])] <- q
    siggenes <- data$qvalue < fdrcutoff[1]
  } else {
    siggenes <- data[, padj] < fdrcutoff[1]
  }
  siggenes[is.na(siggenes)] <- FALSE
  if (showCounts) {
    up <- sum(siggenes & data[, y] > 0)
    down <- sum(siggenes & data[, y] < 0)
    total <- nrow(data)
    custom_annotation <- list(list(x = 1.2, y = 0.02, align = 'left',
                                   text = paste0(up, ' upregulated<br>',
                                               down, ' downregulated<br>',
                                               total, ' total genes'),
                                   font = list(size = 11, color = "black"),
                                   xref = 'paper', yref = 'paper',
                                   showarrow = F))
  } else custom_annotation = NULL

  if (!(length(scheme) - 1) %in% (length(fdrcutoff) * 1:2)) {
    stop("Number of colours in 'scheme' does not fit with number of cuts in 'fdrcut'")
  }
  fdrcuts <- cut(data[, padj], c(-1, fdrcutoff, Inf))
  siggenes <- length(fdrcutoff) + 1 - as.numeric(fdrcuts)
  siggenes[is.na(siggenes)] <- 0
  if ((length(scheme) - 1 == length(fdrcutoff))) {
    # symmetric colours
    data$col <- factor(siggenes, levels = 0:length(fdrcutoff),
                       labels = c('ns', paste0('FDR<', fdrcutoff)))
  } else {
    # asymmetric colours
    fc <- data[, y] > 0
    siggenes[fc & siggenes != 0] <- siggenes[fc & siggenes != 0] + length(fdrcutoff)
    data$col <- factor(siggenes, levels = 0:(length(fdrcutoff) * 2),
                       labels = c('ns', paste0('FC<0, FDR<', fdrcutoff),
                                paste0('FC>0, FDR<', fdrcutoff)))
  }

  easylabel(data, x, y, col = 'col',
            ylab = ylab,
            xlab = xlab,
            filename = filename,
            scheme = scheme, zeroline = FALSE, hline = hline,
            labelDir = labelDir,
            custom_annotation = custom_annotation, ...)
}


# Annotate gene labels
annotation <- function(labels, data, x, y, current_xy = NULL,
                       labSize = 12, labelDir = "radial",
                       labCentre = c(0,0), xyspan = c(1,1)) {
  if (length(labels)!= 0) {
    annot <- lapply(1:length(labels), function(j) {
      i <- labels[j]
      row <- data[i, ]
      sx <- row[, x]
      sy <- row[, y]
      dx <- (sx - labCentre[1]) / xyspan[1]
      dy <- (sy - labCentre[2]) / xyspan[2]
      z <- sqrt(dx^2 + dy^2)
      if (labelDir == 'radial') {
        ax <- dx/z*75
        ay <- -dy/z*75
      } else if (labelDir == 'origin') {
        ox <- sx / xyspan[1]
        oy <- sy / xyspan[2]
        z <- sqrt(ox^2 + oy^2)
        ax <- ox/z*75
        ay <- -oy/z*75
      } else if (labelDir == 'xellipse') {
        dy <- dy / 4
        z <- sqrt(dx^2 + dy^2)
        ax <- dx/z*75
        ay <- -dy/z*75
      } else if (labelDir == 'yellipse') {
        dx <- dx / 5
        z <- sqrt(dx^2 + dy^2)
        ax <- dx/z*75
        ay <- -dy/z*75
      } else if (labelDir == 'horiz') {
        ax <- sign(dx) * 75
        ay <- 0
      } else if (labelDir == 'vert') {
        ax <- 0
        ay <- -sign(dy) * 75
      } else if (labelDir == 'x') {
        ax <- sign(dx) * 75
        ay <- -sign(dy) * 75
      } else if (labelDir == 'rect') {
        if (abs(dx) > abs(dy)) {
          ax <- sign(dx) * 75
          ay <- 0
        } else {
          ax <- 0
          ay <- -sign(dy) * 75
        }
      } else if (labelDir == 'oct') {
        ang <- atan2(dy, dx)
        ang <- round(ang * 4 / pi)
        ax <- cospi(ang / 4) * 75
        ay <- -sinpi(ang / 4) * 75
      }

      if (j <= length(current_xy)) {
        if (!is.null(current_xy[[j]])) {
          ax = current_xy[[j]]$ax
          ay = current_xy[[j]]$ay
        }
      }
      list(x = sx, y = sy, ax = ax, ay = ay,
           text = i, textangle = 0,
           font = list(color = "black", size = labSize),
           arrowcolor = "black", arrowwidth = 1, arrowhead = 0, arrowsize = 1.5,
           xanchor = "auto", yanchor = "auto")
    })
  } else {annot <- list()}
  annot
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
  for (i in 1:nrow(df)) {
    lines(c(df$x[i], df$ax2[i]), c(df$y[i], df$ay2[i]),
          col = line_col, xpd = NA)
  }
}

# Allow plotly to show expressions as axis titles
exprToHtml <- function(x) {
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
                      border_radius = 8, n = 20, ...) {
  if (border_radius == 0) {
    return(rect(xleft, ybottom, xright, ytop, ...))
  }
  # convert pixels to y axis units
  figheight <- (par("din")[2] - sum(par("mai")[c(1, 3)]))  # inches
  border_radius <- border_radius * diff(par("usr")[3:4]) / (figheight * 100)
  # assumes textbox, i.e. width > height
  yheight <- abs(ytop - ybottom)
  border_radius <- min(c(border_radius, yheight / 2))
  yi <- border_radius
  xi <- border_radius * diff(par("usr")[1:2]) / diff(par("usr")[3:4])
  xi <- xi * figheight / (par("din")[1] - sum(par("mai")[c(2, 4)]))
  for (i in 1:length(xleft)) {
    x <- c(xright[i] - xi + xi * cx(0, pi/2, n),        # corner TR
           xleft[i] + xi + xi * cx(pi/2, pi, n),        # corner TL
           xleft[i] + xi + xi * cx(pi, 3*pi/2, n),      # corner BL
           xright[i] - xi + xi * cx(3*pi/2, 2*pi, n))   # corner BR
    y <- c(ytop[i] - yi + yi * cy(0, pi/2, n),          # corner TR
           ytop[i] - yi + yi * cy(pi/2, pi, n),         # corner TL
           ybottom[i] + yi + yi * cy(pi, 3*pi/2, n),    # corner BL
           ybottom[i] + yi + yi * cy(3*pi/2, 2*pi, n))  # corner BR
    polygon(x, y, ...)
  }
}

# corner arc functions
cx <- function(from, to, n) cos(seq(from, to, length.out = n))
cy <- function(from, to, n) sin(seq(from, to, length.out = n))

# convert pixels to xy axis units
pixelToXY <- function(pix) {
  figheight <- (par("din")[2] - sum(par("mai")[c(1, 3)]))  # inches
  yi <- pix * diff(par("usr")[3:4]) / (figheight * 100)
  xi <- yi * diff(par("usr")[1:2]) / diff(par("usr")[3:4])
  xi <- xi * figheight / (par("din")[1] - sum(par("mai")[c(2, 4)]))
  c(xi, yi)
}

# convert pch to plotly symbol
# offset by 1 since pch starts from 0
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
