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
#' * Switch to SVG when finalised (only do this at last moment as otherwise
#'   editing is very slow).
#' * Press camera button in modebar to save image as SVG.
#' @param data Dataset (data.frame or data.table) to use for plot.
#' @param x Specifies column of x coordinates in `data`.
#' @param y Specifies column of y coordinates in `data`.
#' @param labs Specifies the column in `data` with label names for points. 
#' Label names do not have to be unique. If `NULL` defaults to `rownames(data)`.
#' @param startLabels Vector of initial labels. With a character vector, labels 
#' are identified in the column specified by `labs`. With a numeric vector,
#' points to be labelled are referred to by row number.
#' @param cex.text Font size for labels. Default 0.72 to match plotly font size.
#' See [text()].
#' @param col Specifies which column in `data` affects point colour. Must be
#' categorical. If it is not a factor, it will be coerced to a factor.
#' @param colScheme A single colour or a vector of colours for points.
#' @param alpha Alpha value for transparency of points.
#' @param shape Specifies which column in `data` controls point shapes. If not a
#' factor, will be coerced to a factor.
#' @param shapeScheme A single symbol for points or a vector of symbols.
#' See `pch` in [points()].
#' @param size Either a single value for size of points (default 8), or
#' specifies which column in `data` affects point size for bubble charts.
#' @param sizeRange Range of size of points for bubble charts.
#' @param xlab x axis title. Accepts expressions when exporting base graphics.
#' Set `cex.lab` to alter the font size of the axis titles (default 1).
#' Set `cex.axis` to alter the font size of the axis numbering (default 1).
#' @param ylab y axis title. Accepts expressions when exporting base graphics.
#' @param xlim The x limits (x1, x2) of the plot.
#' @param ylim The y limits of the plot.
#' @param xticks List of custom x axis ticks and labels specified as a list of 
#' two named vectors `at = ...` and `labels = ...`. Another method is to use 
#' `xaxp` as a vector of the form `c(x1, x2, n)` giving the coordinates of the 
#' extreme tick marks and the number of intervals between tick-marks.
#' @param yticks List of custom y axis ticks and labels specified as a list of 
#' two named vectors `at = ...` and `labels = ...`. Another method is to use 
#' `yaxp` as a vector of the form `c(y1, y2, n)` giving the coordinates of the 
#' extreme tick marks and the number of intervals between tick-marks.
#' @param showOutliers Logical whether to show outliers on the margins of the
#' plot.
#' @param outlier_shape Symbol for outliers.
#' @param outline_col Colour of symbol outlines. Set to `NA` for no outlines.
#' @param outline_lwd Line width of symbol outlines.
#' @param plotly_filter Refers to a column of logical values in `data` used to 
#' filter rows to reduce the number of points shown by plotly. We recommend 
#' using this for datasets with >100,000 rows. When saving to pdf, the full 
#' original dataset is still plotted. This is useful for plots with millions of 
#' points such as Manhattan plots where a subset of points to be labelled is 
#' already known.
#' @param width Width of the plot in pixels. Saving to pdf scales 100 pixels to
#' 1 inch.
#' @param height Height of the plot in pixels.
#' @param showgrid Either logical whether to show gridlines, or a character 
#' value where "x" means showing x axis gridlines and "y" means showing y axis 
#' gridlines.
#' @param zeroline Logical whether to show lines at x = 0 and y = 0.
#' @param hline Adds horizontal lines at values of y.
#' @param vline Adds vertical lines at values of x.
#' @param mgp The margin line for the axis title, axis labels and axis line.
#' See [par()].
#' @param Ltitle A character or expression (see [plotmath]) value specifying
#' text for left side title. Size of font can be changed using `cex.lab`.
#' @param Rtitle A character or expression value specifying text for right side
#' title. Size of font can be changed using `cex.lab`.
#' @param LRtitle_side On which side of the plot for `Ltitle` and `Rtitle`
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
#' @param labCentre Coordinates in x/y units of the central point towards which
#' radial labels converge. Defaults to the centre of the plot.
#' @param lineLength Initial length of label lines in pixels.
#' @param text_col Colour of label text. If set to 
#' `"match"` label text will match the colour of each point.
#' @param line_col Colour of label lines. If set to 
#' `"match"` label line will match the colour of each point.
#' @param rectangles Logical whether to show rectangles around labels
#' (not supported by plotly).
#' @param rect_col Colour for filling rectangles (not supported by plotly). If 
#' set to `"match"` rectangle fill colour will match the colour of each point.
#' @param border_col Colour of rectangle borders (not supported by plotly). 
#' Use `border_col = NA` to omit borders. If set to `"match"` rectangle border 
#' colour will match the colour of each point.
#' @param padding Amount of padding in pixels around label text.
#' @param border_radius Amount of roundedness in pixels to apply to label
#' rectangles (not supported by plotly).
#' @param showLegend Logical whether to show or hide the legend.
#' @param legendxy Vector of coordinates for the position of the legend. 
#' Coordinates are in plotly paper reference with `c(0, 0)` being the bottom 
#' left corner and `c(1, 1)` being the top right corner of the plot window.
#' Plotly has unusual behaviour in that the x coordinate always aligns the left 
#' side of the legend. However, the y coordinate aligns the top, middle or 
#' bottom of the legend dependent on whether it is in the top, middle or bottom 
#' 1/3 of the plot window. So `c(1, 0)` positions the legend in the bottom right 
#' corner outside the right margin of the plot, while `c(1, 0.5)` centre aligns 
#' the legend around the centre of y axis.
#' @param filename Filename for saving plots to pdf in a browser. Rstudio opens 
#' its own pdf file.
#' @param panel.last An expression to be evaluated after plotting has taken
#' place but before the axes, title and box are added. This can be useful for
#' adding extra titles, legends or trend lines. Currently only works when saving
#' plots using base graphics and does not work with plotly. See [plot.default]
#' @param fullGeneNames Logical whether to expand gene symbols using
#' Bioconductor AnnotationDbi package. With multiple matches, returns first
#' value only.
#' See [AnnotationDbi::mapIds()].
#' @param AnnotationDb Annotation database to use when expanding gene symbols.
#' Defaults to human gene database `AnnotationDb = org.Hs.eg.db`.
#' @param custom_annotation List of annotations to be added via
#' [plotly::layout()].
#' @param output_shiny Logical whether to output a shiny app. If `FALSE` a 
#' plotly figure will be returned.
#' @param ... Further graphical parameters passed to `plot()` when saving via
#' base graphics. The most useful for most users are likely to be `cex.lab`
#' which alters axis title font size (default 1, see [par()]), `cex.axis` which
#' alters axis numbering font size (default 1), and `panel.last` which allows
#' additional plotting functions to be called after the main plot has been
#' plotted but before the labels and label lines are drawn, which will allow the
#' addition of trend lines, extra titles or legends for example (see
#' [plot.default()]).
#' @seealso [easyVolcano()], [easyMAplot()]
#' @return  By default no return value. If `output_shiny = FALSE` a plotly 
#' figure is returned.
#' @importFrom shiny fluidPage tabsetPanel tabPanel fluidRow column
#' radioButtons selectizeInput actionButton checkboxGroupInput observe
#' updateSelectizeInput reactiveValues isolate reactive debounce
#' observeEvent modalDialog textAreaInput tagList modalButton showModal
#' removeModal h4 h5 shinyApp downloadButton selectInput br textInput req
#' downloadHandler
#' @importFrom plotly plot_ly layout plotlyOutput renderPlotly event_data
#' event_register config plotlyProxy plotlyProxyInvoke add_markers %>%
#' @importFrom RColorBrewer brewer.pal
#' @importFrom DT dataTableOutput datatable formatSignif
#' @importFrom grDevices adjustcolor pdf dev.off col2rgb rgb
#' @importFrom graphics abline legend lines mtext par points polygon rect
#' strheight strwidth text axis
#' @importFrom stats as.formula
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinybusy show_modal_spinner remove_modal_spinner
#' @examples 
#' 
#' # Simple example using mtcars dataset
#' data(mtcars)
#' # Launch easylabel Shiny app: only run this example in interactive R sessions
#' if (interactive()) {
#' easylabel(mtcars, x = 'mpg', y = 'wt', col = 'cyl')
#' }
#' @export

easylabel <- function(data, x, y,
                      labs = NULL,
                      startLabels = NULL,
                      cex.text = 0.72,
                      col = NULL, colScheme = NULL,
                      alpha = 1,
                      shape = NULL,
                      shapeScheme = 21,
                      size = 8,
                      sizeRange = c(4, 80),
                      xlab = x, ylab = y,
                      xlim = NULL, ylim = NULL,
                      xticks = NULL, yticks = NULL,
                      showOutliers = TRUE,
                      outlier_shape = 5,
                      outline_col = 'white',
                      outline_lwd = 0.5,
                      plotly_filter = NULL,
                      width = 800, height = 600,
                      showgrid = FALSE, zeroline = TRUE,
                      hline = NULL, vline = NULL,
                      mgp = c(1.8, 0.5, 0),
                      Ltitle = "", Rtitle = "",
                      LRtitle_side = 1,
                      labelDir = "radial",
                      labCentre = NULL,
                      lineLength = 75,
                      text_col = 'black', line_col = 'black',
                      rectangles = FALSE,
                      rect_col = 'white', border_col = 'black',
                      padding = 3, border_radius = 5,
                      showLegend = TRUE,
                      legendxy = c(1.02, 1),
                      filename = NULL,
                      panel.last = NULL,
                      fullGeneNames = FALSE,
                      AnnotationDb = NULL,
                      custom_annotation = NULL, 
                      output_shiny = TRUE, 
                      ...) {
  name_data <- deparse(substitute(data))
  if (is.null(filename)) filename <- paste0("label_", name_data)
  args <- list(...)
  if (!inherits(data, 'data.frame') | inherits(data, 'tbl')) data <- as.data.frame(data)
  # plotly axes
  if (is.logical(showgrid)) {
    showgrid <- if (showgrid) "xy" else ""
  }
  pxaxis <- list(title = exprToHtml(xlab),
                 showgrid = grepl("x", showgrid, ignore.case = TRUE),
                 color = 'black', ticklen = 5,
                 showline = TRUE, zeroline = zeroline)
  pyaxis <- list(title = exprToHtml(ylab),
                 showgrid = grepl("y", showgrid, ignore.case = TRUE),
                 color = 'black', ticklen = 5, 
                 showline = TRUE, zeroline = zeroline)
  if (!is.null(xticks)) {
    pxaxis <- c(pxaxis, tickmode = list('array'),
                tickvals = list(xticks$at),
                ticktext = list(xticks$labels))
  } else if (!is.null(args$xaxp)) {
    pxaxis <- c(pxaxis, tick0 = args$xaxp[1],
                dtick = (args$xaxp[2] - args$xaxp[1]) / args$xaxp[3])
  }
  if (!is.null(yticks)) {
    pyaxis <- c(pyaxis, tickmode = list('array'),
                tickvals = list(yticks$at),
                ticktext = list(yticks$labels))
  } else if (!is.null(args$yaxp)) {
    pyaxis <- c(pyaxis, tick0 = args$yaxp[1],
                dtick = (args$yaxp[2] - args$yaxp[1]) / args$yaxp[3])
  }
  
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
    pyaxis <- c(pyaxis, range = as.list(ylim))
  }
  if (!is.null(xlim)) {
    notNA <- !is.na(data[,x])
    data$outlier[notNA & (data[, x] < xlim[1] | data[, x] > xlim[2])] <- TRUE
    data[notNA & data[, x] < xlim[1], x] <- xlim[1]
    data[notNA & data[, x] > xlim[2], x] <- xlim[2]
    xyspan[1] <- xlim[2] - xlim[1]
    xlim[1] <- xlim[1] - xyspan[1] * 0.025
    xlim[2] <- xlim[2] + xyspan[1] * 0.025
    pxaxis <- c(pxaxis, range = as.list(xlim))
  }

  # combine symbols & outlier symbol
  if (!showOutliers) {
    data <- data[!data$outlier, ]
    showOutliers <- 1
  } else {
    if (any(data$outlier)) {
      showOutliers <- 2
      if (!is.null(shape)) {
        data$comb_symbol <- factor(data[, shape])
        levels(data$comb_symbol) <- c(levels(data$comb_symbol), "Outlier")
        data$comb_symbol[data$outlier] <- "Outlier"
        shapeScheme <- c(shapeScheme, outlier_shape)
      }
    } else showOutliers <- 1
  }

  if (is.null(labCentre)) {
    labCentre <- c(mean(range(data[, x], na.rm = TRUE)),
                   mean(range(data[, y], na.rm = TRUE)))
  }

  # checks on data and variables
  if (!is.null(col)) {
    if (!col %in% colnames(data)) stop(paste0("Column '", col, "' not found"))
    if (!is.factor(data[, col])) data[, col] <- factor(data[, col])
  }
  if (!is.null(shape)) {
    if (!shape %in% colnames(data)) {
      stop(paste0("Column '", shape, "' not found"))}
    if (!is.factor(data[, shape])) data[, shape] <- factor(data[, shape])
    if (length(shapeScheme) < nlevels(data[, shape])) {
      if (!identical(shapeScheme, 21)) {
      warning(paste0("shapeScheme has fewer levels than ",
                     name_data, "$", shape), call. = FALSE)
      }
      shapeScheme <- c(21, 24, 22, 25, 23,
                       1:2, 0, 6, 5, 3:4, 7:13)[1:nlevels(data[, shape])]
    }
  }

  if (is.null(colScheme)) {
    colScheme <- if (!is.null(col)) {
      brewer.pal(nlevels(data[,col]), "Set1")
    } else 'black'
  } else {
    if (is.null(col) & length(colScheme) > 1) {
      warning("`col` is not set. Using first colour only.", call. = FALSE)
    } else if (!is.null(col)) {
      if (length(colScheme) < nlevels(data[, col])) {
        warning(paste0("colScheme has fewer levels than ", name_data, "$", col),
                call. = FALSE)
      }
    }
  }
  colScheme <- col2hex(colScheme)
  
  # deal with case that col is NULL but label colours are set to "match"
  if (is.null(col)) {
    if (line_col == "match") line_col <- colScheme[1]
    if (text_col == "match") text_col <- colScheme[1]
    if (rect_col == "match") rect_col <- colScheme[1]
    if (border_col == "match") border_col <- colScheme[1]
  }
  ptext_col <- if (rectangles & text_col == "white") rect_col else text_col

  # plotly arguments
  psymbols <- pch2symbol[shapeScheme + 1]
  outlier_psymbol <- pch2symbol[outlier_shape + 1]
  sizeSwitch <- switch(class(size), "numeric" = 1, "character" = 2)
  if (sizeSwitch == 2) {
    if (!size %in% colnames(data)) stop(paste0("Column '", size, "' not found"))
    if (!class(data[, size]) %in% c('numeric', 'integer')) {
      stop(paste(size, "is not numeric"))
    }
    data$plotly_size <- sqrt(data[, size]) / max(sqrt(data[, size]), na.rm = TRUE) *
      max(sizeRange) + min(sizeRange)
  }
  if (is.na(outline_col)) outline_lwd <- 0  # fix plotly no outlines
  if (all(shapeScheme < 21) & outline_lwd == 0.5) outline_lwd <- 1
  pmarkerOutline <- list(width = outline_lwd,
                        color = outline_col)
  pmarker <- list(opacity = alpha,
                  line = pmarkerOutline,
                  sizemode = 'diameter')
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
  
  # plotly_filter
  plotly_data <- data
  if (!is.null(plotly_filter)) {
    plotly_data <- data[data[, plotly_filter], ]
  }
  
  # initialise labelchoices
  labelchoices <- if (is.null(labs)) rownames(plotly_data) else plotly_data[, labs]
  if (is.character(startLabels)) {
    startLabels <- which(labelchoices %in% startLabels)
  } else if (!is.null(plotly_filter)) {
    rownum <- 1:nrow(data)
    rownum <- rownum[data[, plotly_filter]]
    startLabels <- which(rownum %in% startLabels)
  }
  pkey <- 1:length(labelchoices)
  labSize <- cex.text / 0.72 * 12
  start_annot <- annotation(startLabels, plotly_data, x, y,
                            labelchoices = labelchoices,
                            labSize = labSize,
                            labelDir = labelDir, labCentre = labCentre,
                            xyspan = xyspan,
                            lineLength = lineLength,
                            col = col, colScheme = colScheme, 
                            text_col = ptext_col, line_col = line_col)
  start_xy <- lapply(start_annot, function(i) list(ax = i$ax, ay = i$ay))
  names(start_xy) <- startLabels
  hovertext <- labelchoices
  if (fullGeneNames) {
    if (!requireNamespace("AnnotationDbi", quietly = TRUE)) {
      stop("Can't find package AnnotationDbi. Try:
           BiocManager::install('AnnotationDbi')",
           call. = FALSE)
    }
    if (is.null(AnnotationDb)) {
      if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
        stop("Can't find gene annotation database org.Hs.eg.db. Try:
             BiocManager::install('org.Hs.eg.db')",
             call. = FALSE)
      }
      AnnotationDb <- org.Hs.eg.db::org.Hs.eg.db
    }
    plotly_data$gene_fullname <- AnnotationDbi::mapIds(AnnotationDb, labelchoices,
                                                "GENENAME", "ALIAS",
                                                multiVals = 'first')
    notNA <- !is.na(plotly_data$gene_fullname)
    hovertext[notNA] <- paste0(labelchoices[notNA], "\n",
                               plotly_data$gene_fullname[notNA])
  }
  
  if (!is.null(hline)) {
    pshapes = lapply(hline, function(i) {
      list(type = "line",
           line = list(width = 1, color = '#AAAAAA', dash = 'dash'),
           x0 = 0, x1 = 1, y0 = i, y1 = i, xref = "paper")
    })
  } else {
    pshapes = list()
  }
  if (!is.null(vline)) {
    pshapes = c(pshapes, lapply(vline, function(i) {
      list(type = "line",
           line = list(width = 1, color = '#AAAAAA', dash = 'dash'),
           y0 = 0, y1 = 1, x0 = i, x1 = i, yref = "paper")
    }))
  }
  
  if(output_shiny){
  ui <- fluidPage(
    tabsetPanel(
      tabPanel("Plot",
               fluidRow(
                 br(),
                 withSpinner(
                   plotlyOutput("plotly", height = paste0(height, "px"))
                 ),
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
                        if (!is.null(col)) {
                          checkboxGroupInput('colgroup', 'Select groups',
                                           levels(data[, col]),
                                           selected = levels(data[, col]))
                        }
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
      annot <- annotation(labs, plotly_data, x, y,
                          labelchoices = labelchoices,
                          labSize = labSize, labelDir = ldir,
                          labCentre = labCentre, xyspan = xyspan,
                          lineLength = lineLength,
                          col = col, colScheme = colScheme, 
                          text_col = ptext_col, line_col = line_col)
      annot <- c(annot, LRtitles, custom_annotation)
      
      main_plotly(plotly_data, x, y, 
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
                  pshapes)

    })

    # download plot using base graphics
    output$save_plot <- downloadHandler(filename = function()
    {paste0(input$filename, ".pdf")}, content = function(file) {
      
      if (nrow(data) > 80000) {
        show_modal_spinner(spin = "self-building-square",
                           text = "Saving pdf ...", color = "royalblue")
        on.exit(remove_modal_spinner(), add = TRUE)
      }
      labs <- labels$list
      current_xy <- labelsxy$list
      xrange <- range(c(data[, x], xlim), na.rm = TRUE)
      yrange <- range(c(data[, y], ylim), na.rm = TRUE)
      xspan <- xrange[2] - xrange[1]
      yspan <- yrange[2] - yrange[1]
      if (length(labs) > 0) {
        annot <- annotation(labs, plotly_data, x, y, current_xy,
                            labelchoices = labelchoices,
                            labSize = labSize,
                            lineLength = lineLength,
                            col = col, colScheme = colScheme, 
                            text_col = ptext_col, line_col = line_col)
        annotdf <- data.frame(x = unlist(lapply(annot, '[', 'x')),
                              y = unlist(lapply(annot, '[', 'y')),
                              ax = unlist(lapply(annot, '[', 'ax')),
                              ay = unlist(lapply(annot, '[', 'ay')),
                              text = unlist(lapply(annot, '[', 'text')))
        if (!is.null(col)) annotdf$col <- colScheme[plotly_data[as.numeric(labs), col]]
        # convert plotly ax,ay to x,y coords
        annotdf$ax <- annotdf$x +
          annotdf$ax / (width - 150) * xspan * 1.2
        annotdf$ay <- annotdf$y -
          annotdf$ay / height * yspan * 1.2
        # expand xlim, ylim for labels on the edges
        if (is.null(xlim)) {
          xlim <- xrange <- range(c(xrange, annotdf$ax), na.rm = TRUE)
          xspan <- xlim[2] - xlim[1]
          xlim[1] <- xrange[1] <- xlim[1] - xspan * 0.01
          xlim[2] <- xrange[2] <- xlim[2] + xspan * 0.01
          xspan <- xspan * 1.02
        }
        if (is.null(ylim)) {
          ylim <- yrange <- range(c(yrange, annotdf$ay), na.rm = TRUE)
          yspan <- ylim[2] - ylim[1]
        }
      }
      leg_xy <- c(legendxy[1] * xspan * 1.05 + xrange[1] - xspan * 0.025,
                  legendxy[2] * yspan * 1.05 + yrange[1] - yspan * 0.025)
      colScheme2 <- adjustcolor(colScheme, alpha.f = alpha)
      if (!is.null(col)) data <- data[order(data[, col]), ]
      legenddist <- max(
        (max(nchar(c(levels(data[, col]), levels(data[, shape]))), na.rm = TRUE)
         + 3) * 0.37, 6)
      xaxt <- if (is.null(xticks)) 's' else 'n'
      yaxt <- if (is.null(yticks)) 's' else 'n'
      
      # Grid lines
      xgrid <- pretty(c(xrange, xlim), n = 7)
      ygrid <- pretty(c(yrange, ylim), n = 7)
      if (!is.null(args$xaxp)) {
        xgrid <- seq(args$xaxp[1], args$xaxp[2], length.out = args$xaxp[3] + 1)
      }
      if (!is.null(args$yaxp)) {
        ygrid <- seq(args$yaxp[1], args$yaxp[2], length.out = args$yaxp[3] + 1)
      }
      
      pdf(file, width = width/100, height = height/100 + 0.75)
      oldpar <- par(no.readonly = TRUE)
      on.exit(par(oldpar), add = TRUE)
      par(mgp = mgp, mar = c(4, 4, 2, legenddist), tcl = -0.3,
          las = 1, bty = 'l', font.main = 1)
      plot(data[!data$outlier, x], data[!data$outlier, y],
           pch = if (is.null(shape)) shapeScheme else shapeScheme[data[!data$outlier, shape]],
           bg = if (is.null(col)) colScheme2 else colScheme2[data[!data$outlier, col]],
           col = if (!is.null(col)) {
             if (all(shapeScheme > 20)) {
               outline_col
             } else {
               colScheme2[data[!data$outlier, col]]
             }
           } else {colScheme2},
           cex = switch(sizeSwitch, size / 8,
                        data[!data$outlier, 'plotly_size'] / 8),
           lwd = outline_lwd,
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
           },
           panel.last = panel.last)
      if (!is.null(xticks)) {
        axis(1, at = xticks$at, labels = xticks$labels, ...)
      }
      if (!is.null(yticks)) {
        axis(2, at = yticks$at, labels = yticks$labels, ...)
      }
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
        legbg <- colScheme2
        legpch <- rep(shapeScheme[1], length(colScheme2))
        legcol <- if (any(shapeScheme < 21)) colScheme2 else rep(outline_col, length(colScheme2))
      }
      if (!is.null(shape)) {
        legtext <- c(legtext, levels(data[, shape]))
        legpch <- c(legpch, shapeScheme)
        if (length(colScheme2) == 1) {
          legbg <- c(legbg, rep(colScheme2, length(shapeScheme)))
          legcol <- c(legcol, rep(colScheme2, length(shapeScheme)))
        } else {
          legbg <- c(legbg, rep('black', length(shapeScheme)))
          legcol <- c(legcol, rep('black', length(shapeScheme)))
        }
      }
      # special case of col == shape
      if (length(col) > 0 & length(shape) > 0) {
        if (shape == col) {
          legtext <- levels(data[, col])
          legbg <- colScheme2
          legcol <- if (any(shapeScheme < 21)) colScheme2 else outline_col
          legpch <- shapeScheme
        }
      }

      pt.lwd <- outline_lwd
      if (any(data$outlier)) {
        points(data[data$outlier, x], data[data$outlier, y],
               pch = outlier_shape,
               col = if (!is.null(col)) {
                 colScheme2[data[data$outlier, col]]} else colScheme2,
               cex = size / 8)
        legpch <- c(legpch, outlier_shape)
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
        # padding
        pxy <- pixelToXY(padding)
        annotdf$texth <- strheight(annotdf$text, cex = cex.text) + 2 * pxy[2]
        annotdf$textw <- strwidth(annotdf$text, cex = cex.text) + 2 * pxy[1]
        # plot label line
        linerect(annotdf,
                 line_col = if (line_col == "match") annotdf$col else line_col)
        if (rectangles) {
          roundRect(annotdf$ax - annotdf$textw/2, annotdf$ay - annotdf$texth/2,
                    annotdf$ax + annotdf$textw/2, annotdf$ay + annotdf$texth/2,
                    col = if (rect_col != "match" | is.na(rect_col)) {
                      rect_col} else annotdf$col,
                    border = if (border_col != "match" | is.na(border_col)) {
                      border_col} else annotdf$col,
                    border_radius = border_radius, xpd = NA)
        }
        text(annotdf$ax, annotdf$ay, annotdf$text,
             xpd = NA, cex = cex.text,
             col = if (text_col == "match") annotdf$col else text_col)
      }
      if (!is.null(c(col, shape)) & showLegend) {
        legend(x = leg_xy[1], y = leg_xy[2],
               legend = legtext, pt.bg = legbg,
               pt.lwd = pt.lwd, pt.cex = 0.9,
               col = legcol, pch = legpch, bty = 'n',
               cex = 0.75, xjust = 0, xpd = NA,
               yjust = c(0, 0.5, 1)[cut(legendxy[2], 
                                        breaks = c(-Inf, 1/3, 2/3, Inf))]
               )
               # yjust needed for complex plotly behaviour
      }
      if (!is.null(custom_annotation) & showLegend) {
        custtext <- custom_annotation[[1]]$text
        custtext <- gsub("<br>", "\n", custtext)
        legend(x = xrange[2] + xspan * 0.02, y = yrange[1],
               legend = custtext,
               bty = 'n', cex = 0.65, xjust = 0, yjust = 0, xpd = NA)
      }
      dev.off()

    }, contentType = 'application/pdf')

    updateSelectizeInput(session, 'label',
                         choices = unique(labelchoices), server = TRUE)
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
    input_label <- reactive({input$label}) %>% debounce(500)

    # Update labels from selectize
    observeEvent(input_label(), {
      currentsel <- input_label()
      convert_labs <- labelchoices[as.numeric(labels$list)]
      if (length(currentsel) == 0) {labels$list <- character(0)
      } else if (any(!currentsel %in% convert_labs)) {
        addsel <- currentsel[!currentsel %in% convert_labs]
        labels$list <- c(labels$list, which(labelchoices %in% addsel))
      } else if (any(!convert_labs %in% currentsel)) {
        removesel <- convert_labs[!convert_labs %in% currentsel]
        removenum <- which(labelchoices %in% removesel)
        labels$list <- labels$list[!labels$list %in% removenum]
        labelsxy$list <- labelsxy$list[!names(labelsxy$list) %in% removenum]
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # update plotly labels
    observeEvent(labels$list, {
      labs <- labels$list
      current_xy <- labelsxy$list
      # Annotate gene labels
      annot <- annotation(labs, plotly_data, x, y, current_xy,
                          labelchoices = labelchoices,
                          labSize = labSize,
                          labelDir = input$labDir, labCentre = labCentre,
                          xyspan = xyspan,
                          lineLength = lineLength,
                          col = col, colScheme = colScheme, 
                          text_col = ptext_col, line_col = line_col)
      labelsxy$list <- lapply(annot, function(i) list(ax = i$ax, ay = i$ay))
      names(labelsxy$list) <- labs
      plotlyProxy('plotly', session) %>%
        plotlyProxyInvoke("relayout",
                          list(annotations = append(annot, custom_annotation)))
      sel_labels <- input$label
      sel_labels <- which(labelchoices %in% sel_labels)
      if (any(!sel_labels %in% as.numeric(labs)) | any(!as.numeric(labs) %in% sel_labels)) {
        updateSelectizeInput(session, 'label', choices = unique(labelchoices),
                             selected = labelchoices[as.numeric(labs)], server = TRUE)
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
      showCols <- colnames(plotly_data)[!colnames(plotly_data) %in%
                                   c('outlier', 'comb_symbol', 'plotly_filter')]
      df <- plotly_data[, showCols]
      if (!is.null(col)) df <- df[plotly_data[, col] %in% input$colgroup, ]
      cols <- colnames(df)[sapply(df, class) == "numeric"]
      rn <- if (is.null(labs)) TRUE else {
        if (!is.null(col)) {
          labelchoices[plotly_data[,col] %in% input$colgroup]
        } else labelchoices
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
        batch_genes <- which(labelchoices %in% batch_genes)
        labels$list <- c(labels$list, batch_genes)
        removeModal()
      }
    })

    # update label direction
    observeEvent(input$labDir, {
      labs <- labels$list
      # Redraw gene labels
      annot <- annotation(labs, plotly_data, x, y, current_xy = NULL,
                          labelchoices = labelchoices,
                          labSize = labSize,
                          labelDir = input$labDir, labCentre = labCentre,
                          xyspan = xyspan,
                          lineLength = lineLength,
                          col = col, colScheme = colScheme, 
                          text_col = ptext_col, line_col = line_col)
      labelsxy$list <- lapply(annot, function(i) list(ax = i$ax, ay = i$ay))
      names(labelsxy$list) <- labs
      plotlyProxy('plotly', session) %>%
        plotlyProxyInvoke("relayout",
                          list(annotations = append(annot, custom_annotation)))
    })


  }

  shinyApp(ui, server)
  } else {
    labs <- startLabels
    annot <- annotation(labs, plotly_data, x, y,
                        labelchoices = labelchoices,
                        labSize = labSize, labelDir = labelDir,
                        labCentre = labCentre, xyspan = xyspan,
                        lineLength = lineLength,
                        col = col, colScheme = colScheme, 
                        text_col = ptext_col, line_col = line_col)
    annot <- c(annot, LRtitles, custom_annotation)
    
    main_plotly(plotly_data, x, y, 
                col, colScheme,
                shape, psymbols, outlier_psymbol,
                sizeSwitch, size, sizeRange,
                showOutliers, pt = 1,
                pmarker,
                hovertext, pkey,
                width, height,
                args,
                pxaxis, pyaxis,
                annot,
                legendxy, showLegend,
                pshapes)
  }
}

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
                        pshapes
                        ) {
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
                      col = 'white', border = 'black',
                      border_radius = 8, n = 20, ...) {
  if (border_radius == 0) {
    return(rect(xleft, ybottom, xright, ytop, col = col, border = border, ...))
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
  figheight <- (par("din")[2] - sum(par("mai")[c(1, 3)]))  # inches
  yi <- pix * diff(par("usr")[3:4]) / (figheight * 100)
  xi <- yi * diff(par("usr")[1:2]) / diff(par("usr")[3:4])
  xi <- xi * figheight / (par("din")[1] - sum(par("mai")[c(2, 4)]))
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
