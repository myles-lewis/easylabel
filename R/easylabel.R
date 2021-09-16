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
#' * You can move the legend as well.
#' * Switch to SVG when finalised (only do this at last moment as otherwise
#'   editing is very slow).
#' * Press camera button in modebar to save image as SVG.
#' * The Table tab shows a table view of the dataset to help with annotation.
#' @param data Dataset to use for plot.
#' @param x specifies column of x coordinates in `data`.
#' @param y specifies column of y coordinates in `data`.
#' @param col specifies the column (ideally a factor) in `data` with which
#' to colour points.
#' @param labs specifies the column in `data` with label names for points.
#' If `NULL` defaults to use `rownames(data)`.
#' @param scheme vector of colours for points.
#' @param xlab x axis title.
#' @param ylab y axis title.
#' @param startLabels vector of initial labels.
#' @param xlim the x limits (x1, x2) of the plot.
#' @param ylim the y limits of the plot.
#' @param symbols passed to plotly to specify symbols for normal points and
#' outliers.
#' @param showOutliers Logical whether to show outliers on the margins of the
#' plot.
#' @param width Width of the plot in pixels.
#' @param height Height of the plot in pixels.
#' @param showgrid Logical whether to show plotly gridlines.
#' @param zeroline Logical whether to show lines at x=0 and y=0.
#' @param hline Adds horizontal lines at values of y.
#' @param vline Adds vertical lines at values of x.
#' @param markerSize Size of markers as per plotly.
#' @param alpha Alpha value for transparency of points.
#' @param markerOutline List of plotly arguments to define marker outlines.
#' @param marker List of arguments to control plotly markers.
#' @param custom_annotation List of annotations to be added via [plotly::layout()].
#' @seealso [plot_ly()]
#' @importFrom shiny fluidPage tabsetPanel tabPanel fluidRow column
#' radioButtons selectizeInput actionButton checkboxGroupInput observe
#' updateSelectizeInput reactiveValues isolate reactive debounce
#' observeEvent modalDialog textAreaInput tagList modalButton showModal
#' removeModal h5 shinyApp
#' @importFrom plotly plot_ly layout plotlyOutput renderPlotly event_data
#' event_register config plotlyProxy plotlyProxyInvoke add_markers %>%
#' @importFrom RColorBrewer brewer.pal
#' @importFrom DT dataTableOutput datatable formatSignif
#' @export


easylabel <- function(data, x, y, col, labs=NULL, scheme=NULL, xlab=x, ylab=y, startLabels=NULL,
                      xlim=NULL, ylim=NULL, symbols=c('circle', 'diamond-open'),
                      showOutliers=TRUE,
                      width=800, height=600,
                      showgrid=FALSE, zeroline=TRUE, hline=NULL, vline=NULL,
                      markerSize=8, alpha=1,
                      markerOutline=list(width=0.5, color='white'),
                      marker=list(size=markerSize, opacity=alpha, line=markerOutline),
                      custom_annotation=NULL
                     ) {
  data$outlier <- FALSE
  if (!is.null(ylim)) {
    notNA <- !is.na(data[,y])
    data$outlier[notNA & (data[, y] < ylim[1] | data[, y] > ylim[2])] <- TRUE
    data[notNA & data[, y] < ylim[1], y] <- ylim[1]
    data[notNA & data[, y] > ylim[2], y] <- ylim[2]
    yspan <- ylim[2] - ylim[1]
    ylim[1] <- ylim[1] - yspan * 0.025
    ylim[2] <- ylim[2] + yspan * 0.025
  }
  if (!is.null(xlim)) {
    notNA <- !is.na(data[,x])
    data$outlier[notNA & (data[, x] < xlim[1] | data[, x] > xlim[2])] <- TRUE
    data[notNA & data[, x] < xlim[1], x] <- xlim[1]
    data[notNA & data[, x] > xlim[2], x] <- xlim[2]
    xspan <- xlim[2] - xlim[1]
    xlim[1] <- xlim[1] - xspan * 0.025
    xlim[2] <- xlim[2] + xspan * 0.025
  }
  if (!showOutliers) {
    data <- data[!data$outlier, ]
    showOutliers <- 1
  } else {
    if (any(data$outlier)) {
      showOutliers <- 2
      data$symbol <- factor(as.numeric(data$outlier) +1, labels=c(" ", "Outlier"))
    } else showOutliers <- 1
  }

  if (is.null(scheme)) scheme <- brewer.pal(nlevels(data[,col]), "Set1")
  labelchoices <- if (is.null(labs)) rownames(data) else data[, labs]
  startLabels <- startLabels[startLabels %in% labelchoices]
  start_annot <- annotation(startLabels, data, x, y)
  start_xy <- lapply(start_annot, function(i) list(ax=i$ax, ay=i$ay))
  names(start_xy) <- startLabels

  ui <- fluidPage(
    tabsetPanel(
      tabPanel("Plot",
        fluidRow(
          plotlyOutput("plotly", height=paste0(height, "px"))),
        fluidRow(column(3,
                        radioButtons("ptype", label=h5("Plot type"), choices=c("WebGL (fast)"=1, "SVG (slow)"=2), selected=1)),
                 column(4,
                        selectizeInput('label', h5('Select labels'), choices=NULL,
                                       options=list(onInitialize = I('function() { this.setValue(""); }')),
                                       multiple=T)),
                 column(4,
                        actionButton("add_batch", "Add batch"),
                        actionButton("clear", "Clear all"))
        )
      ),
      tabPanel("Table",
        fluidRow(
          column(2,
                 checkboxGroupInput('selgroup', 'Select groups', levels(data[, col]), selected = levels(data[, col]))
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
      # isolate(labs <- input$label)
      labs <- startLabels
      isolate(pt <- as.numeric(input$ptype))
      annot <- annotation(labs, data, x, y)
      isolate(labels$annot <- annot)
      if (!is.null(hline)) {
          shapes=lapply(hline, function(i) {
            list(type="line", line=list(width=1, color='#AAAAAA', dash='dash'), x0=0, x1=1, y0=i, y1=i, xref="paper")
          })
      } else {
        shapes=list()
      }
      if (!is.null(vline)) {
        shapes=c(shapes, lapply(vline, function(i) {
          list(type="line", line=list(width=1, color='#AAAAAA', dash='dash'), y0=0, y1=1, x0=i, x1=i, yref="paper")
        }))
      }

      switch(showOutliers,
             # no outliers
             plot_ly(data=data, x=as.formula(paste0('~', x)), y=as.formula(paste0('~', y)), type=switch(pt, 'scattergl', 'scatter'),
                     mode='markers', color=as.formula(paste0('~', col)), colors=scheme,
                     marker=marker,
                     text=labelchoices, hoverinfo='text',
                     key=labelchoices, source='lab_plotly',
                     width=width, height=height) %>%
               layout(xaxis=list(title=xlab, showgrid=showgrid, color='black', ticklen=5, showline=TRUE, zeroline=zeroline),
                      yaxis=list(title=ylab, showgrid=showgrid, color='black', ticklen=5, showline=TRUE, zeroline=zeroline)),
             # with outliers
             plot_ly(data=data[!data$outlier,], x=as.formula(paste0('~', x)), y=as.formula(paste0('~', y)), type=switch(pt, 'scattergl', 'scatter'),
                     mode='markers', color=as.formula(paste0('~', col)), colors=scheme,
                     marker=marker,
                     text=labelchoices[!data$outlier], hoverinfo='text',
                     key=labelchoices[!data$outlier], source='lab_plotly',
                     legendgroup='Main',
                     width=width, height=height) %>%
               add_markers(data=data[data$outlier,], x=as.formula(paste0('~', x)), y=as.formula(paste0('~', y)), type=switch(pt, 'scattergl', 'scatter'),
                           color=as.formula(paste0('~', col)), colors=scheme,
                           marker=marker,
                           symbol=I(symbols[2]),
                           text=labelchoices[data$outlier], hoverinfo='text',
                           key=labelchoices[data$outlier], legendgroup='outlier', name='outlier') %>%
               layout(
                 xaxis=list(range=as.list(xlim), title=xlab, showgrid=showgrid, color='black', ticklen=5, showline=TRUE, zeroline=zeroline),
                 yaxis=list(range=as.list(ylim), title=ylab, showgrid=showgrid, color='black', ticklen=5, showline=TRUE, zeroline=zeroline))
      ) %>%
        layout(annotations=append(annot, custom_annotation),
               hovermode='closest',
               legend = list(font=list(color='black')),
               shapes = shapes) %>%
        # highlight(on="plotly_selected", opacityDim = 1) %>%
        config(edits = list(annotationTail = TRUE, legendPosition = TRUE),
               plotGlPixelRatio = 6,
               toImageButtonOptions=list(format="svg")) %>%
        event_register(event='plotly_click')

    })

    updateSelectizeInput(session, 'label', choices = labelchoices, server = TRUE)
    labels <- reactiveValues(list=startLabels)
    labelsxy <- reactiveValues(list=start_xy)

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
    input_label <- reactive({input$label}) %>% debounce(150)

    # Update labels from selectize
    observeEvent(input_label(), {
      currentsel <- input_label()
      if (length(currentsel)==0) {labels$list <- character(0)
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
      annot <- annotation(labs, data, x, y, current_xy)
      labelsxy$list <- lapply(annot, function(i) list(ax=i$ax, ay=i$ay))
      names(labelsxy$list) <- labs
      plotlyProxy('plotly', session) %>%
        plotlyProxyInvoke("relayout", list(annotations=append(annot, custom_annotation)))
      sel_labels <- input$label
      if (any(!sel_labels %in% labs) | any(!labs %in% sel_labels)) {
        updateSelectizeInput(session, 'label', choices = labelchoices,
                             selected=labs, server = TRUE)
      }

    })

    # update plot type without replotting
    observeEvent(input$ptype, {
      pt <- as.numeric(input$ptype)
      plotlyProxy('plotly', session) %>%
        plotlyProxyInvoke("restyle", list(type=switch(pt, 'scattergl', 'scatter')))
    })

    # store the location of moved labels by detecting plotly relayout events of the type `annotations[1].ax`
    observeEvent(event_data("plotly_relayout", source='lab_plotly'), {
      s <- event_data("plotly_relayout", source='lab_plotly')
      if (grepl("annotations\\[[0-9]+\\]\\.ax", names(s)[[1]])) {
        w <- as.numeric(gsub("annotations\\[|\\]\\.ax", "", names(s)[1])) +1
        labelsxy$list[[w]] <- list(ax=s[[1]], ay=s[[2]])
      }
    })

    output$table <- DT::renderDataTable({
      cols <- colnames(data)[sapply(data, class) == "numeric"]
      df <- data[data[,col] %in% input$selgroup, ]
      rn <- if (is.null(labs)) {
        TRUE
      } else {
        labelchoices[data[,col] %in% input$selgroup, ]
      }
      datatable(df, rownames = rn) %>% formatSignif(cols, digits=3)
    })

    observeEvent(input$clear, {
      labels$list <- list()
    })

    # Modal for batch gene input
    batchModal <- function() {
      modalDialog(
        textAreaInput("batchlabs", "Type or paste a list of genes", height = '300px'),
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

    }

  shinyApp(ui, server)

}


#' Interactive volcano plot labels
#'
#' Interactive labelling of volcano plots using shiny/plotly interface.
#'
#' @param data The dataset for the plot. Automatically attempts to recognises
#' DESeq2 and limma objects.
#' @param x column containing log fold change. For DESeq2 and limma objects
#' this is automatically set.
#' @param y column containing p values. For DESeq2 and limma objects this is
#' automatically set.
#' @param fdrcutoff Cut-off for FDR significance. Defaults to FDR < 0.05
#' @param fccut Log fold change cut-off. Optional, where 0 means not applied.
#' @param scheme Colour scheme. If no fold change cut-off is set, 2 colours
#' need to be specified. With a fold change cut-off, 3 colours are required.
#' @param showCounts Logical whether to show legend with number of
#' differentially expressed genes.
#' @param useQ Logical whether to convert nominal P values to q values.
#' Requires the qvalue Bioconductor package.
#' @param ... Other arguments passed to [easylabel()].
#' @seealso [easylabel()]
#' @importFrom qvalue qvalue
#' @export

volcanoplot <- function(data, x=NULL, y=NULL, fdrcutoff=0.05, fccut=0, scheme=c('darkgrey', 'blue', 'red'),
                        showCounts=TRUE, useQ=FALSE, ...) {
  if (is.null(x)) {
    if ('log2FoldChange' %in% colnames(data)) x='log2FoldChange'  # DESeq2
    if ('logFC' %in% colnames(data)) x='logFC'  # limma
  }
  if (is.null(y)) {
    if ('pvalue' %in% colnames(data)) {
      y='pvalue'  # DESeq2
      padj='padj'
    }
    if ('P.Value' %in% colnames(data)) {
      y='P.Value'  # limma
      padj='adj.P.Val'
    }
  }
  data[, 'log10P'] <- -log10(data[, y])
  if (useQ) {
    data$qvalue <- NA
    data$qvalue[!is.na(data[, padj])] <- qvalue::qvalue(data[!is.na(data[, padj]), y])$qvalues
    siggenes <- data$qvalue < fdrcutoff
  } else {
    siggenes <- data[, padj] < fdrcutoff
  }
  siggenes[is.na(siggenes)] <- FALSE
  if (sum(siggenes) >0) fdrline <- min(data[siggenes, 'log10P'])
  fccut <- abs(fccut)
  if (showCounts) {
    up <- sum(siggenes & data[, x] > fccut)
    down <- sum(siggenes & data[, x] <= -fccut)
    total <- nrow(data)
    custom_annotation <- list(list(x=1.18, y=0.02, align='left',
                                   text=paste0(up, ' upregulated<br>',
                                               down, ' downregulated<br>',
                                               total, ' total genes'),
                                   font = list(size=11, color = "black"),
                                   xref='paper', yref='paper', showarrow=F))
  } else custom_annotation=NULL
  if (fccut==0) {
    data$col <- factor(siggenes, levels=c(F, T), labels=c('ns', paste0('FDR<', fdrcutoff)))
  } else {
    fc <- abs(data[siggenes, x]) > fccut
    siggenes <- as.numeric(siggenes)
    siggenes[siggenes==1] <- as.numeric(fc) + 1
    data$col <- factor(siggenes, levels=0:2, labels=c('ns', paste0('FDR<', fdrcutoff, ', FC<', fccut),
                                                      paste0('FDR<', fdrcutoff, ', FC>', fccut)))
  }
  y <- 'log10P'
  # this line removes a few genes which have P value < FDR cutoff but are excluded by DESeq2
  if (!is.null(fdrline)) data <- data[!(is.na(data[, padj]) & data$log10P > fdrline), ]
  easylabel(data, x, y, col='col', xlab='log<sub>2</sub> fold change',
            ylab='-log<sub>10</sub> P', scheme=scheme, zeroline=FALSE, hline=fdrline,
            custom_annotation=custom_annotation, ...)
}

# Annotate gene labels
annotation <- function(labels, data, x, y, current_xy=NULL) {
  if (length(labels)!=0) {
    annot <- lapply(1:length(labels), function(j) {
      i <- labels[j]
      row <- data[i, ]
      sx <- row[, x]
      sy <- row[, y]
      z <- sqrt(sx^2 + sy^2)
      ax <-sx/z*75
      ay <- -sy/z*75
      if (j <= length(current_xy)) {
        if (!is.null(current_xy[[j]])) {
          ax=current_xy[[j]]$ax
          ay=current_xy[[j]]$ay
        }
      }
      list(x=sx, y=sy, ax=ax, ay=ay,
           text=i, textangle=0,
           font=list(color="black", size=12),
           arrowcolor="black", arrowwidth=1, arrowhead=0, arrowsize=1.5,
           xanchor="auto", yanchor="auto")
    })
  } else {annot <- list()}
  annot
}
