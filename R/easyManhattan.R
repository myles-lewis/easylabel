
#' Interactive Manhattan plot labels
#'
#' Interactive labelling of Manhattan plots using 'shiny' and 'plotly' 
#' interface.
#'
#' @param data The dataset (data.frame or data.table) for the plot.
#' @param chrom The column of chomosome values in `data`.
#' @param pos The column of SNP positions in `data`.
#' @param p The column of p values in `data`.
#' @param labs The column of labels in `data`.
#' @param startLabels Vector of initial labels. With a character vector, labels 
#' are identified in the column specified by `labs`. With a numeric vector,
#' points to be labelled are referred to by row number.
#' @param pcutoff Cut-off for p value significance. Defaults to 5E-08.
#' @param chromGap Size of gap between chromosomes along the x axis in base 
#' pairs. If `NULL` this is automatically calculated dependent on the size of 
#' the genome. Default is around 3E07 for a human genome, and smaller for 
#' smaller genomes.
#' @param chromCols A vector of colours for points by chromosome. Colours are 
#' recycled dependent on the length of the vector.
#' @param sigCol Colour for statistically significant points. Ignored if set to 
#' `NA`.
#' @param alpha Transparency for points.
#' @param labelDir Option for label lines. See [easylabel()].
#' @param xlab x axis title. Accepts expressions.
#' @param ylab y axis title. Accepts expressions.
#' @param xlim The x limits (x1, x2) of the plot.
#' @param ylim The y limits of the plot.
#' @param outline_col Colour of symbol outlines. Passed to [easylabel()].
#' @param shapeScheme A single symbol for points or a vector of symbols. Passed 
#' to [easylabel()].
#' @param size Specifies point size. Passed to [easylabel()].
#' @param width Width of the plot in pixels. Saving to pdf scales 100 pixels to 
#' 1 inch.
#' @param height Height of the plot in pixels.
#' @param lineLength Initial length of label lines in pixels.
#' @param npoints Maximum number of points to plot when saving to pdf. Defaults 
#' to plot the top 1 million points by p value. Setting a value of `NA` will 
#' plot all points.
#' @param nplotly Maximum number of points to display via plotly. We recommend 
#' the default setting of 100,000 points (or fewer).
#' @param npeaks Number of peaks to label initially.
#' @param span a peak is defined as the most significant SNP within a window of 
#' width span centred at that SNP.
#' @param transpose Logical whether to transpose the plot.
#' @param filename Filename for saving to pdf.
#' @param ... Other arguments passed to [easylabel()].
#' @seealso [easylabel()] [easyVolcano()]
#' @return By default no return value. If `output_shiny = FALSE` or the shiny 
#' button 'Export plotly & exit' is pressed, a plotly figure is returned. 
#' See [easylabel()].
#' @importFrom gtools mixedsort
#' @export

easyManhattan <- function(data, chrom = 'chrom', pos = 'pos', p = 'p',
                          labs = 'rsid',
                          startLabels = NULL,
                          pcutoff = 5e-08,
                          chromGap = NULL,
                          chromCols = c('royalblue', 'skyblue'),
                          sigCol = 'red',
                          alpha = 0.7,
                          labelDir = 'horiz',
                          xlab = "Chromosome position",
                          ylab = expression("-log"[10] ~ "P"),
                          xlim = NULL,
                          ylim = NULL,
                          outline_col = NA,
                          shapeScheme = 16,
                          size = 6,
                          width = ifelse(transpose, 600, 1000),
                          height = ifelse(transpose, 800, 600),
                          lineLength = 60,
                          npoints = 1E6,
                          nplotly = 1E5,
                          npeaks = NULL,
                          span = 2e7,
                          transpose = FALSE,
                          filename = NULL, ...) {
  if (is.null(filename)) filename <- paste0("manhattan_",
                                            deparse(substitute(data)))
  # PLINK headings
  if (!chrom %in% colnames(data)) {
    if ("CHR" %in% colnames(data)) {chrom <- "CHR"
    } else stop(paste0("Column '", chrom, "' not found"))
  }
  if (!pos %in% colnames(data)) {
    if ("BP" %in% colnames(data)) {pos <- "BP"
    } else stop(paste0("Column '", pos, "' not found"))
  }
  if (!p %in% colnames(data)) {
    if ("P" %in% colnames(data)) {p <- "P"
    } else stop(paste0("Column '", p, "' not found"))
  }
  if (!labs %in% colnames(data)) {
    if ("SNP" %in% colnames(data)) {labs <- "SNP"
    } else stop(paste0("Column '", labs, "' not found"))
  }
  index <- order(data[,p])
  data$plotly_filter <- FALSE
  data$plotly_filter[index[1:nplotly]] <- TRUE
  if (!is.na(npoints) & nrow(data) > npoints) {
    # thin points near x axis
    s1 <- seq_len(nplotly)
    s2len <- nrow(data) - nplotly
    s2 <- round(seq_len(npoints - nplotly) * s2len / (npoints - nplotly)) + nplotly
    s3 <- rev(nrow(data) - seq_len(nplotly) +1)
    data <- data[index[unique(c(s1, s2, s3))], ]
  }
  data$logP <- -log10(data[, p])
  chrom_list <- gtools::mixedsort(unique(data[, chrom]), na.last = NA)
  data[, chrom] <- factor(data[, chrom], levels=chrom_list)
  maxpos <- tapply(data[, pos], data[, chrom], max, na.rm = TRUE)
  maxpos <- maxpos[chrom_list]  # reorder
  minpos <- tapply(data[, pos], data[, chrom], min, na.rm = TRUE)
  minpos <- minpos[chrom_list]  # reorder
  # calculate gap
  if (is.null(chromGap)) {
    chromGap <- sum(maxpos - minpos) / length(chrom_list) / 4.15
  }
  chrom_cumsum <- c(0, cumsum(maxpos - minpos + chromGap))
  chrom_cumsum2 <- chrom_cumsum - c(minpos, 0)
  chrom_cumsum <- chrom_cumsum[1:length(maxpos)]
  chrom_cumsum2 <- chrom_cumsum2[1:length(maxpos)]
  data$genome_pos <- data[, pos] + chrom_cumsum2[as.numeric(data[, chrom])]
  data <- data[order(data$genome_pos), ]
  data$col <- ((as.numeric(data[, chrom]) - 1) %% length(chromCols)) + 1
  colScheme <- chromCols
  if (!is.na(sigCol)) {
    data$col[data[, p] < pcutoff] <- length(chromCols) + 1
    colScheme <- c(chromCols, sigCol)
  }
  if (length(chrom_list) > 1) {
    xticks <- list(at = chrom_cumsum + 0.5 * (maxpos - minpos), 
                   labels = levels(data[, chrom]))
  } else xticks <- NULL
  
  # labelchoices <- if (is.null(labs)) rownames(data) else data[[labs]]
  if (is.character(startLabels)) {
    startLabels <- which(data[[labs]] %in% startLabels)
  } 
  
  # find local maxima
  if (!is.null(npeaks)) {
    message("Finding peaks...")
    i <- 2
    index <- order(data[, p])
    pks <- index[1]
    while (length(pks) < npeaks & i < length(index)) {
      if (all(abs(data[index[i], 'genome_pos'] - data[pks, 'genome_pos']) > span)) {
        pks <- c(pks, index[i])
      }
      i <- i + 1
    }
    rsLabels <- c(startLabels, pks)
  } else {rsLabels <- startLabels}
  
  # fix x axis for locus plots
  if(length(unique(data$chrom)) == 1) data$genome_pos <- data$pos
  
  # fix ylim
  lim <- range(data$logP, na.rm = TRUE)
  lim[2] <- lim[2] + diff(lim) * 0.02
  glim <- range(data$genome_pos, na.rm = TRUE)
  glim <- glim + diff(glim) * 0.01 * c(-1, 1)
  
  if (!transpose) {
    if (is.null(xlim)) xlim <- glim
    if (is.null(ylim)) ylim <- lim
    easylabel(data, x = 'genome_pos', y = 'logP',
              labs = labs,
              xlab = xlab, 
              ylab = ylab,
              xlim = xlim,
              ylim = ylim,
              xticks = xticks,
              labelDir = labelDir,
              startLabels = rsLabels,
              col = 'col', colScheme = colScheme, alpha = alpha,
              outline_col = outline_col,
              shapeScheme = shapeScheme,
              size = size,
              width = width, height = height,
              lineLength = lineLength,
              zeroline = FALSE,
              plotly_filter = 'plotly_filter',
              showLegend = FALSE,
              filename = filename,
              bty = 'l', xaxs = "i", yaxs = "i", las = 1, cex.axis = 0.85, ...)
  } else {
    # transposed
    if (!is.null(xticks)) {
      data$genome_pos <- -data$genome_pos
      xticks$at <- -xticks$at
    }
    if (is.null(xlim)) xlim <- lim
    if (is.null(ylim)) ylim <- rev(-glim)
    easylabel(data, y = 'genome_pos', x = 'logP',
              labs = labs,
              xlab = ylab, ylab = xlab,
              xlim = xlim,
              ylim = ylim,
              yticks = xticks,
              labelDir = labelDir,
              startLabels = startLabels,
              col = 'col', colScheme = colScheme, alpha = alpha,
              outline_col = outline_col,
              shapeScheme = shapeScheme,
              size = size,
              width = width, height = height,
              lineLength = lineLength,
              zeroline = FALSE,
              plotly_filter = 'plotly_filter',
              showLegend = FALSE,
              filename = filename,
              bty = 'l', xaxs = "i", yaxs = "i", las = 1, cex.axis = 0.9, ...)
  }
}
