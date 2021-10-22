# easyVolcano and easyMAplot functions

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
#' @param padj Name of the column containing adjusted p values (optional). If 
#' `y` is specified and `padj` is left blank or equal to `y`, nominal unadjusted 
#' p values are used for cut-off for significance instead of adjusted p values.
#' @param fdrcutoff Cut-off for FDR significance. Defaults to FDR < 0.05. If `y` 
#' is specified manually and `padj` is left blank then this refers to the 
#' cut-off for significant points using nominal unadjusted p values. 
#' @param fccut Optional vector of log fold change cut-offs.
#' @param colScheme Colour scheme. If no fold change cut-off is set, 2 colours
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
#' @return No return value
#' @importFrom stats p.adjust
#' @export


easyVolcano <- function(data, x = NULL, y = NULL, padj = y,
                        fdrcutoff = 0.05, fccut = NULL,
                        colScheme = c('darkgrey', 'blue', 'red'),
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
    if (!requireNamespace("qvalue", quietly = TRUE)) {
      stop("Can't find package qvalue Try:
           BiocManager::install('qvalue')",
           call. = FALSE)
    }
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
  
  # if using nominal p values
  fdr_or_p <- if (y == padj) "P<" else "FDR<"
  if (is.null(fccut)) {
    data$col <- factor(siggenes, levels = c(F, T),
                       labels = c('ns', paste0(fdr_or_p, fdrcutoff)))
    colScheme <- colScheme[1:2]
  } else {
    fccut <- abs(fccut)
    if (!(length(colScheme)-1) %in% ((length(fccut)+1) * 1:2)) {
      stop("Number of colours in 'colScheme' does not fit with number of cuts in 'fccut'")
    }
    siggenes <- as.numeric(siggenes)
    if (all(fccut == 0)) {
      # simple 3 colour version (grey: ns, blue: FC<0, red: FC>0)
      fc <- cut(data[,x], c(-Inf, 0, Inf))
      siggenes[siggenes == 1] <- as.numeric(fc[siggenes == 1])
      data$col <- factor(siggenes, levels = 0:(length(fccut) + 1),
                         labels = c('ns',
                                    paste0(fdr_or_p, fdrcutoff, ', FC<0'),
                                    paste0(fdr_or_p, fdrcutoff, ', FC>0')))
    } else if (length(colScheme) - 1 == length(fccut) + 1) {
      # symmetric colours
      fc <- cut(abs(data[, x]), c(-1, fccut, Inf))
      siggenes[siggenes == 1] <- as.numeric(fc[siggenes == 1])
      data$col <- factor(siggenes, levels = 0:(length(fccut) + 1),
                         labels = c('ns',
                                    paste0(fdr_or_p, fdrcutoff,
                                           ', FC<', fccut[1]),
                                    paste0(fdr_or_p, fdrcutoff, ', FC>', fccut)))
    } else {
      # asymmetric colours
      fccut <- sort(unique(c(fccut, 0, -fccut)))
      fc <- cut(data[,x], c(-Inf, fccut, Inf))
      siggenes[siggenes == 1] <- as.numeric(fc[siggenes == 1])
      data$col <- factor(siggenes, levels = 0:(length(fccut)+1),
                         labels = c('ns',
                                    paste0(fdr_or_p, fdrcutoff,
                                           ', FC<', fccut[1]),
                                    paste0(fdr_or_p, fdrcutoff, ', ',
                                           fccut[-length(fccut)],
                                           '<FC<', fccut[-1]),
                                    paste0(fdr_or_p, fdrcutoff, ', FC>',
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
            colScheme = colScheme, zeroline = FALSE, hline = fdrline,
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
#' @param padj Name of the column containing adjusted p values (optional). For DESeq2 and
#' limma objects this is automatically set. If `y` is specified and `padj` is 
#' left blank or equal to `y`, nominal unadjusted p values are used for cut-off 
#' for significance.
#' @param fdrcutoff Cut-off for FDR significance. Defaults to FDR < 0.05. Can
#' be vector with multiple cut-offs. To use nominal P values instead of adjusted 
#' p values, set `y` but leave `padj` blank.
#' @param colScheme Colour colScheme. Length must match either length(fdrcutoff) + 1
#' to allow for non-significant genes, or match length(fdrcutoff) * 2 + 1 to
#' accommodates asymmetric colour colSchemes for positive & negative fold change.
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
#' @return No return value
#' @importFrom stats p.adjust
#' @export


easyMAplot <- function(data, x = NULL, y = NULL, padj = y, fdrcutoff = 0.05,
                       colScheme = c('darkgrey', 'blue', 'red'),
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
    if (!requireNamespace("qvalue", quietly = TRUE)) {
      stop("Can't find package qvalue Try:
           BiocManager::install('qvalue')",
           call. = FALSE)
    }
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
  
  # if using nominal p values
  fdr_or_p <- if (y == padj) "P<" else "FDR<"
  if (!(length(colScheme) - 1) %in% (length(fdrcutoff) * 1:2)) {
    stop("Number of colours in 'colScheme' does not fit with number of cuts in 'fdrcut'")
  }
  fdrcuts <- cut(data[, padj], c(-1, fdrcutoff, Inf))
  siggenes <- length(fdrcutoff) + 1 - as.numeric(fdrcuts)
  siggenes[is.na(siggenes)] <- 0
  if ((length(colScheme) - 1 == length(fdrcutoff))) {
    # symmetric colours
    data$col <- factor(siggenes, levels = 0:length(fdrcutoff),
                       labels = c('ns', paste0(fdr_or_p, fdrcutoff)))
  } else {
    # asymmetric colours
    fc <- data[, y] > 0
    siggenes[fc & siggenes != 0] <- siggenes[fc & siggenes != 0] + length(fdrcutoff)
    data$col <- factor(siggenes, levels = 0:(length(fdrcutoff) * 2),
                       labels = c('ns', paste0('FC<0, ', fdr_or_p, fdrcutoff),
                                  paste0('FC>0, ', fdr_or_p, fdrcutoff)))
  }

  easylabel(data, x, y, col = 'col',
            ylab = ylab,
            xlab = xlab,
            filename = filename,
            colScheme = colScheme, zeroline = FALSE, hline = hline,
            labelDir = labelDir,
            custom_annotation = custom_annotation, ...)
}


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
#' @param outline_col Colour of symbol outlines. Passed to [easylabel()].
#' @param shapeScheme A single symbol for points or a vector of symbols. Passed 
#' to [easylabel()].
#' @param size Specifies point size. Passed to [easylabel()].
#' @param width Width of the plot in pixels. Saving to pdf scales 100 pixels to 
#' 1 inch.
#' @param lineLength Initial length of label lines in pixels.
#' @param npoints Maximum number of points to plot when saving to pdf. Defaults 
#' to plot the top 1 million points by p value. Setting a value of `NA` will 
#' plot all points.
#' @param nplotly Maximum number of points to display via plotly. We recommend 
#' the default setting of 100,000 points (or fewer).
#' @param npeaks Number of peaks to label initially.
#' @param span a peak is defined as the most significant SNP within a window of 
#' width span SNPs centred at that SNP. Large numbers take significantly longer.
#' @param transpose Logical whether to transpose the plot.
#' @param filename Filename for saving to pdf.
#' @param ... Other arguments passed to [easylabel()].
#' @seealso [easylabel()] [easyVolcano()]
#' @return No return value
#' @importFrom gtools mixedsort
#' @importFrom splus2R peaks
#' @export

easyManhattan <- function(data, chrom = 'chrom', pos = 'pos', p = 'p',
                          labs = 'rsid',
                          pcutoff = 5e-08,
                          chromGap = NULL,
                          chromCols = c('royalblue', 'skyblue'),
                          sigCol = 'red',
                          alpha = 0.7,
                          labelDir = 'horiz',
                          xlab = "Chromosome position",
                          ylab = expression("-log"[10] ~ "P"),
                          outline_col = NA,
                          shapeScheme = 16,
                          size = 6,
                          width = 1000,
                          lineLength = 60,
                          npoints = 1E6,
                          nplotly = 1E5,
                          npeaks = NULL,
                          span = min(c(nrow(data), npoints), na.rm = TRUE) / 200,
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
    data <- data[index[1:npoints], ]  # shrink dataset
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
  
  # find local maxima
  if (!is.null(npeaks)) {
    cat("Finding peaks...\n")
    span <- span + 1 - (span %% 2)
    pks <- splus2R::peaks(data$logP, span = span)
    pks_index <- which(pks)
    sort_pks <- pks_index[order(data$logP[pks_index], decreasing = TRUE, 
                                na.last = NA)]
    startLabels <- sort_pks[1:npeaks]
  } else startLabels <- NULL
  
  if (!transpose) {
    easylabel(data, x = 'genome_pos', y = 'logP',
            labs = labs,
            xlab = xlab, ylab = ylab,
            xticks = xticks,
            labelDir = labelDir,
            startLabels = startLabels,
            col = 'col', colScheme = colScheme, alpha = alpha,
            outline_col = outline_col,
            shapeScheme = shapeScheme,
            size = size,
            width = width,
            lineLength = lineLength,
            zeroline = FALSE,
            plotly_filter = 'plotly_filter',
            showLegend = FALSE,
            filename = filename,
            bty = 'n', las = 2, cex.axis = 0.9, ...)
  } else {
    if (!is.null(xticks)) {
      data$genome_pos <- -data$genome_pos
      xticks$at <- -xticks$at
    }
    easylabel(data, y = 'genome_pos', x = 'logP',
              labs = labs,
              xlab = ylab, ylab = xlab,
              yticks = xticks,
              labelDir = labelDir,
              startLabels = startLabels,
              col = 'col', colScheme = colScheme, alpha = alpha,
              outline_col = outline_col,
              shapeScheme = shapeScheme,
              size = size,
              width = width,
              lineLength = lineLength,
              zeroline = FALSE,
              plotly_filter = 'plotly_filter',
              showLegend = FALSE,
              filename = filename,
              bty = 'n', las = 2, cex.axis = 0.9, ...)
  }
}
