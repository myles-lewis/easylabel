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
#' @param padj Name of the column containing adjusted p values. Cannot be NULL
#' when y is not NULL.
#' @param fdrcutoff Cut-off for FDR significance. Defaults to FDR < 0.05
#' @param fccut Optional vector of log fold change cut-offs.
#' @param colScheme Colour scheme. If no fold change cut-off is set, 2 colours
#' need to be specified. With a single fold change cut-off, 3 or 5 colours are
#' required, depending on whether the colours are symmetrical about x = 0.
#' Accommodates asymmetric colour colSchemes with multiple fold change cut-offs
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
#' @importFrom stats p.adjust
#' @export


easyVolcano <- function(data, x = NULL, y = NULL, padj = NULL,
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

  if (is.null(fccut)) {
    data$col <- factor(siggenes, levels = c(F, T),
                       labels = c('ns', paste0('FDR<', fdrcutoff)))
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
                                    paste0('FDR<', fdrcutoff, ', FC<0'),
                                    paste0('FDR<', fdrcutoff, ', FC>0')))
    } else if (length(colScheme) - 1 == length(fccut) + 1) {
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
#' @param padj Name of the column containing adjusted p values. For DESeq2 and
#' limma objects this is automatically set.
#' @param fdrcutoff Cut-off for FDR significance. Defaults to FDR < 0.05. Can
#' be vector with multiple cut-offs.
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
#' @importFrom stats p.adjust
#' @export


easyMAplot <- function(data, x = NULL, y = NULL, padj = NULL, fdrcutoff = 0.05,
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

  if (!(length(colScheme) - 1) %in% (length(fdrcutoff) * 1:2)) {
    stop("Number of colours in 'colScheme' does not fit with number of cuts in 'fdrcut'")
  }
  fdrcuts <- cut(data[, padj], c(-1, fdrcutoff, Inf))
  siggenes <- length(fdrcutoff) + 1 - as.numeric(fdrcuts)
  siggenes[is.na(siggenes)] <- 0
  if ((length(colScheme) - 1 == length(fdrcutoff))) {
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
            colScheme = colScheme, zeroline = FALSE, hline = hline,
            labelDir = labelDir,
            custom_annotation = custom_annotation, ...)
}
