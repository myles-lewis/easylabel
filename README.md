### easylabel

Install from Github
```
devtools::install_github("myles-lewis/easylabel")
library(easylabel)
```

Instructions:
1. Hover over and click on/off genes which you want to label.
2. When you have selected all your chosen genes, then drag gene names to move label positions.
3. You can move the legend as well.
4. Switch to SVG when finalised (only do this at last moment as otherwise editing is very slow).
5. Press camera button in modebar to save image as SVG.

Examples:

```
setwd("/Users/myles/R/R4RA/")
ymatrix <- readRDS('ymatrix_drug_time.rds')
volc1 <- readRDS('DESeq2.nociceptive.res.RDS')
volc2 <- readRDS('limma.output.RDS')
scheme <- c('grey', 'green3', 'gold3', 'blue')

easylabel(ymatrix, 'x', 'y', 'col', scheme=scheme,
  xlab="log<sub>2</sub> fold change post-Rituximab",
  ylab="log<sub>2</sub> fold change post-Tocilizumab")

volcanoplot(volc1)
volcanoplot(volc1, fccut=1.25, fdrcutoff=0.2, ylim=c(0, 6), xlim=c(-5,5))
volcanoplot(volc1, fccut=1.25, fdrcutoff=0.2, ylim=c(0, 6), xlim=c(-5,5), show_outliers=F)
volcanoplot(volc1, fccut=1.25, fdrcutoff=0.2, vline=c(-1.25, 1.25))
volcanoplot(volc2, fccut=0.5, start_labels=c('MS4A1', 'IL6'))
```
