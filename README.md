### easylabel

Install from local repo
```
setwd("/users/.../")
devtools::install("easylabel")
library(easylabel)
```

Install from Github
```
devtools::install_github("myles-lewis/easylabel", auth_token="...")
library(easylabel)
```

Instructions:
* Hover over and click on/off genes which you want to label.
* When you have selected all your chosen genes, then drag gene names to move
  label positions.
* Click the save button to export a PDF in base graphics.

To export an SVG from plotly: 
* You can move the legend as well.
* Switch to SVG when finalised (only do this at last moment as otherwise
  editing is very slow).
* Press camera button in modebar to save image as SVG.

Examples:

```
ymatrix <- readRDS('/Users/myles/R/R4RA/ymatrix_drug_time.rds')
volc1 <- readRDS('/Users/myles/R/R4RA/DESeq2.nociceptive.res.RDS')

easylabel(ymatrix, x = 'x', y = 'y', col = 'col',
          scheme = c('darkgrey', 'green3', 'gold3', 'blue'),
          xlab = expression("log"[2] ~ " fold change post-Rituximab"),
          ylab = expression("log"[2] ~ " fold change post-Tocilizumab"),
          showgrid = TRUE, fullname = TRUE)

volcanoplot(volc1, useQ = TRUE, fullname = TRUE)

volcanoplot(volc1, fccut = c(1, 2), fdrcutoff = 0.2, ylim = c(0, 6), xlim=c(-5,5),
            scheme=c('darkgrey', 'blue', 'orange', 'red'), useQ = TRUE)

volcanoplot(volc1, fccut = 1, fdrcutoff = 0.2, ylim = c(0, 6), xlim=c(-5,5),
            scheme=c('darkgrey', 'blue', 'lightblue', 'orange', 'red'), vline=c(-1,1))

volcanoplot(volc1, fccut = c(1, 2), fdrcutoff = 0.2, ylim = c(0, 6), xlim=c(-5,5),
            scheme=c('darkgrey', brewer.pal(9, 'RdYlBu')[-(4:6)]), alpha=0.75, outline_col=NA)
```
