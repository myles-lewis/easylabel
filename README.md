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
(see vignette)

```
ymatrix <- readRDS('/Users/myles/R/R4RA/ymatrix_drug_time.rds')
easylabel(ymatrix, x = 'x', y = 'y', col = 'col',
          colScheme = c('darkgrey', 'green3', 'gold3', 'blue'),
          xlab = expression("log"[2] ~ " fold change post-Rituximab"),
          ylab = expression("log"[2] ~ " fold change post-Tocilizumab"),
          showgrid = TRUE)
```
Use the `volcanoplot()` function to quickly plot a volcano plot from DESeq2 or
EdgeR objects. The `useQ` argument will switch to using q values for FDR.

```
volc1 <- readRDS('/Users/myles/R/R4RA/DESeq2.nociceptive.res.RDS')
volcanoplot(volc1, useQ = TRUE)
```

Use the `MAplot()` function to quickly plot an MA plot from DESeq2 or EdgeR
objects.

```
MAplot(volc1, useQ = TRUE)
```

The `fullGeneNames` argument will use Bioconductor package `AnnotationDbi` and
the `org.Hs.eg.db` human gene database to expand gene symbols in the Table tab.
Both will need to be installed from Bioconductor.

```
BiocManager::install("AnnotationDbi")
BiocManager::install("org.Hs.eg.db")
volcanoplot(volc1, useQ = TRUE, fullGeneNames = TRUE)
```

You can add left and right sided titles using `Ltitle` and `Rtitle` to explain
the direction of effect for up/downregulation. The use of `expression` in the
example below shows how to add left/right arrow symbols to the titles.
`LRtitle_side = 1` puts these titles on the bottom while `= 3` puts them on the
top. `cex.lab` controls font size for these titles as well as axis titles.
`cex.axis` controls font size for axis numbering.

```
volcanoplot(volc1, useQ = TRUE, fullGeneNames = TRUE,
            Ltitle = expression(symbol("\254") ~ "Non-responder"),
            Rtitle = expression("Responder" ~ symbol("\256")),
            LRtitle_side = 1,
            cex.lab = 0.9, cex.axis = 0.8)
```

The colour scheme system has been expanded to allow multiple fold change
cut-offs. In this example the colours are symmetrical.

```
volcanoplot(volc1, fccut = c(1, 2), fdrcutoff = 0.2, 
            ylim = c(0, 6), xlim=c(-5,5),
            colScheme=c('darkgrey', 'blue', 'orange', 'red'), useQ = TRUE)
```

In the next 2 plots, the colours range from blue for downregulated genes,
through to red for upregulated genes.

```
volcanoplot(volc1, fccut = 1, fdrcutoff = 0.2, 
            ylim = c(0, 6), xlim = c(-5, 5),
            colScheme=c('darkgrey', 'blue', 'lightblue', 'orange', 'red'), 
            vline = c(-1, 1))
```

The next example has 6 colours and also shows how to remove the white outlines
around points and use transparency instead.

```
library(RColorBrewer)
volcanoplot(volc1, fccut = c(1, 2), fdrcutoff = 0.2, ylim = c(0, 6), xlim=c(-5,5),
            colScheme = c('darkgrey', brewer.pal(9, 'RdYlBu')[c(9:7, 3:1)])), 
            alpha = 0.75, outline_col = NA)
```

A box around the plot can be added using `bty = 'o'`.
Axes can be customised by first suppressing the initial axis using `xaxt = 'n'` 
or `yaxt = 'n'` and then adding an `axis()` call using `panel.last`.
A top title can also be added using `main`. 

```
volcanoplot(volc1, useQ = TRUE, fullGeneNames = TRUE,
            xaxt = 'n', yaxt = 'n', bty = 'o', 
            main = "DEG volcano plot",
            panel.last = {
              axis(side = 1, at = -6:6)
              axis(side = 2, at = 0:12)
            })
```

