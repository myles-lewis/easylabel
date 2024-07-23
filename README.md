# easylabel

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/easylabel)](https://cran.r-project.org/package=easylabel)
[![Downloads](https://cranlogs.r-pkg.org/badges/easylabel)](https://CRAN.R-project.org/package=easylabel)

A common problem in R is labelling scatter plots with large numbers of points
and/or labels. We provide a utility for easy labelling of scatter plots, and 
quick plotting of volcano plots and MA plots for gene expression analyses as 
well as Manhattan plots for genetic analyses. Using an interactive shiny 
and plotly interface, users can hover over points to see where specific points 
are located and click on points to easily label them. Labels can be toggled 
on/off simply by clicking. An input box and batch input window provides an easy 
way to label points by name. Labels can be dragged around the plot to place 
them optimally, and the finished plot can be easily exported directly to PDF 
for publication. Plots and labels are highly customisable allowing for control 
over label and line colour with options including rounded label boxes and 
matching of label colours to point colours.

## Installation

Install from CRAN
```
install.packages("easylabel")
library(easylabel)
```

Install from Github
```
devtools::install_github("myles-lewis/easylabel")
```

If you wish to use the optional `useQ` function with `easyVolcano()` and 
`easyMAplot()`, you will need to install additional package `qvalue` from 
Bioconductor:
```
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("qvalue")
```

If you wish to use the optional `fullGeneNames` function, you will need to 
install packages `AnnotationDbi` and `org.Hs.eg.db` from Bioconductor:
```
BiocManager::install("AnnotationDbi")
BiocManager::install("org.Hs.eg.db")
```

## Quick start
Simple instructions:
* Hover over and click on/off genes which you want to label.
* When you have selected all your chosen genes, then drag gene names to move
  label positions.
* Click the save button to export a PDF in base graphics.

To export an SVG from plotly: 
* Switch to SVG when finalised (only do this at last moment as otherwise
  editing is very slow).
* Press camera button in modebar to save image as SVG.

## Examples
### Simple scatter plot
Simple scatter plot from the gapminder dataset setting colours and marker shapes 
and introducing the shiny app for clicking on points to set labels and dragging 
labels around to position them.

```
# gapminder data set
if(!require(gapminder)) {install.packages("gapminder")}
library(gapminder)
easylabel(gapminder[gapminder$year == 2007, ], x = 'gdpPercap', y = 'lifeExp',
          col = 'continent', shape = 'continent',
          size = 10,
          labs = 'country', 
          zeroline = FALSE)
```

### Bubble chart
`size` can be set to a column in the dataset to create a bubble chart.

```
library(gapminder)
easylabel(gapminder[gapminder$year == 2007, ], x = 'gdpPercap', y = 'lifeExp',
          col = 'continent', labs = 'country', 
          size = 'pop',
          alpha = 0.6,
          zeroline = FALSE)
```

### Volcano plots
Volcano plots can be quickly and easily created from results objects directly 
from DESeq2 and limma.

```
volc1 <- results(DESeq2_dds)
easyVolcano(volc1, fccut = 0)
```
