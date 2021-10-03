### easylabel

Install from CRAN
```
install_package("easylabel")
library(easylabel)
```

Install from Github
```
devtools::install_github("myles-lewis/easylabel")
```

Install from local repo
```
setwd("/users/.../")
devtools::install("easylabel")
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

Simple instructions:
* Hover over and click on/off genes which you want to label.
* When you have selected all your chosen genes, then drag gene names to move
  label positions.
* Click the save button to export a PDF in base graphics.

To export an SVG from plotly: 
* You can move the legend as well.
* Switch to SVG when finalised (only do this at last moment as otherwise
  editing is very slow).
* Press camera button in modebar to save image as SVG.

# Examples:
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

# Bubble chart
`size` can be set to a column in the dataset to create a bubble chart.

```
library(gapminder)
easylabel(gapminder[gapminder$year == 2007, ], x = 'gdpPercap', y = 'lifeExp',
          col = 'continent', labs = 'country', 
          size = 'pop',
          alpha = 0.6,
          zeroline = FALSE)
```

# Volcano plots
Volcano plots can be quickly and easily created from results objects directly 
from DESeq2 and limma.

```
volc1 <- results(DESeq2_dds)
easyVolcano(volc1, fccut = 0)
```
