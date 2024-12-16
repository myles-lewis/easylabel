News
=====

# easylabel 0.3.1
###### 16/12/2024
* Added option to save other file types (svg, png, jpeg, tiff) from shiny 
interface.
* Added ability to embed an image of the points as a rastered bitmap (png). This 
helps reduce file size with large numbers of points.
* Added saving of state of labels from shiny and enabled reloading

# easylabel 0.2.8
###### 30/10/2023
* Bugfix for peak detection error when `showOutliers = FALSE` in `easyManhattan()`
* Aesthetic improvements to `easyManhattan()`
* Improved thinning of data points near the x axis in `easyManhattan()` with 
large GWAS
* Faster peak finding algorithm for Manhattan plots

# easylabel 0.2.7
###### 29/08/2023
* Added `qqplot()` for fast plotting of log QQ p-value plots for genomic 
analyses

# easylabel 0.2.6
###### 11/09/2022
* Improve passing of DESeq2 objects into `easyVolcano()`

# easylabel 0.2.5
###### 3/12/2021

* Improve rounded rectangles padding

# easylabel 0.2.4
###### 13/11/2021

* Add option for label text, line and box colours to match colour of each point
* Label text & line colours now also work in plotly
* Enable export of plotly objects
* Fix bugs in MA plot

# easylabel 0.2.2
###### 22/10/2021

* Added `easyManhattan()` function
* Added `plotly_filter` argument to allow large datasets to be labelled
* Legend can be repositioned by setting `legendxy`
* Improved click stability in the shiny app
* Added support for custom tick marks and tick labels
* Allow gridlines for x or y axis alone
* Allow option to use nominal p values alone in `easyVolcano()`

# easylabel 0.1.0
###### 03/10/2021

* This is the initial build of easylabel