# plotTools package

This package includes a set of useful functions to plot the figures about analyzing multi-omics data.


## Installation
Install devtools

```
if(!require("devtools", quietly = T))
  install.packages("devtools")
```

Install plotTools
```
devtools::install_github("paodan/plotTools")

```


## Project multidimensional samples onto 2D space
* plotMetaOn2D
* plotMetaOnPCA
* plot2D

## Plot volcano plot for differential analysis
* plotVolcano

## Plot manhattan plot for GWAS and DNA methylation results
* plotManhattan

## Compare values of different groups using boxplot/violin plot
* compareBoxplot
* plotExpViolin

## Generate animation using image files
* makeAnimation

If you have an issue to install the package magick, make sure you have installed ImageMagick STL:

On Ubuntu:
`sudo apt-get install libmagick++-dev`

On Fedora or CentOS/RHEL:
`sudo yum install ImageMagick-c++-devel`

On macOS:
`brew install imagemagick@6`


## Plot screeplot for PCA results
* screeplot2

## Manipulate aes in ggplot object
* aes_add

## Plot pathway/GO terms
* dotplot_text

## Plot heatmap with repelled row labels
* add_flag

## Plot error bars
* geom_errorbar2


