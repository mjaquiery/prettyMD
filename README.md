# prettyMD
Make statistics in Rmd files look prettier. Remember to tell your chunks to render results='asis'

## Installation

```r
if(!require(prettyMD)) {
  devtools::install_github('mjaquiery/prettyMD')
  library(prettyMD)
}
```

## Usage

`````rmd
```{r results = 'asis')
  data <- data.frame(x = rnorm(100), y = rnorm(100, 0.2)) # two normal distributions with some overlap
  md.ttest(data$x, data$y, c('*Mean|Control*', '*Mean|Treatment*'), paired = T)
```
`````
