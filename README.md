# rcpp.cgolr
Conway's Game of Life in R

## Examples
* Make a movie from each rule:

```R
require(cgolr)
.ex <- paste0(
    path.package('cgolr'), 
    '/examples/search.R'
); 
source(.ex)
```

* Run webapp:

```R
require(shiny)
.ex <- paste0(
    path.package('cgolr'), 
    '/webapp/'
); 
runApp(.ex, port=8080, launch.browser=T)
```
