# Examples

### Make a movie from each rule:
```R
library(cellauto)
.ex <- paste0(
    path.package('cellauto'), 
    '/examples/search.R'
); 
source(.ex)
```

### Run webapp:

```R
library(shiny)
.ex <- paste0(
    path.package('cellauto'), 
    '/webapp/'
); 
runApp(.ex, port=8080, launch.browser=T)
```
