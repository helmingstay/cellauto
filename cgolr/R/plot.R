## S3 methods

## default levelplot method
## no axes, par.settings from x$plot_data
levelplot.Rcpp_cgolr <- function(x, .at=NULL,
    ...
) {
    if (is.null(.at)) {
        .at <- x$plot_data$at
    }
    ## copy grid data in   
    #x$plot_data$plot.grid$z <- as.vector(x$grid)
    ##
    .tmp <- x$plot_data
    ret <- levelplot(t(x$grid),
        #levelplot(z ~ x*y, plot.grid,
        scales=list(draw=FALSE),
        colorkey=F,
        xlab='', ylab='',
        at = .at,
        #cuts=ncolor-1,
        par.settings=.tmp$lattice.theme,
        useRaster=.tmp$raster,
        ...
    )
    return(ret)
}

plot.Rcpp_cgolr <- function(x, ...) {
    ret <- levelplot(x, ...)
    plot(ret)
    invisible(ret)
}

image.Rcpp_cgolr <- function(x, ...) {
    image(x$grid, ...)
}
