## S3 methods

## default levelplot method
## no axes, par.settings from x$plot_data
levelplot.cellauto <- function(x, .at=NULL, 
    .draw=FALSE, .key=FALSE,
    ...
) {
    .curr <- x$settings
    if (is.null(.at)) {
        .at <- .curr$at
    }
    ## copy grid data in   
    #x$plot_data$plot.grid$z <- as.vector(x$grid)
    ##
    ret <- levelplot(t(x$mats$grid),
        #levelplot(z ~ x*y, plot.grid,
        scales=list(draw=.draw),
        colorkey=.key,
        xlab='', ylab='',
        at = .at,
        #cuts=ncolor-1,
        par.settings=.curr$theme_levelplot,
        useRaster=.curr$raster,
        ...
    )
    return(ret)
}

plot.cellauto <- function(x, ...) {
    ret <- levelplot(x, ...)
    plot(ret)
    invisible(ret)
}

image.cellauto <- function(x, ...) {
    image(x$mat$grid, ...)
}
