# cgolr: Conway's Game of Life (in R)
# Copyright (C) 2015-2016 Christian Gunning
# code [at] x14n [dot] org

cgolr.setup <- function(
    nrow, ncol, 
    settings=cgolr.set.default(),
    init.grid = c('empty','random','crosshairs')
) {
    if (!is.list(settings)){
        stop('settings must be a named list')
    } else {
        ## otherwise merge with current settings
        settings <- cgolr.settings(settings)
    }
    if ( length(nrow)!=1 || length(ncol)!=1 ) {
        warning("In cgolr: only the first element of nrow / ncol used.")
        nrow <- nrow[1]
        ncol <- ncol[1]
    }
    ret <- new(cgolr, as.integer(nrow), as.integer(ncol))
    ## initialize rules
    with(settings, 
        ret$init_rules(
            born, lives, 
            r.rad, c.rad,
            r.offset, c.offset
        )
    )
    grid.type <- match.arg(init.grid)
    new.grid <- switch(grid.type, 
        empty = matrix(0, nrow=nrow, ncol=ncol),
        random = cgolr.grid.random(ret),
        crosshairs = cgolr.grid.crosshairs(ret)
    )
    ret$grid <- new.grid
    return(ret)
}
