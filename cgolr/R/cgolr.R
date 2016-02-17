# cgolr: Conway's Game of Life (in R)
# Copyright (C) 2015-2016 Christian Gunning
# code [at] x14n [dot] org

## create / return new cgolr object
## initialize
cgolr_new <- function(
    nrow, ncol, 
    settings=cgolr_settings(quiet=TRUE),
    init.grid = c('blank','random','crosshairs')
) {
    if (!is.list(settings)){
        stop('settings must be a named list')
    } else {
        ## otherwise merge with current settings
        settings <- cgolr_settings(settings)
    }
    if ( length(nrow)!=1 || length(ncol)!=1 ) {
        warning("In cgolr: only the first element of nrow / ncol used.")
        nrow <- nrow[1]
        ncol <- ncol[1]
    }
    ret <- new(cgolr, as.integer(nrow), as.integer(ncol))
    ## fields from settings
    ret$grow <- settings$grow
    ret$decay <- settings$decay
    ## initialize rules
    ## store initials
    ret$settings <- settings
    ##
    with(settings,
        ret$init_rules(
            born, lives, 
            r.rad, c.rad,
            r.offset, c.offset
        )
    )
    ret$user_data$init_grid <- init.grid
    ##
    grid.type <- match.arg(init.grid)
    switch(grid.type, 
        blank = init_grid_blank(ret),
        random = init_grid_random(ret),
        crosshairs = init_grid_crosshairs(ret)
    )
    ## initialize plotting defaults
    init_plot(ret)
    return(ret)
}
