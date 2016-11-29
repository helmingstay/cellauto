# cellauto: Cellular Automata Explorer (in R)
# Copyright (C) 2015-2016 Christian Gunning
# code [at] x14n [dot] org

## initialize rules based on settings
init_rules <- function(x) {
    .curr <- x$settings
    x$init_rules(
        .curr$born, .curr$lives, 
        .curr$r.rad, .curr$c.rad,
        .curr$r.offset, .curr$c.offset
    )
}

## create / return new object
## initialize
cellauto_new <- function(
    nrow, ncol, 
    .settings=NULL,
    init.grid = c('blank','random','crosshairs')
) {
    if (is.null(.settings)) {
        .settings <- cellauto_settings(quiet=TRUE)
    } else if (!is.list(.settings)){
        stop('.settings must be NULL, or a named list')
    } else {
        ## otherwise merge with current settings
        .settings <- cellauto_settings(.settings=.settings)
    }
    if ( length(nrow)!=1 || length(ncol)!=1 ) {
        warning("In cellauto_new: only the first element of nrow / ncol used.")
        nrow <- nrow[1]
        ncol <- ncol[1]
    }
    ret <- new(cellauto, as.integer(nrow), as.integer(ncol))
    ## fields from settings
    ret$grow <- .settings$grow
    ret$decay <- .settings$decay
    ## initialize rules
    ## store initials
    ret$settings <- .settings
    ##
    init_rules(ret)

    ret$user_data$init.grid <- init.grid
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
