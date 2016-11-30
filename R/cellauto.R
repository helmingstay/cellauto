# cellauto: Cellular Automata Explorer (in R)
# Copyright (C) 2015-2016 Christian Gunning
# code [at] x14n [dot] org

cellauto <- setRefClass("cellauto", 
    fields = list(
        ## user accessible, ctor
        rules='list',
        #### input game grid
        grid='matrix',
        ## update rules
        ## pass in as named list
        #### each element of *_at must be <= (2*radius_row+1)*(2*radius_col+1) -1 (max_neighbor)
        born_at='bool',
        lives_at='bool',
        radius_row='integer',
        radius_col='integer',
        grow='numeric',
        decay='numeric',
        ## 
        settings='list',
        ## read-only?
        age='integer',
        counts='integer',
        alive='matrix',
        neighbor='matrix',
        neighbor_size='integer',
        address = 'matrix'
    )
)
    
## helper methods
cellauto$methods(
    ## overlay input list and args onto settings list
    ## return invisibly, e.g. works as a getter
    init_settings = function(_settings=NULL, ...) {
        ## either can be absent
        ## user-passed list supercedes
        settings <<- list_append(_settings, settings)
        ## user-passed args supercedes
        settings <<- list_append(list(...), settings)
        list_check(settings, settings$names.settings)
        invisible(settings)
    }
    ## 
    init_rules = function(_rules) {
        list_check(_rules, settings$names_rules)
        have <- names(_rules)
        if (!setequal(have, needed) ) {
            stop(paste0("Missing / extra elements in rules list, expect: ", need, " got: ", have))
        }
        rules <<- _rules
        ## assign to object
        grow <<- rules$grow
        decay <<- rules$decay
        radius_row <<- rules$radius_row
        radius_col <<- rules$radius_col
        ## max_neighbor
        nmax <- (2*radius_row+1)*(2*radius_col+1)-1)
        ## born_at / lives_at
        b_at <- logical(nmax)
        b_at[rules$born] <- TRUE
        l_at <- logical(nmax)
        l_at[rules$lives] <<- TRUE
        ## finalize
        neighborhood_size <<- nmax
        born_at <<- b_at
        lives_at <<- l_at
    }
    init_grid = function(_grid) {
        ## dimensions
        nr = nrow(_grid)
        nc = ncol(_grid)
        nsqr <- prod(rn*nc)
        ## create basic objects
        grid <<- _grid
        ## is currently alive
        alive <<- (grid >= 1.0)
        ## number living neighbors
        neighbor <<- matrix(0L, nrow=nr, ncol=nc)
        ## one col per cell, index of neighbor by row
        address <<- matrix(0L, nrow=neighborhood_size, ncol=nsqr)
        ## fill address
        cpp_init_address(address, radius_row, radius_col, nr);
        cpp_update(TRUE, grow, which(alive), 
            neighbor, address, grid, alive
        )
    }
)

cellauto$methods(
    initialize = function(
        _grid, 
        _settings=setting_by_name(.cellauto_defaults$color_list, 'bw')
        ## dots passed to / override rule_by_name
        _rules=rule_by_name(.cellauto_defaults$notable_rules, 'life')
    ) {
        init_settings(_settings)
        ## get by name
        init_rules(_rules)
        init_grid(_grid)
    }
    steps = function(nstep) {
        cpp_steps(nstep, mats);
        nalive = sum(alive) 
        age = age + nstep
    }
)


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
