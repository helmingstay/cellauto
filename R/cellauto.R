# cellauto: Cellular Automata Explorer (in R)
# Copyright (C) 2015-2016 Christian Gunning
# code [at] x14n [dot] org

cellauto <- setRefClass("cellauto", 
    fields = list(
        ## user accessible, ctor
        rules='list',
        #### list of matrices, including game grid
        mats='list'
        ## update rules
        ## pass in as named list
        #### each element of *_at must be <= (2*radius_row+1)*(2*radius_col+1) -1 (max_neighbor)
        ## 
        settings='list',
        ## read-only?
        counts='list'
    )
)
    
## helper methods
cellauto$methods(
    ## overlay input list and args onto settings list
    ## return invisibly, e.g. works as a getter
    init_settings = function(.settings=NULL, ...) {
        ## either can be absent
        ## user-passed list supercedes
        settings <<- list_append(.settings, settings)
        ## user-passed args supercedes
        settings <<- list_append(list(...), settings)
        list_check(settings, settings$names.settings)
        invisible(settings)
    }
    ## 
    init_rules = function(.rules) {
        list_check(.rules, settings$names_rules)
        have <- names(.rules)
        if (!setequal(have, needed) ) {
            stop(paste0("Missing / extra elements in rules list, expect: ", need, " got: ", have))
        }
        rules <<- .rules
        ## assign to object
        ## max_neighbor
        nmax <- with(rules,
            (2*radius_row+1)*(2*radius_col+1)-1
        )
        ## born_at / lives_at
        b_at <- logical(nmax)
        b_at[rules$born] <- TRUE
        l_at <- logical(nmax)
        l_at[rules$lives] <<- TRUE
        ## finalize
        rules$neighborhood_size <<- nmax
        rules$born_at <<- b_at
        rules$lives_at <<- l_at
    }
    init_grid = function(.grid) {
        fin = list()
        ## dimensions
        nr = nrow(.grid)
        nc = ncol(.grid)
        nsqr <- prod(rn*nc)
        ## create basic objects
        fin$grid <<- .grid
        ## is currently alive
        fin$alive <<- (grid >= 1.0)
        ## number living neighbors
        fin$neighbor <<- matrix(0L, nrow=nr, ncol=nc)
        ## one col per cell, index of neighbor by row
        fin$address <<- matrix(0L, nrow=neighborhood_size, ncol=nsqr)
        mats <<- fin
        ## fill address
        cpp_init_address(mats, rules);
        cpp_update(mats, TRUE, which(fin$alive))
    }
)

cellauto$methods(
    initialize = function(
        .grid, 
        .settings=setting_by_name(.cellauto_defaults$color_list, 'bw')
        ## dots passed to / override rule_by_name
        .rules=rule_by_name(.cellauto_defaults$notable_rules, 'life')
    ) {
        init_settings(.settings)
        ## get by name
        init_rules(.rules)
        init_grid(.grid)
    }
    steps = function(nstep) {
        cpp_steps(nstep, mats);
        counts$nalive <<- sum(mats$alive)
        counts$age <<- counts$age + nstep
    }
)


## prepare data structures for lattice plotting
## by default use theme from settings.R /.cellauto_defaults
cellauto$methods(
    init_plot = function(.raster=TRUE, 
        levelplot_theme=.cellauto_defaults$levelplot_theme
    ) {
        ## use current values of settings
        .curr <- x$settings
        ## make color ramp
        .col <- c(.curr$color.dead, .curr$color.ramp)
        ## ramp, breaks = ncolor -1 - 1 (live col)
        .col <- colorRampPalette(colors=.col, space='Lab')(.curr$ncolor-1)
        ## set "regions" colors for lattice
        .col=c(.col, .curr$color.live)
        .curr$levelplot_theme$regions = .col
        ## 
        ## levelplot at (zlim)
        ## may need to recompute as grid changes
        .curr$at <- with(.curr, 
            seq(from=zlim[1], to=zlim[2], length.out=ncolor+1)
        )
        .curr$raster <- .raster
        ## end within x$plot_data, 
    }
)

## create / return new object
## initialize
cellauto_new <- function(.grid, .settings=NULL) {
    if (is.null(.settings)) {
        .settings <- cellauto_settings(quiet=TRUE)
    } else if (!is.list(.settings)){
        stop('.settings must be NULL, or a named list')
    } else {
        ## otherwise merge with current settings
        .settings <- cellauto_settings(.settings=.settings)
    }
    if ( !is.matrix(.grid) || !is.numeric(.grid)) {
        stop("In cellauto_new: grid must be numeric matrix")
    }
    )
    ## initialize plotting defaults
    init_plot(ret)
    return(ret)
}
