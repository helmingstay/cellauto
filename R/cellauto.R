# cellauto: Cellular Automata Explorer (in R)
# Copyright (C) 2015-2016 Christian Gunning
# code [at] x14n [dot] org

cellauto <- setRefClass("cellauto", 
    fields = list(
        ## user accessible, ctor
        rules='list',
        #### list of matrices, including game grid
        mats='list',
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
    init_settings = function(.settings=list(), ...) {
        ## if not absent then user-passed list supercedes
        .curr <- list_append(.settings, settings)
        ## user-passed args supercede
        .curr <- list_append(list(...), .curr)
        ## check names against standard 
        list_check(.curr, .cellauto_defaults$names.settings)
        ## assign at end
        settings <<- .curr
        invisible(settings)
    }
)
cellauto$methods(
    ## 
    init_rules = function(.rules) {
        list_check(.rules, .cellauto_defaults$names.rules)
        ## assign to object
        ## max_neighbor
        nmax <- with(.rules,
            (2*radius_row+1)*(2*radius_col+1)-1
        )
        ## born_at / lives_at
        ## 0-based indexes
        b_at <- logical(nmax)
        b_at[.rules$born-1] <- TRUE
        l_at <- logical(nmax)
        l_at[.rules$lives-1] <- TRUE
        ## finalize
        .rules$neighborhood_size <- nmax
        .rules$born_at <- b_at
        .rules$lives_at <- l_at
        rules <<- .rules
    }
)
cellauto$methods(
    init_grid = function(.grid) {
        fin = list()
        ## dimensions
        nr = nrow(.grid)
        nc = ncol(.grid)
        nsqr <- prod(nr*nc)
        ## create basic objects
        fin$grid <- .grid
        ## is currently alive
        fin$alive <- (.grid >= 1.0)
        ## number living neighbors
        fin$neighbor <- matrix(0L, nrow=nr, ncol=nc)
        fin$neighbor0 <- matrix(0L, nrow=nr, ncol=nc)
        ## one col per cell, index of neighbor by row
        fin$address <- matrix(0L, nrow=rules$neighborhood_size, ncol=nsqr)
        mats <<- fin
        ## fill address
        cpp_init_address(mats, rules$radius_row, rules$radius_col);
        cpp_update(which(fin$alive), mats)
    }
)

cellauto$methods(
    initialize = function(
        .grid, 
        .settings=setting_by_name(.cellauto_defaults$color_list, 'bw'),
        ## dots passed to / override rule_by_name
        .rules=rule_by_name(.cellauto_defaults$notable_rules, 'life'),
        .raster=TRUE
    ) {
        init_settings(.settings)
        ## get by name
        init_rules(.rules)
        init_grid(.grid)
        init_plot(.raster)
    }
)
cellauto$methods(
    steps = function(nstep) {
        cpp_steps(
            nstep, 
            rules$born_at, rules$lives_at, 
            settings$grow, settings$decay, 
            mats, counts
        );
        counts$nalive <<- sum(mats$alive)
        counts$age <<- counts$age + nstep
    }
)

## prepare data structures for lattice plotting
## by default use theme from settings.R /.cellauto_defaults
cellauto$methods(
    init_plot = function(.raster=TRUE) {
        ## use current values of settings
        .curr <- settings
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
