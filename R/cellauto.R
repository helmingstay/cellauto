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
        b_at <- logical(nmax)
        b_at[.rules$born] <- TRUE
        l_at <- logical(nmax)
        l_at[.rules$lives] <- TRUE
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
        ## number living neighbors
        fin$alive <- matrix(FALSE, nrow=nr, ncol=nc)
        fin$neighbor <- matrix(0L, nrow=nr, ncol=nc)
        fin$neighbor0 <- matrix(0L, nrow=nr, ncol=nc)
        ## one col per cell, index of neighbor by row
        fin$address <- matrix(0L, nrow=rules$neighborhood_size, ncol=nsqr)
        mats <<- fin
        ## fill address
        cpp_init_address(mats, rules$radius_row, rules$radius_col);
        ## set neighbor counts, alive
        .updates <- which(.grid >= 1.0)-1
        cpp_update(.updates, mats)
        ##
        counts <<- list(age=0, nbirth=0, ndeath=0)
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
    step = function() {
        cpp_step(
            rules$born_at, rules$lives_at, 
            settings$grow, settings$decay, 
            mats, counts
        );
        counts$nalive <<- sum(mats$alive)
        counts$age <<- counts$age + 1
    },
    steps = function(nstep) {
        while (nstep >0 ) {
            step()
            nstep = nstep -1
        }
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
        .curr$theme = list(regions=list(col=.col))
        ## 
        ## levelplot at (zlim)
        ## may need to recompute as grid changes
        .curr$at <- with(.curr, 
            seq(from=zlim[1], to=zlim[2], length.out=ncolor+1)
        )
        .curr$raster <- .raster
        settings <<- .curr
        ## end within x$plot_data, 
    }
)
