## Settings
## See https://github.com/yihui/animation/blob/master/R/ani.options.R for example
.cgolrEnv <- new.env()

.cgolrEnv$allowed.names <- c(
    'rule_name',
    'born', 'lives',
    'grow', 'decay',
    'r.rad', 'c.rad',
    'r.offset', 'c.offset',
    'ncolor', 'zlim',
    'color.live', 'color.dead',
    'color.ramp'
)


## helper function to combine list 
## and remove duplicates (from under) when they exist
list_append <- function(over, under) {
    ## combine, input args take precendence
    ret <- append(over, under)
    ## remvoe dups - grab the first
    ret <- ret[unique(names(ret))]
    return(ret)
}

cgolr_settings <- function(
    settings = NULL, quiet=FALSE, ...
) {
    lst <- list(...)
    ## if present, append named options to list 
    if (is.list(settings)) {
        lst <- c(settings, lst)
    }
    .curr <- .cgolrEnv$.settings
    ## query 
    .allowed <- paste0("cgolr settings, allowed names: \n", 
        paste(.cgolrEnv$allowed.names, collapse = ', '), '\n')
    ## nothing to set, instead get
    if (!length(lst) && is.null(settings)) {
        if (!quiet) cat(.allowed)
        return(.curr)
    }
    ## basic error checking
    if (is.null(names(lst))) stop("Argument names required")
    if ( !all(names(lst) %in% .cgolrEnv$allowed.names)) {
        stop(paste0("Unrecognized argument. ", .allowed))
    }
    ## finally, set
    .curr[names(lst)] <- lst
    assign('.settings', .curr, envir=.cgolrEnv)
    return(.curr)
}



##############################
## Store defaults in env
##############################

## Default theme: no padding levelplot
## add colors in init.R / init_plot
.cgolrEnv$levelplot_theme  <- list(
    layout.heights = list(
        top.padding = 0,
        main.key.padding = 0,
        key.axis.padding = 0,
        axis.xlab.padding = 0,
        xlab.key.padding = 0,
        key.sub.padding = 0,
        bottom.padding = 0
    ),
    layout.widths = list(
        left.padding = 0,
        key.ylab.padding = 0,
        ylab.axis.padding = 0,
        axis.key.padding = 0,
        right.padding = 0
    )
)

## for more info see 
## https://en.wikipedia.org/wiki/Life-like_cellular_automaton
.cgolrEnv$notable_rules <- within(list(), {
    replicator <- list(
        born=c(1,3,5,7),
        lives=c(1,3,5,7)
    )
    seeds <- list(
        born=c(2),
        ## must be type numeric, even if empty
        lives=numeric()
    )
    rule25_4 <- list(
        born=c(2,5),
        lives=c(4)
    )
    no_death <- list(
        born=c(3),
        lives=c(0:8)
    )
    life <- list(
        born=c(3),
        lives=c(2,3)
    )
    life34 <- list(
        born=c(3,4),
        lives=c(3,4)
    )
    diamoeba <- list(
        born=c(3,5,6,7,8),
        lives=c(5:8)
    )
    rule_2x2 <- list(
        born=c(3,6),
        lives=c(1,2,5)
    )
    highlife <- list(
        born=c(3,6),
        lives=c(2,3)
    )
    day_night <- list(
        born=c(3,6:8),
        lives=c(3,4,6:8)
    )
    morley <- list(
        born=c(3,6,8),
        lives=c(2,4,5)
    )
    anneal <- list(
        born=c(4,6:8),
        lives=c(3,5:8)
    )
})
    

rule_by_name <- function(name=NULL, ...) {
    .rules <- .cgolrEnv$notable_rules
    if (is.null(name)) {
        return(.rules)
    }
    if (!(name %in% names(.rules))) { 
        stop(paste0('Rule not implemented. Allowed names: \n', paste(.rules, collapse=', ')) )
    }
    ## pass rule as list to settings
    .set <- .rules[[name]]
    .set$rule_name <- name
    ## combine, input args take precendence
    .set <- list_append(list(...), .set)
    return(.set)
}


## color_* functions prepare a list
color_bw <- function(...) {
    cols <- within(list(),{
       color.live <- 'white'
       color.dead <- 'black'
       ## first = old, last = newly born
       color.ramp <- c('darkgrey', 'lightgrey')
    })
    ## precedence to input args
    ret <- list_append(list(...), cols)
    return(ret)
}

color_reds <- function(...) {
    cols <- within(list(), {
       color.live <- 'red'
       color.dead <- 'grey10'
       ## first = old, last = newly born
       color.ramp <- c('darkslateblue', 'firebrick')
    })
    ## precedence to input args
    ret <- list_append(list(...), cols)
    return(ret)
}

color_blues <- function(...) {
    cols <- within(list(), {
        color.live <- 'lightslategrey'
        color.dead <- '#060606'
        ## first = old, last = newly born
        color.ramp <- c('darkslateblue','cornflowerblue')
    })
    ## precedence to input args
    ret <- list_append(list(...), cols)
    return(ret)
}

color_rgb <- function(...) {
    cols <- within(list(), {
        color.live <- 'red'
        color.dead <- 'grey10'
        ## first = old, last = newly born
        color.ramp <- c('darkslateblue','green')
    })
    ## precedence to input args
    ret <- list_append(list(...), cols)
    return(ret)
}

## Default game of life
## User supplied args overrides
cgolr_settings_default <- function(...) {
    .default <- colors_bw()
    .default <- within(.default, {
       grow <- 1 
       decay <- 1 
       born <- 3 
       lives <- c(2,3) 
       r.rad <- 1 
       c.rad <- 1 
       r.offset <- 0 
       c.offset <- 0 
       ncolor <- 100
       zlim <- c(0,1)
    })
    ## over-ride list w/user-supplied
    ret <- cgolr_settings(settings = .default, ...)
    return(ret)
}

## set initial default settings
cgolr_settings(settings=cgolr_settings_default())
