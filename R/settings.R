## Settings
## See https://github.com/yihui/animation/blob/master/R/ani.options.R for example

## helper function to combine list 
## and remove duplicates (from under) when they exist
list_append <- function(over, under) {
    ## combine, input args take precendence
    ret <- append(over, under)
    ## remvoe dups - grab the first
    ret <- ret[unique(names(ret))]
    return(ret)
}

list_check <- function(lst, allowed) {
    have <- names(lst)
    ## basic error checking
    if (is.null(have) stop("List names required.")
    bad <- setdiff(have, allowed)
    if ( length(bad) >0) {
        stop(paste0("Unrecognized list element: ", bad, collapse=', '))
    }
}

rule_by_name <- function(rulelist, name, 
    radius_row=1, radius_col=1, ...
) {
    allowed <- names(rulelist)
    if (!(name %in% allowed)) { 
        stop(paste0('Rule not found. Allowed names: \n', paste(allowed, collapse=', ')) )
    }
    ## pass rule as list 
    ret <- rulelist[[name]]
    ret$rule_name <- name
    ## use sensible defaults 
    ret$radius_row <- radius_row
    ret$radius_col <- radius_col
    ## combine, input args take precendence
    ret <- list_append(list(...), ret)
    return(ret)
}

## Default game of life
## User supplied args overrides
setting_by_name <- function(color_list, name, ...) {
    ret <- color_list[[name]]
    ret <- within(ret, {
        grow <- 0 
        decay <- 1 
        ncolor <- 100
        zlim <- c(0,1)
    })
    ## over-ride list w/user-supplied
    ret <- cellauto_settings(.settings = ret, ...)
    return(ret)
}

##############################
## Store defaults in env
##############################

.cellauto_defaults <- within(list(), {
    ## allowed names
    names.rules <- c(
        # 'offset_row', 'offset_col',
        'rule_name',
        'born', 'lives',
        'radius_row', 'radius_col'
    )
    names.settings <- c(
        'names.settings', 'names.rules',
        'grow', 'decay',
        'ncolor', 'zlim',
        'color.live', 'color.dead',
        'color.ramp'
    )

    ## Default theme: no padding levelplot
    levelplot_theme  <- list(
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
    ## see https://en.wikipedia.org/wiki/Life-like_cellular_automaton
    notable_rules <- within(list(), {
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
    color_list <- within(list(), {
        bw <- within(list(),{
           color.live <- 'white'
           color.dead <- 'black'
           ## first = old, last = newly born
           color.ramp <- c('darkgrey', 'lightgrey')
        })
        reds <- within(list(), {
           color.live <- 'red'
           color.dead <- 'grey10'
           ## first = old, last = newly born
           color.ramp <- c('darkslateblue', 'firebrick')
        })
        blues <- within(list(), {
            color.live <- 'lightslategrey'
            color.dead <- '#060606'
            ## first = old, last = newly born
            color.ramp <- c('darkslateblue','cornflowerblue')
        })
        rgb <- within(list(), {
            color.live <- 'red'
            color.dead <- 'grey10'
            ## first = old, last = newly born
            color.ramp <- c('darkslateblue','green')
        })
    })
})
.cellauto_defaults <- as.environment(.cellauto_defaults) 
