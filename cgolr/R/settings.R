## Settings
## See https://github.com/yihui/animation/blob/master/R/ani.options.R for example
.cgolrEnv <- new.env()

.cgolrEnv$allowed.names <- c(
    'born', 'lives',
    'grow', 'decay',
    'r.rad', 'c.rad',
    'r.offset', 'c.offset'
)

cgolr_settings <- function(settings = NULL, ...) {
    lst <- list(...)
    ## if present, append named options to list 
    if (is.list(settings)) {
        lst <- c(settings, lst)
    }
    .curr <- .cgolrEnv$.settings
    ## query 
    .allowed <- paste0("cgolr settings, allowed names: \n", 
        paste(.cgolrEnv$allowed.names, collapse = ', '))
    ## nothing to set, instead get
    if (!length(lst)|| is.null(settings)) {
        cat(.allowed)
        return(.curr)
    }
    
    ## basic error checking
    if (is.null(names(lst))) stop("Argument names required")
    if ( !all(names(lst) %in% .cgolrEnv$allowed.names)) {
        stop(paste0("Unrecognized argument. ", .allowed))
    }
    ## finally, set
    .curr[names(lst)] <- lst
    .cgolrEnv$.settings <- .curr
    return(.curr)
}

## Default game of life
## User supplied args overrides
cgolr_settings_default <- function(...) {
    .default <- within(list(), {
       grow <- 1 
       decay <- 1 
       born <- 3 
       lives <- c(2,3) 
       r.rad <- 1 
       c.rad <- 1 
       r.offset <- 0 
       c.offset <- 0 
    })
    ## over-ride list w/user-supplied
    ret <- cgolr_settings(settings = .default, ...)
    return(ret)
}

