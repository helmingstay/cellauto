## fill grid  ran
init_grid_random <- function(.nrow, .ncol, prob=0.5) {
    ret <- matrix(rbinom(.nrow*.ncol, 1, prob), nrow=.nrow)
    ret
}

## fill grid  ran
init_grid_blank <- function(.nrow, .ncol) {
    matrix(0, .nrow, .ncol)
}

## h.bars = length 2
## v.bar = length 1
## enforce symmetrix around center?
init_grid_crosshairs <- function(.nrow, .ncol, 
    h.bars=c(4,4/3),
    v.bar=c(3),
    ## cut off this many pixels
    ends = 10
) {
    .dim.min = min(c(.nrow, .ncol))
    ## index of rows/cols to place horiz / vert bars
    .h.bar <- .nrow %/% h.bars
    .v.bar <- .ncol %/% v.bar
    ##  ensure box has L/R symmetry 
    .v.bar <- c(.v.bar, .v.bar+diff(.h.bar))
    ## crete line between box corners
    .diag <- (ends:(.dim.min)-ends))
    ## draw through center of canvas
    .offset <- min(.v.bar) - min(.h.bar)
    ## 
    .diag <- as.data.frame(cbind(row=.diag, 
        col1=(.diag)+.offset, 
        col2=rev(.diag)+.offset
    ))
    ## remove rows that run off
    ## subset as df, convert back to matrix for indexing
    .diag <- as.matrix(subset(.diag, 
        col1+ends <= .ncol & 
        col2+ends <= .ncol
    ))
    ## final obj
    ret <- matrix(0, nrow=.nrow, ncol=.ncol)
    ret[.h.bar,(1+ends):(.ncol-ends)] <- 1
    ret[(1+ends):(.nrow-ends), .v.bar] <- 1
    ## diagonals, use matrix indexing 
    ## cols forward and reverse
    ret[.diag[,c(1,2)]] <- 1
    ret[.diag[,c(1,3)]] <- 1
    ret 
}


