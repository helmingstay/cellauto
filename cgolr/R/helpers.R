## fill grid  ran
cgolr.grid.random <- function(x, prob=0.5) {
    ret <- rbinom(prod(x$dim), 1, prob)
    dim(ret) <- x$dim
    return(ret)
}

## h.bars = length 2
## v.bar = length 1
## enforce symmetrix around center?
cgolr.grid.crosshairs <- function(x, 
    h.bars=c(4,4/3),
    v.bar=c(3),
    ## cut off this many pixels
    ends = 10
) {
    .dim <- x$grid
    ## index of rows/cols to place horiz / vert bars
    .h.bar <- .dim[1] %/% h.bars
    .v.bar <- .dim[2] %/% v.bars
    ##  ensure box has L/R symmetry 
    .v.bar <- c(.v.bar, .v.bar+diff(.h.bar))
    ## crete line between box corners
    .diag <- (ends:(min(.dim)-ends))
    .offset <- min(.v.bar) - min(.h.bar)
    .diag <- cbind(row=.diag, 
        col1=(.diag)+.offset, 
        col2=rev(.diag)+.offset
    )
    ## final obj
    ret <- matrix(0, nrow=.dim[1], ncol=.dim[2])
    ret[.h.bar,(1+ends):(.dim[2]-ends)] <- 1
    ret[(1+ends):(.dim[1]-ends), .v.bar] <- 1
    ret[.diag[,c(1,2)]] <- 1
    ret[.diag[,c(1,3)]] <- 1
    return(ret)
}
