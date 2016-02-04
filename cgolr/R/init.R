## init_* functions take an object,
## modify it in-place, 
## and return invisible()

## fill grid  ran
init_grid_random <- function(x, prob=0.5) {
    grid <- rbinom(prod(x$dim), 1, prob)
    dim(grid) <- x$dim
    x$grid <- grid
    invisible()
}

## fill grid  ran
init_grid_blank <- function(x) {
    x$grid[] <- 0
    invisible()
}

## h.bars = length 2
## v.bar = length 1
## enforce symmetrix around center?
init_grid_crosshairs <- function(x, 
    h.bars=c(4,4/3),
    v.bar=c(3),
    ## cut off this many pixels
    ends = 10
) {
    .dim <- x$dim
    ## index of rows/cols to place horiz / vert bars
    .h.bar <- .dim[1] %/% h.bars
    .v.bar <- .dim[2] %/% v.bar
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
    grid <- matrix(0, nrow=.dim[1], ncol=.dim[2])
    grid[.h.bar,(1+ends):(.dim[2]-ends)] <- 1
    grid[(1+ends):(.dim[1]-ends), .v.bar] <- 1
    grid[.diag[,c(1,2)]] <- 1
    grid[.diag[,c(1,3)]] <- 1
    x$grid <- grid
    invisible()
}

## prepare data structures for lattice plotting
## add to x$plot_data
## return nothing
init_plot <- function(x, 
    ncolor=100, 
    color.live = 'lightslategrey',
    color.dead = '#060606',
    ## first = old, last = newly born
    color.ramp = c('darkslateblue','cornflowerblue'),
    zlim=c(0,1),
    .raster=TRUE
) {
    ## make color ramp
    .color <- c(color.dead,color.ramp)
    ## ramp, breaks = ncolor -1 - 1 (live col)
    .color <- colorRampPalette(colors=.color, space='Lab')(.ncolor-2)
    ## set "regions" colors for lattice
    .color=c(.color, color.live)
    ## 
    ## data needed for plotting
    x$plot_data <- within(x$plot_data, {
        ## levelplot at (zlim)
        ## may need to recompute as grid changes
        at <- seq(from=zlim[1], to=zlim[2], length.out=ncolor)
        ncolor <- length(.color)
        raster <- .raster
        ## no padding levelplot
        ## and colors
        lattice.theme <- list(
            regions=list(
                col=.color
            ),
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
    ## end within x$plot_data, 
    })
    invisible()
}
