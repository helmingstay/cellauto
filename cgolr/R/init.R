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
        col1+ends <= .dim[2] & 
        col2+ends <= .dim[2] 
    ))
    ## final obj
    grid <- matrix(0, nrow=.dim[1], ncol=.dim[2])
    grid[.h.bar,(1+ends):(.dim[2]-ends)] <- 1
    grid[(1+ends):(.dim[1]-ends), .v.bar] <- 1
    ## diagonals, use matrix indexing 
    ## cols forward and reverse
    grid[.diag[,c(1,2)]] <- 1
    grid[.diag[,c(1,3)]] <- 1
    x$grid <- grid
    invisible()
}

## prepare data structures for lattice plotting
## add to x$plot_data
## return nothing
## by default use theme from settings.R /.cgolrEnv
init_plot <- function(x, .raster=TRUE, 
    levelplot_theme=.cgolrEnv$levelplot_theme
) {
    ## use current values of settings
    .pars <- x$settings
    ## make color ramp
    .color <- c(.pars$color.dead, .pars$color.ramp)
    ## ramp, breaks = ncolor -1 - 1 (live col)
    .color <- colorRampPalette(colors=.color, space='Lab')(.pars$ncolor-1)
    ## set "regions" colors for lattice
    .color=c(.color, .pars$color.live)
    ## 
    ## data needed for plotting
    x$plot_data$ncolor <- .pars$ncolor
    x$plot_data <- within(x$plot_data, {
        ## levelplot at (zlim)
        ## may need to recompute as grid changes
        at <- seq(from=.pars$zlim[1], to=.pars$zlim[2], length.out=ncolor+1)
        raster <- .raster
        theme <- levelplot_theme
        theme$regions <- list(col=.color)
    ## end within x$plot_data, 
    })
    invisible()
}
