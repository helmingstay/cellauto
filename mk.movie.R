## plotting function
mk.movie <- function(.nstep, obj, .silent=T, 
    .compare.at=1e1, 
    .noise.start=1e9,
    .noise.at=1e2, 
    ## on average, 1 per step
    .noise.prop=.noise.at/prod(dim(obj$grid))
) {
    ## dimension and total number of elements
    .dim <- dim(obj$grid)
    .n.tot <- prod(.dim)
    ## For levelplot
    .plot.grid <- expand.grid(y=1:.dim[1], x=1:.dim[2], z=1)
    .plot.grid$z <- as.vector(obj$grid)
    .plot <- levelplot(z ~ x*y, .plot.grid, 
        scales=list(draw=F), xlab='', ylab='',
        colorkey=F, useRaster=T, at=.at,
        par.settings=.theme.novpadding 
    )
    ## comparison for end of life
    .comp.grid <- (obj$grid >= 1)
    ## 
    cat(sprintf('## R Tempdir: %s\n', tempdir()))
    for (ii in 1:(.nstep-1)) {
        ## report
        if (!.silent) cat(sprintf('## Processing:\t%2.0f%%\t\tFrame %d of %d\r',(100*ii)/.nstep, ii, .nstep))
        ## plot, then step
        plot(.plot)
        obj$step()
        ## stop if living cells are identical
        ## between now and last comarison grid
        if (!(ii%%.compare.at)) {
            .comp.new <- (obj$grid >= 1)
            if (identical(.comp.grid, .comp.new)) {
                cat(sprintf('## No life change at gen %d\n', ii)) 
                return()
            }
            .comp.grid <- .comp.new
        }
        ## inject life-noise at given period
        if (ii >= .noise.start && !(ii%%.noise.at)) {
            ## integer number of 
            ## noise pixels per noise event
            #.n.noise <- round(.n.tot * .noise.prop)
            .n.noise <- rbinom(1, .n.tot, .noise.prop)
            ## indices of noise
            .noise <- sample.int(.n.tot, size=.n.noise)
            obj$grid[.noise] <- 1
        }
        #.nc <- 4
        #obj$grid[,1:.nc] <- rep(c(0,1,1), length.out=.dim[1]*.nc)
        .plot$panel.args.common$z <- as.vector(obj$grid)
    }
}
