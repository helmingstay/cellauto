## use with animation
## take nsteps, plotting every plot.at steps
cgolr_movie <- function(x, 
    nstep, plot.at = 1,
    quiet=FALSE,
    ...
){
    .nframe <- (nstep %/%plot.at)
    for (ii in 1:.nframe) {
        ## report
        if (!quiet) cat(sprintf('%02d%%\r', as.integer((100*ii/.nframe))))
        ## plot
        plot(levelplot(x, ...))
        x$steps(plot.at)
    }
    invisible()
}
