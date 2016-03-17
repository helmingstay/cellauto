## use with animation
## take nsteps, plotting every plot.at steps
movie_basic <- function(x, 
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


## create an audio sample
## (to add audio to movie)
movie_tone <- function(freq, interval=1/10,
    rate=44.1e3, nedge = 50
) {
    ## use linear fade for zero-crossing
    edge <- (0:(nedge-1))/nedge
    ## total samples
    nsamp <- rate*interval
    ## volume envelope
    env <- c(edge, rep(1, nsamp-2*nedge),rev(edge))
    ret <- synth2(ifreq=rep(freq, nsamp), env=env,
        f=rate, output='audioSample'
    )
    return(ret)
}

## use movie_tone to generage soundtrack
## from results of helper_measure
movie_bleep <- function(
    .df, .means, prev, interval=1/10,
    .bounds = c(40, 500), ...
) {
    .freqs <- .bounds[1]:.bounds[2]
    ## high sd = flat freq weights
    .prob.col <- 1/(.freqs^(1/(5*.df$sdcol)))
    .prob.row <- 1/(.freqs^(1/(5*.df$sdrow)))
    #.prob.col <- 1/(.freqs^(1/(.df$sdcol/.means$col)))
    #.prob.row <- 1/(.freqs^(1/(.df$sdrow/.means$row)))
    .tone.row <- movie_tone(sample(.freqs, 1, prob=.prob.row), interval)
    .tone.col <- movie_tone(sample(.freqs, 1, prob=.prob.col), interval)
    .fn.row <- sprintf('row.%05d.wav', .df$frame)
    .fn.col <- sprintf('col.%05d.wav', .df$frame)
    save.wave(.tone.col, .fn.col)
    save.wave(.tone.row, .fn.row)
}


## plot with time-jumps
## sound not currently working
movie_annot <- function(
    obj, .nstep = 150, 
    ## frames of pre-roll
    .npreamble=30,
    compare_at=10, compare_ngen=20,
    .sound=F
) {
    .rule <- obj$settings$rule_name
    ## test for no change
    .comp.grid <- helper_compare_grid(obj, init=T)
    ## number of total vid frames 
    ## stats nrows should equal
    .frame = 1
    ## fill data.frame to put grid statistics in
    stats <- as.data.frame(t(helper_measure(obj)))
    stats[2:(.npreamble+.nstep),] <- NA
    ## preamble - roll tape before advancing model
    for(ii in 1:.npreamble) {
        stats[.frame,] <- helper_measure(obj)
        .frame <- .frame + 1
        plot(levelplot(obj))
        helper_overlay_text('Rule: ', .rule, .y=0.15)
    }
    for (jj in 1:.nstep) {
        ## plot, add age, step 
        plot(levelplot(obj))
        helper_overlay_text('Rule: ', .rule, .y=0.15)
        helper_overlay_text('Age: ', obj$age, .y=0.115)
        helper_overlay_text('Alive: ', sprintf('%2.3f%%', obj$nalive / prod(obj$dim)), .y=0.08)
        helper_overlay_text('Born: ', obj$nbirth, .y=0.045)
        helper_overlay_text('Died: ', obj$ndeath, .y=0.01)
        cat(sprintf('## %2d%%\r', round(100*(jj/.nstep))))
        obj$step()
        stats[.frame,] <- helper_measure(obj)
        .frame <- .frame + 1
        ## stop if living cells are identical
        ## between now and last comarison grid
        if (!(jj%%compare_at)) {
            ## has changed in prev .ngens
            .comp.grid <- helper_compare_grid(
                obj, .comp.grid, compare_ngen
            )
            ## null == no change
            if (is.null(.comp.grid)) {
                cat(sprintf('## No life change at gen %d\n', ii)) 
                return()
            }
        }
    }
    if (.sound) {
        stats$frame <- 1:nrow(stats)
        ## normalize
        .means <- with(stats, data.frame(
            col=mean(sdcol), 
            row=mean(sdrow) 
        ))
        .cwd <- setwd(tempdir())
        .tmp.stats <- subset(stats, 0==(frame%%2))
            dlply(.tmp.stats, 'frame', function(.df) {
                bleep(.df, .means, interval=1/5)
            })
        .cwd <- setwd(tempdir())
        print(stats)
    }
}


## a more complex movie,
## compare for death/cyclic behavior
## inject noise
movie_noise <- function(.nstep, obj, .silent=T, 
    ## test for no changes / cyclic over ngen?
    compare_at=1e1, compare_ngen=1,
    .step.size=1,
    .noise.start=1e9,
    .noise.at=1e2, 
    ## on average, 1 per step
    .noise.prop=.noise.at/prod(dim(obj$grid))
) {
    ## dimension and total number of elements
    .dim <- dim(obj$grid)
    .n.tot <- prod(.dim)
    ## initialize comparison for end of life
    ## has changed state in prev .
    .comp.grid <- helper_compare_grid(obj, init=T)
    ## 
    for (ii in 1:(.nstep-1)) {
        ## report
        if (!.silent) cat(sprintf('## Processing:\t%2.0f%%\t\tFrame %d of %d\r',(100*ii)/.nstep, ii, .nstep))
        ## plot, then step
        plot(.plot)
        obj$steps(.step.size)
        ## stop if living cells are identical
        ## between now and last comarison grid
        if (!(ii%%compare_at)) {
            ## has changed in prev .ngens
            .comp.grid <- helper_compare_grid(
                obj, .comp.grid, compare_ngen
            )
            ## null == no change
            if (is.null(.comp.grid)) {
                cat(sprintf('## No life change at gen %d\n', ii)) 
                return()
            }
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
    }
}

movie_wrapper <- function(
    movie.fun, .video.name,
    ## video quality, 0 = lossless, higher = lower quial
    crf=2,
    .other.opts='-hide_banner -preset slow -c:v libx264  -pix_fmt yuv420p',
    ## pass ... to movie.fun
    cleanup=T, ...
){
    not.used <- saveVideo(
        movie.fun(...),
        video.name=.video.name,
        other.opts=paste('-crf', crf, .other.opts)
    )
    ## animation moves video from tempdir on success
    ## manually cleanup as we go
    if (cleanup) {
        file.remove(file.path(tempdir(), .video.name))
    }
}
