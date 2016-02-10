measure <- function(x) {
    .col <- sd(colMeans(x$grid))
    .row <- sd(rowMeans(x$grid))
    ret <- c(
        age=x$age, sdcol=.col, sdrow=.row, 
        zeros=sum(x$grid==0), alive=x$nalive,
        birth=x$nbirth, death=x$ndeath)
    return(ret)
}

tone <- function(freq, interval=1/10,
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

bleep <- function(.df, .means, prev, interval=1/10,
    .bounds = c(40, 500), ...
) {
    .freqs <- .bounds[1]:.bounds[2]
    ## high sd = flat freq weights
    .prob.col <- 1/(.freqs^(1/(5*.df$sdcol)))
    .prob.row <- 1/(.freqs^(1/(5*.df$sdrow)))
    #.prob.col <- 1/(.freqs^(1/(.df$sdcol/.means$col)))
    #.prob.row <- 1/(.freqs^(1/(.df$sdrow/.means$row)))
    .tone.row <- tone(sample(.freqs, 1, prob=.prob.row), interval)
    .tone.col <- tone(sample(.freqs, 1, prob=.prob.col), interval)
    .fn.row <- sprintf('row.%05d.wav', .df$frame)
    .fn.col <- sprintf('col.%05d.wav', .df$frame)
    save.wave(.tone.col, .fn.col)
    save.wave(.tone.row, .fn.row)
}

.init_plot <- function(x) {
    init_plot(x,
        ncolor=100,
        color.live = 'red',
        color.dead='grey10',
        color.ramp=c('darkslateblue', 'firebrick')
        #color.ramp=c('darkslateblue', 'green')
    )
}

.my.text <- function(
    .text, .num, .y, .x=0.01, .just=c(0,0),
    .gp = gpar(fontsize=20, col="grey")
) {
    grid.text(paste0(.text,.num), x=.x, y=.y, just=.just, gp=.gp)
}

## plot with time-jumps
movie_steps <- function(x, 
    .nstep = 150, 
    .ndead = 30, .npreamble=30
) {
    .rule <- x$user_data$init_settings$rule_name
    ## number of total vid frames 
    ## stats nrows should equal
    .frame = 1
    ## fill data.frame to put grid statistics in
    stats <- as.data.frame(t(measure(x)))
    stats[2:(.npreamble+.nstep),] <- NA
    ## preamble - roll tape before advancing model
    for(kk in 1:.npreamble) {
        stats[.frame,] <- measure(x)
        .frame <- .frame + 1
        plot(levelplot(x))
        .my.text('Rule: ', x$user_data$init_settings$rule_name, .y=0.15)
    }
    for (jj in 1:.nstep) {
        ## plot, add age, step 
        plot(levelplot(x))
        .my.text('Rule: ', .rule, .y=0.15)
        .my.text('Age: ', x$age, .y=0.115)
        .my.text('Alive: ', sprintf('%2.3f%%', x$nalive / prod(x$dim)), .y=0.08)
        .my.text('Born: ', x$nbirth, .y=0.045)
        .my.text('Died: ', x$ndeath, .y=0.01)
        ## if dead long enough, call it quits
        if (!x$nalive) {
            .ndead = .ndead -1
        }
        if (!.ndead) return()
        cat(sprintf('## %2d%%\r', round(100*(jj/.nstep))))
        x$step()
        stats[.frame,] <- measure(x)
        .frame <- .frame + 1
    }
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
