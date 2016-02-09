require(cgolr)
require(animation)
require(grid)
require(plyr)


.grid <- 'crosshairs'
.dim <- c(720, 1280)
cgolr_settings(decay=0.05)

.my.text <- function(
    .text, .num, .y, .x=0.01, .just=c(0,0),
    .gp = gpar(fontsize=20, col="grey")
) {
    grid.text(paste0(.text,.num), x=.x, y=.y, just=.just, gp=.gp)
}

## plot with time-jumps
movie_steps <- function(x, 
    .nstep = 150, .ntimes = 5, .nframes = 50
) {
    for (ii in 1:.ntimes) {
        for (jj in 1:.nframes) {
            ## plot, add age, step 
            plot(levelplot(x))
            .my.text('Rule: ', x$user_data$init_settings$rule_name, .y=0.15)
            .my.text('Age: ', x$age, .y=0.115)
            .my.text('Alive: ', sprintf('%2.3f%%', x$nalive / prod(x$dim)), .y=0.08)
            .my.text('Born: ', x$nbirth, .y=0.045)
            .my.text('Died: ', x$ndeath, .y=0.01)
            x$step()
        }
        ## fastforward nsteps, repeat plot
        x$steps(.nstep)
        ## report
        cat(sprintf('## %2d%%\r', round(100*(ii/.ntimes))))
    }
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

ani.options(
    ## does interval have any effect??
    interval = 1/10,
    ani.height=.dim[1], ani.width=.dim[2]
)

## grab all rules
.rules <- names(cgolr_settings_rule_by_name())
## make movie for each rule
# 
l_ply(tail(.rules,2), function(.rule) {
    cat(paste0('## Processing rule ', .rule, '\n'))
    ## get settings for this rule
    .set <- cgolr_settings_rule_by_name(.rule)
    ## construct
    .this <- cgolr_new(
        .dim[1], .dim[2], 
        settings=.set, init.grid=.grid
    )
    ## initialize plotting
    .init_plot(.this)
    ## advance / plot / render
    .fn=paste0('search-', .rule, '.mp4')
    saveVideo(
        movie_steps(.this),
        video.name=.fn,
        ## throw away - fast
        other.opts='-hide_banner -crf 20 -preset fast -c:v libx264  -pix_fmt yuv420p'
    )
    ## go to dir and manually run
    .cwd <- setwd(tempdir())
    system(paste0('ffmpeg -y -framerate 10 -i Rplot%d.png -hide_banner -crf 2 -preset slow -c:v libx264  -pix_fmt yuv420p ', .fn))
    system(sprintf('cp %s %s', .fn, .cwd))
    setwd(.cwd)
})
