require(cellauto)
require(animation)
require(grid)
require(plyr)

## set object defaults
.grid <- 'crosshairs'
#.dim <- c(240, 320)
.dim <- c(480, 640)
#.dim <- c(720, 1280)
cellauto_settings(.settings=color_bw(),quiet=T)
cellauto_settings(decay=0.05)

## unique filename
.basename <- paste0(
    'cellauto_movie_search',
    format(Sys.Date()), '-'
)
## video quality
.crf <- 12

ani.options(
    ## does interval have any effect??
    interval = 1/10,
    ani.type='png', ani.dev='png',
    ani.height=.dim[1], ani.width=.dim[2]
)

## grab all rules
.rules <- names(rule_by_name())
## don't run these as long
.run.short <- c('day_night', 'morley')
## max number of steps for short and long
.run.len <- list(short=500, long=1e3)
## make movie for each rule
# 
l_ply(.rules, function(.rule) {
    cat(paste0('## Processing rule ', .rule, '\n'))
    ## get settings for this rule
    .set <- rule_by_name(.rule)
    if (.rule %in% .run.short) {
        .nstep = .run.len$short
    } else {
        .nstep = .run.len$long
    }
    ## construct obj
    .this <- cellauto_new(
        .dim[1], .dim[2], 
        .settings=.set, init.grid=.grid
    )
    ## initialize plotting
    not.used <- init_plot(.this)
    ## filename for this rule
    .fn=paste0(.basename, .rule, '.mp4')
    #.fn.audio <- paste0('stereo.', .rule, '.ogg')
    ## make movie
    not.used <- movie_wrapper(
        movie_annot, .video.name=.fn,
        obj=.this, .nstep=.nstep, .npreamble=10,
        crf=.crf
    )
    ## go to dir and manually run
    #.cwd <- setwd(tempdir())
    ## process audio
    ## if doing this, make sure cleanup = F, above
    #system("sox --combine concat 'row.*.wav' fin.row.wav")
    #system("sox --combine concat 'col.*.wav' fin.col.wav")
    #system(paste0("sox -M fin.row.wav fin.col.wav -t wav - | ffmpeg -i pipe:0 -c:a libvorbis ", .fn.audio))
    #system(sprintf('ffmpeg -y -i %s -i %s -hide_banner -c:v copy  -c:a copy fin-%s', .fn, .fn.audio, .fn ))
    #system(sprintf('mv %s %s', .fn, .cwd))
    #system('rm *.wav *.ogg')
    ## and back
    #setwd(.cwd)
})
