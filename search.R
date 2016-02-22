require(cgolr)
require(animation)
require(grid)
require(plyr)
pss('helpers.R')


.grid <- 'crosshairs'
.dim <- c(720, 1280)
cgolr_settings(settings=color_bw(),quiet=T)
#cgolr_settings(decay=0.05)




ani.options(
    ## does interval have any effect??
    interval = 1/10,
    ani.height=.dim[1], ani.width=.dim[2]
)

## grab all rules
.rules <- names(rule_by_name())
## don't run these as long
.run.short <- c('day_night', 'morley')
## make movie for each rule
# 
l_ply(.rules, function(.rule) {
    cat(paste0('## Processing rule ', .rule, '\n'))
    ## get settings for this rule
    .set <- rule_by_name(.rule)
    if (.rule %in% .run.short) {
        .nstep = 500
    } else {
        .nstep = 1e3
    }
    #.nstep = 100
    ## construct
    .this <- cgolr_new(
        .dim[1], .dim[2], 
        settings=.set, init.grid=.grid
    )
    ## initialize plotting
    #init_plot(.this)
    not.used <- init_plot(.this)
    ## advance / plot / render
    .fn=paste0('classic-', .rule, '.mp4')
    #.fn.audio <- paste0('stereo.', .rule, '.ogg')
    not.used <- saveVideo(
        movie_steps(.this, .nstep=.nstep, .npreamble=10),
        video.name=.fn,
        ## throw away - fast
        other.opts='-hide_banner -crf 2 -preset slow -c:v libx264  -pix_fmt yuv420p'
    )
    ## clean up
    file.remove(file.path(tempdir(), .fn))
    ## go to dir and manually run
    #.cwd <- setwd(tempdir())
    #system(paste0('ffmpeg -y -framerate 10 -i Rplot%d.png -hide_banner -crf 2 -preset slow -c:v libx264  -pix_fmt yuv420p ', .fn))
    ## process audio
    #system("sox --combine concat 'row.*.wav' fin.row.wav")
    #system("sox --combine concat 'col.*.wav' fin.col.wav")
    #system(paste0("sox -M fin.row.wav fin.col.wav -t wav - | ffmpeg -i pipe:0 -c:a libvorbis ", .fn.audio))
    #system(sprintf('ffmpeg -y -i %s -i %s -hide_banner -c:v copy  -c:a copy fin-%s', .fn, .fn.audio, .fn ))
    #system(sprintf('mv %s %s', .fn, .cwd))
    #system('rm *.wav *.ogg')
    ## and back
    #setwd(.cwd)
})
