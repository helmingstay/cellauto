require(cgolr)
require(animation)

## No death - fills screen
# .defaults <- cgolr.set.default(decay=0.05, born=3, lives=0:8)
## Diamoeba B35678/S5678
.defaults <- cgolr_settings_default(decay=0.01, born=c(3,5,6,7,8), lives=5:8)
## 34 - blob
#.defaults <- cgolr_settings_default(decay=0.05, born=3:4, lives=3:4)
#.defaults <- cgolr.set.default(decay=0.05)

.test <- cgolr_new(720, 1280, 
    init.grid='crosshairs',
    settings=.defaults
)

init_plot(.test, 
    ncolor=10,
    color.live = 'red',
    color.dead='grey10',
    color.ramp=c('darkslateblue', 'firebrick')
)

#my.nstep <- 1.5e3
#.test <- cgolr.setup(480, 640)
#.test$grid <- cgolr.grid.random(.test, prob=0.25)

my.nstep <- 1e2
#my.nstep <- 2e3

ani.options(
    ## does interval have any effect??
    interval = 1/16,
    ani.height=.test$dim[1], ani.width=.test$dim[2]
)

saveVideo(
    cgolr_movie(.test, my.nstep),
    video.name='diamoeba-box.mp4',
    other.opts='-hide_banner -crf 5 -preset slow -c:v libx264  -pix_fmt yuv420p'
)
