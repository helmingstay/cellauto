## Deprecated
## possibly interesting: example of PNG input
#source('plot.prep.R')
#source('mk.movie.R')
library(png)
require(cellauto)

## file from https://www.flickr.com/photos/129008627@N07/20312241890/in/album-72157652172210561/
.cwd <- setwd('input')
#system('convert input/tess.star.jpg -rotate 90 -resize 1280x720\\! -colorspace Gray  -white-threshold 25%   input/tess.star.gray.png'
.fn.base <- 'chama'
.fn.new <- sprintf('fix-%s.png',.fn.base)
.vid.name <- sprintf('picvid.%s.%s.mp4', .fn.base, 'base')
system(sprintf('convert %s.jpg -crop 1280x720+0+0 -colorspace Gray -black-threshold 33%% -negate %s', .fn.base, .fn.new))
.in.png <- readPNG(source=.fn.new)
## flip vert
.in.png <- .in.png[nrow(.in.png):1,]
setwd(.cwd)


set.seed(2)
## output dimensions
.dim <- c(720, 1280)
my.nstep <- 5e2
## define gridsize, neighborhood, offsets
.test <- new(cgolr, .dim[1],.dim[2])
## rule atlas: https://en.wikipedia.org/wiki/Life-like_cellular_automaton
## "normal rules
.test$init_rules(born_at=3, lives_at=2:3, 1,1,0,0)
##
## replicator - descent into static @~1500
#.test$init_rules(lives_at=c(1,3,5,7), born_at=c(1,3,5,7), r.rad=1, c.rad=1, r.offset=0, c.offset=0)
## 34 life
#.test$init_rules(lives_at=c(3,4), born_at=c(3,4), r.rad=1, c.rad=1, r.offset=0, c.offset=0)
## grow/decay does not effect dynamics
## just eye candy
.test$grow <- 1.000
.test$decay <- 0.025
## initialize grid
.test$grid <- .in.png

ani.options(
    ## does interval have any effect??
    interval = 1/12,
    ani.height=.dim[1], ani.width=.dim[2]
)


#my.noise.at <- 1
## approx once per frame
#my.noise.prop <- my.noise.at/prod(.dim) 
#my.noise.start <- my.nstep
saveVideo(
    mk.movie(my.nstep, .test, .silent=F, 
        #.noise.at=my.noise.at, .noise.prop=my.noise.prop,
        #.noise.start=my.noise.start,
    ), video.name=.vid.name,
    ## ffmpeg opts: https://trac.ffmpeg.org/wiki/Encode/H.264
    ## -framerate for input, -r for output
    ## see video.stackexchange.com/questions/13066/how-to-encode-a-video-at-30-fps-from-images-taken-at-7-fps
    other.opts='-hide_banner -crf 10 -preset slow -c:v libx264  -pix_fmt yuv420p'
)
