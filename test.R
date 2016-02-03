pss('plot.prep.R')
source('mk.movie.R')
require(cgolr)

set.seed(2)
## output dimensions
#.dim <- c(600,800)
.dim <- c(720, 1280)
my.nstep <- 1.5e3
## snowflakes: diam(5,5), lives(2:3) born(3), dens(.45)
## trees: diam(4,5), lives(2:3) born(3), dens(.55)
##
## define gridsize, neighborhood, offsets
.test <- new(cgolr, .dim[1],.dim[2])
## rule atlas: https://en.wikipedia.org/wiki/Life-like_cellular_automaton
## "normal rules
#.test$init_rules(lives_at=2:3, born_at=3, 1,1,0,0)
##
## replicator - descent into static @~1500
#.test$init_rules(lives_at=c(1,3,5,7), born_at=c(1,3,5,7), r.rad=1, c.rad=1, r.offset=0, c.offset=0)
## 34 life
.test$init_rules(born_at=c(3,4), lives_at=c(3,4), r.rad=1, c.rad=1, r.offset=0, c.offset=0)
## grow/decay does not effect dynamics
## just eye candy
.test$grow <- 1.000
.test$decay <- 0.1
## initialize grid
.init.mat <- matrix(
    #rbinom(prod(.dim), 1, 0.05),
    #rep(c(0,1,1), length.out=prod(.dim)),
    0,
    nrow=.dim[1], ncol=.dim[2]
)
## chop off this many pixels from ends
.ends <- 10
## cut region into halves, thirds, 4ths...
#.h.bar.spec <- c(3/2, 4/3, 2:4)
.h.bar.spec <- c(4, 4/3)
#.v.bar.spec <- c(4, 4/3)
.v.bar.spec <- c(3/1)
## index of rows/cols to place horiz / vert bars
.h.bar <- .dim[1] %/% .h.bar.spec
.v.bar <- .dim[2] %/% .v.bar.spec
##  ensure square box 
.v.bar <- c(.v.bar, .v.bar+diff(.h.bar))
## crete line between box corners
.diag <- (.ends:(min(.dim)-.ends))
.offset <- min(.v.bar) - min(.h.bar)
.diag <- cbind(row=.diag, 
    col1=(.diag)+.offset, 
    col2=rev(.diag)+.offset
)


#.test$grid[1:.nc,] <- rep(1, length.out=.dim[2]*.nc)
.init.mat[.h.bar,(1+.ends):(.dim[2]-.ends)] <- 1
.init.mat[(1+.ends):(.dim[1]-.ends), .v.bar] <- 1
.init.mat[.diag[,c(1,2)]] <- 1
.init.mat[.diag[,c(1,3)]] <- 1
.test$grid <- .init.mat
#.test$grid[,1:.nc] <- 0
#.test$grid[,1:.nc] <- rep(c(0,1,1), length.out=.dim[1]*.nc)


ani.options(
    ## does interval have any effect??
    interval = 1/16,
    ani.height=.dim[1], ani.width=.dim[2]
)

#my.nstep <- 2e2
#   user  system elapsed
#364.968  58.400 311.762
## without plotting:
#user  system elapsed         Frame 199 of 200
#205.068   0.788 205.541
## new, with plotting
#   user  system elapsed
#133.352   1.964 115.793
## new, without plotting
# user  system elapsed
#  3.088   0.664   3.196

#my.noise.at <- 1
## approx once per frame
#my.noise.prop <- my.noise.at/prod(.dim) 
#my.noise.start <- my.nstep
saveVideo(
    mk.movie(my.nstep, .test, .silent=F, 
        #.noise.at=my.noise.at, .noise.prop=my.noise.prop,
        #.noise.start=my.noise.start,
    ), video.name='rule.34.box.mp4',
    ## ffmpeg opts: https://trac.ffmpeg.org/wiki/Encode/H.264
    ## -framerate for input, -r for output
    ## see video.stackexchange.com/questions/13066/how-to-encode-a-video-at-30-fps-from-images-taken-at-7-fps
    other.opts='-hide_banner -qp 0 -preset slow -c:v libx264  -pix_fmt yuv420p'
)
