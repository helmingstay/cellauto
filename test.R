pss('plot.prep.R')

set.seed(1)
## output dimensions
.dim <- c(480,640)
## snowflakes: diam(5,5), lives(2:3) born(3), dens(.45)
## trees: diam(4,5), lives(2:3) born(3), dens(.55)
##
## define gridsize, neighborhood, offsets
.test <- new(cgolr, .dim[1],.dim[2],3,3,0,0)
## basic rules
.test$lives_at <- 2:3
.test$born_at <- 3
## grow/decay does not effect dynamics
## just eye candy
.test$grow <- 1.000
.test$decay <- 0.05
## initialize grid
.test$grid <- matrix(
    #rbinom(prod(.dim), 1, 0.05),
    #rep(c(0,1,1), length.out=prod(.dim)),
    0,
    nrow=.dim[1], ncol=.dim[2]
)
.nc <- 3
#.test$grid[1:.nc,] <- rep(1, length.out=.dim[2]*.nc)
.test$grid[1:.nc,] <- 1
.test$grid[,1:.nc] <- 0
#.test$grid[,1:.nc] <- rep(c(0,1,1), length.out=.dim[1]*.nc)

## plotting function
movie <- function(.nstep, obj, .silent=T, .compare.at=1e1) {
    .dim <- dim(obj$grid)
    .grid <- expand.grid(y=1:.dim[1], x=1:.dim[2], z=1)
    .grid$z <- as.vector(obj$grid)
    .plot <- levelplot(z ~ x*y, .grid, 
        scales=list(draw=F), xlab='', ylab='',
        colorkey=F, useRaster=T, at=.at,
        par.settings=.theme.novpadding 
    )
    ## comparison
    .comp.grid <- obj$grid
    ## first step above
    for (ii in 1:(.nstep-1)) {
        if (!.silent) cat(sprintf('## Processing:\t%2.0f%%\t\tFrame %d of %d\r',(100*ii)/.nstep, ii, .nstep))
        plot(.plot)
        obj$step()
        ## stop if living cells are identical
        ## between now and last comarison grid
        if (ii%%.compare.at) {
            if (identical(.comp.grid>=1, obj$grid>=1)) {
                cat(sprintf('## No life change at gen %d\n', ii)) 
                return()
            }
            .comp.grid <- obj$grid
        }
        #.nc <- 4
        #obj$grid[,1:.nc] <- rep(c(0,1,1), length.out=.dim[1]*.nc)
        .plot$panel.args.common$z <- as.vector(obj$grid)
    }
}

ani.options(
    ## does interval have any effect??
    interval = 0.05, 
    ani.height=.dim[1], ani.width=.dim[2]
)

my.nstep <- 4e3
saveVideo(
    movie(my.nstep, .test, .silent=F), 
    video.name='conway.source.mp4',
    ## ffmpeg opts: https://trac.ffmpeg.org/wiki/Encode/H.264
    other.opts='-framerate 15 -qp 0  -preset veryslow -c:v libx264 -r 30 -pix_fmt yuv420p'
)
