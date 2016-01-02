pss('plot.prep.R')

set.seed(2)
## output dimensions
#.dim <- c(600,800)
.dim <- c(720, 1280)
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
.test$decay <- 0.01
## initialize grid
.test$grid <- matrix(
    #rbinom(prod(.dim), 1, 0.05),
    #rep(c(0,1,1), length.out=prod(.dim)),
    0,
    nrow=.dim[1], ncol=.dim[2]
)
## where to place horiz bars
.nc <- c(1,.dim[1] %/% c(2:5))
## chop off this many pixels from ends
.ends <- 0

#.test$grid[1:.nc,] <- rep(1, length.out=.dim[2]*.nc)
.test$grid[.nc,(1+.ends):(.dim[2]-.ends)] <- 1
#.test$grid[,1:.nc] <- 0
#.test$grid[,1:.nc] <- rep(c(0,1,1), length.out=.dim[1]*.nc)

## plotting function
movie <- function(.nstep, obj, .silent=T, 
    .compare.at=1e1, 
    .noise.at=1e2, .noise.prop=1e-4
) {
    ## dimension and total number of elements
    .dim <- dim(obj$grid)
    .n.tot <- prod(.dim)
    ## number of noise pixels, int
    .n.noise <- round(.n.tot * .noise.prop)
    ## For levelplot
    .plot.grid <- expand.grid(y=1:.dim[1], x=1:.dim[2], z=1)
    .plot.grid$z <- as.vector(obj$grid)
    .plot <- levelplot(z ~ x*y, .plot.grid, 
        scales=list(draw=F), xlab='', ylab='',
        colorkey=F, useRaster=T, at=.at,
        par.settings=.theme.novpadding 
    )
    ## comparison
    .comp.grid <- obj$grid
    ## first step above
    for (ii in 1:(.nstep-1)) {
        ## report
        if (!.silent) cat(sprintf('## Processing:\t%2.0f%%\t\tFrame %d of %d\r',(100*ii)/.nstep, ii, .nstep))
        ## plot, then step
        plot(.plot)
        obj$step()
        ## stop if living cells are identical
        ## between now and last comarison grid
        if (!(ii%%.compare.at)) {
            if (identical(.comp.grid>=1, obj$grid>=1)) {
                cat(sprintf('## No life change at gen %d\n', ii)) 
                return()
            }
            .comp.grid <- obj$grid
        }
        ## inject life-noise at given period
        if (!(ii%%.noise.at)) {
            ## indices of noise
            .noise <- sample.int(.n.tot, size=.n.noise)
            obj$grid[.noise] <- 1
        }
        #.nc <- 4
        #obj$grid[,1:.nc] <- rep(c(0,1,1), length.out=.dim[1]*.nc)
        .plot$panel.args.common$z <- as.vector(obj$grid)
    }
}

ani.options(
    ## does interval have any effect??
    interval = 1/30,
    ani.height=.dim[1], ani.width=.dim[2]
)

my.nstep <- 8e3
my.noise.at <- 5e2
my.noise.prop <- 5e-4
saveVideo(
    movie(my.nstep, .test, .silent=F, 
        .noise.at=my.noise.at, .noise.prop=my.noise.prop
    ), video.name='conway.noise.mp4',
    ## ffmpeg opts: https://trac.ffmpeg.org/wiki/Encode/H.264
    other.opts='-hide_banner -qp 0 -preset veryslow -c:v libx264  -pix_fmt yuv420p -r 30'
)
