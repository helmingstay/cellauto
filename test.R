require(cgolr)
require(animation)

## plotting - lattic theme
.ncolor <- 50
.cols <- heat.colors(.ncolor)
.max.z <- 1
.at <- seq(from=0, to=.max.z, length.out=.ncolor)

.theme.novpadding <- list(
    layout.heights = list(
        top.padding = 0,
        main.key.padding = 0,
        key.axis.padding = 0,
        axis.xlab.padding = 0,
        xlab.key.padding = 0,
        key.sub.padding = 0,
        bottom.padding = 0
    ),
    layout.widths = list(
        left.padding = 0,
        key.ylab.padding = 0,
        ylab.axis.padding = 0,
        axis.key.padding = 0,
        right.padding = 0
    ),
    regions=list(
        col=.cols
    )
)

set.seed(1)
.dim <- c(800,1200)
## snowflakes: diam(5,5), lives(2:3) born(3), dens(.45)
## trees: diam(4,5), lives(2:3) born(3), dens(.55)
##
.test <- new(cgolr, .dim[1],.dim[2],3,3,0,0)
.test$lives_at <- 2:3
.test$born_at <- 3
.test$grid <- matrix(
    rbinom(prod(.dim), 1, 0.35),
    nrow=.dim[1], ncol=.dim[2]
)
.test$grow <- 1.000
.test$decay <- 0.15
#.nstep <- 10
#pdf('aa.pdf')
#for (ii in 1:.nstep) {
    #print(ii)
    #image(.test$grid, useRaster=T)
    #.test$step()
#}


## plotting function
movie <- function(.nstep, .silent=T, obj) {
    .dim <- dim(obj$grid)
    .grid <- expand.grid(y=1:.dim[1], x=1:.dim[2], z=1)
    .grid$z <- as.vector(obj$grid)
    .plot <- levelplot(z ~ x*y, .grid, 
        scales=list(draw=F), xlab='', ylab='',
        colorkey=F, useRaster=T, at=.at,
        par.settings=.theme.novpadding 
    )
    plot(.plot)
    ## first step above
    for (ii in 1:(.nstep-1)) {
        if (!.silent) cat(sprintf('Processing:\t%2.0f%%\r',(100*ii)/.nstep))
        #par(mar=rep(0,4), oma=rep(0,4), mai=rep(0,4))
        #image(obj$grid, useRaster=F, xaxt='n', yaxt='n', ann=F)
        obj$step()
        .plot$panel.args.common$z <- as.vector(obj$grid)
        plot(.plot)
    }
}
## animation
ani.options(
    interval = 0.1, 
    ani.height=.dim[1], ani.width=.dim[2],
    ani.type='png', ani.dev='png'
)
my.nstep <- 1e4
saveGIF(movie(my.nstep, .silent=F, .test), movie.name='conway.big.50col.gif')
