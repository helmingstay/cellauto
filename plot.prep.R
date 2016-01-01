require(cgolr)
require(animation)

## animation
ani.options(
    ani.type='png', ani.dev='png'
)

## plotting - lattic theme
.ncolor <- 1600
#.cols <- heat.colors(.ncolor)
.max.z <- 1
.at <- seq(from=0, to=.max.z, length.out=.ncolor)

.decay.ramp.cols <- c('darkslateblue','cornflowerblue')
.decay.ramp <- colorRampPalette(colors=.decay.ramp.cols, space='Lab')(.ncolor-3)
.life.col <- 'lightslategrey'
.dead.col <- '#060606'
## set "regions" colors for lattice
.regions.col=c(.dead.col,.decay.ramp, .life.col)

## no padding levelplot
## and colors
.theme.novpadding <- list(
    regions=list(
        col=.regions.col
    ),
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
    )
)
