## R prototype of package

# Generates a new board random matrix 
# each cell is true w/prob==.density
mk.random.board <- function(
    .nrow=10, .ncol=10, .density=0.5
){
    ret <- rbinom(.nrow*.ncol, size=1, prob=.density)
    ret <- matrix(ret,  nrow=.nrow)
}

# Implements the rulebase
mk.board.step <- function(board, decay, .inmat, .outmat, .alive.thresh, .lives.at) {
    .alive <- board == 1
    .counts <- (.inmat %*% .alive) %*% .outmat 
    .counts <- .counts - .alive
    ## 3 neighbors, or two and alive
    .lives <- .counts == .lives.at | (.counts == (.lives.at - 1) & board >= .alive.thresh )
    ## order is important here
    ret <- board * decay
    ret[.lives] <- TRUE
    return(ret)
}

###  Play Life.  
## By default, plot lattice levelplot, 
## pausing between plot updates.
mk.conway <- function(
    board, .nsteps=50, .plot=TRUE, .pause=T, .delay=0.2,
    .decay=0.5, .alive.thresh = 1, .diam = 3, .lives.at = NULL
) {
    if (is.null(.lives.at)) .lives.at <- .diam
    if(!require(lattice)) stop("Lattice is required")
    .old <- board
    .nrow <- nrow(board)
    .ncol <- ncol(board)
    ## matrices for summing neighborhood
    .inmat <- sapply(1:.nrow, function(x) (.nrow:1 + x )%%.nrow < .diam)
    .outmat <- sapply(1:.ncol, function(x) (.ncol:1 + x )%%.ncol < .diam)
    browser()

    for(ii in 1:.nsteps) {
        if(.plot) {
            .the.plot <- levelplot(
                t(board[.nrow:1,]), colorkey=FALSE,
                xlab=sprintf('Round %d', ii),
                ylab=''
            )
            print(.the.plot)
        }
        if(.pause) {
            Sys.sleep(.delay)
        }
        .new <- mk.board.step(board, .decay, .inmat, .outmat, .alive.thresh, .lives.at)
        if(all((.new==1)==(board==1)) ) {
            cat(sprintf("Life ended at round %d.\n", ii))
            return(.new)
        } 
        if (all((.new==1) == (.old==1)) ) {
            cat(sprintf("Life is boring at round %d.\n", ii))
            return(.new)
        }
        
        .old <- board
        board <- .new
    }
}

.decay.ramp.cols <- c('darkslateblue','cornflowerblue')
.decay.ramp <- colorRampPalette(colors=.decay.ramp.cols, space='Lab')(99)
.life.col <- 'lightslategrey'
## set "regions" colors for lattice
trellis.par.set(regions=list(col=c(.decay.ramp, .life.col))); 
## inspect
show.settings()

# Example usage
#game.of.life(gen.board("blinker"))
#game.of.life(gen.board("glider", 18, 20))
#game.of.life(gen.board(, 50, 50))
a.board <- mk.random.board(200, 300, .density=.3)
life <- mk.conway(a.board,
    ## go forever
    .nsteps=1e6, .alive.thresh = 0.05,
    ## quickly
    .pause=F, #.delay=0.1, 
    ## between zero and 1, small = fast decay
    .decay=2.8/pi,
    .diam = 7,
    .lives.at = 6
)

## terraces: density = 0.01, .alive=0, .decay=2/pi, .diam = 5, .lives.at=4
## death to space invaders: density = 0.3, .alive=0, .decay=2/pi, .diam = 6, .lives.at=7
##  hyphae colonizers: density = 0.3, .alive=0, .decay=2/pi, .diam = 7, .lives.at=7
