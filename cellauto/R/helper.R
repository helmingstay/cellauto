## use grid.text to annotate current device
helper_overlay_text <- function(
    .text, .num, .y, .x=0.01, .just=c(0,0),
    .gp = gpar(fontsize=22, col="lightgreen")
) {
    grid.text(paste0(.text,.num), x=.x, y=.y, just=.just, gp=.gp)
}


## summarize grid,
## not currently very helpful
helper_measure <- function(obj) {
    .col <- sd(colMeans(obj$grid))
    .row <- sd(rowMeans(obj$grid))
    ret <- c(
        age=obj$age, sdcol=.col, sdrow=.row, 
        zeros=sum(obj$grid==0), alive=obj$nalive,
        birth=obj$nbirth, death=obj$ndeath)
    return(ret)
}

## test for any change in last ngen
## if no change return NULL
helper_compare_grid <- function(
    obj, changes_old, compare_ngen, init=F
){
    ## first run, fill lise
    if(init) {
        ## just need correct dimensions
        return(as.vector(obj$grid>0))
    }
    changes <-  as.vector(obj$grid >= (1-obj$decay)^compare_ngen)
    if( identical(changes, changes_old)) {
        ## no change
        return(NULL)
    } else {
        return(changes)
    }
}
