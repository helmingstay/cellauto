// shared class defns would go here:
#include "cellauto_types.h"

// [[Rcpp::plugins("cpp11")]]

// [[Rcpp::export]]
void cpp_init_address(Mats mats, int nrow, int ncol) {
    // no size() for matrix??
    int grid_size = mats.grid.length();
    int grid_nrow = mats.grid.nrow();
    // for each col (neighborhood)
    for (int icell = 0; icell < grid_size; icell++){ 
        // index of address
        int the_row = 0; 
        // loop over neighborhood rows/cols  
        for (int irow = -nrow; irow <= nrow; irow++) {
            for (int icol = -ncol; icol <= ncol; icol++ ) {
                // skip self
                if ((irow == 0) && (icol == 0)) continue;
                // index of this neighbor 
                // mod size of grid (wrap boundaries)
                int the_addr = (icell + icol + (irow*grid_nrow)) % (grid_size);
                // positive mod
                the_addr = (the_addr + grid_size) % grid_size;
                mats.address(the_row, icell) = the_addr;
                the_row++;
            }
        }
    }
}

// state transition function
// toggle alive-state,
// update neighbor counts
// and update the history grid
// return number of changes
// [[Rcpp::export]]
void cpp_update(
    const IntegerVector changes,
    Mats mats,
    const bool lives = true
) {
    int hood_to = 1;
    if (!lives) { 
        // death
        hood_to = -1;
    }
    size_t nhood = mats.address.nrow();
    for (size_t icell : changes) {
        mats.alive[icell] = lives;
        // loop through neighborhood
        for (size_t ihood = 0; ihood < nhood; ihood++) {
            size_t addr = mats.address(ihood, icell);
            mats.neighbor[addr] += hood_to;
        }
    }
};

// [[Rcpp::export]]
size_t cpp_step(
    IntegerVector born_at, IntegerVector lives_at,
    double grow, double decay,
    Mats mats, List counts
) {
    size_t nbirth, ndeath;
    // store initial state
    mats.neighbor0 = clone(mats.neighbor);
    bool alive0;
    int irule;
    // reset transition counts
    nbirth = 0;
    ndeath = 0;
    int ntot = mats.alive.length();
    // run through whole matrix
    for (int ii = 0; ii < ntot; ii++) {
        // order is important for alive0
        alive0 = mats.alive[ii];
        // number of neighbors as index
        //  to 
        irule = mats.neighbor0[ii]-1;
        //Rcpp::Rcout << "ii " << ii << " neigh " << nneigh << " alive0 " << alive0 << std::endl;
        if ( !alive0 ) {
            // was dead, born
            if( born_at[irule] ) {
                nbirth++;
                cpp_update(wrap(ii), mats);
                mats.grid[ii] = 1; 
            }
            mats.grid[ii] *= (1.0 - decay);
        } else {
            // alive
            // grow, then kill if needed
            mats.grid[ii] = (1.0 + mats.grid[ii] * grow);
            if ( !(lives_at[irule])) {
                ndeath++;
                // was alive, dies
                cpp_update(wrap(ii), mats, false);
            }
        }
    }
    // add totals for all steps also?
    counts["nbirth"] = nbirth;
    counts["ndeath"] = ndeath;
}
