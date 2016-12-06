// shared class defns would go here:
#include "cellauto_types.h"

// [[Rcpp::plugins("cpp11")]]

// [[Rcpp::export]]
void cpp_init_address(Mats mats, size_t nrow, size_t ncol) {
    // no size() for matrix??
    size_t grid_size = mats.grid.length();
    size_t grid_nrow = mats.grid.nrow();
    // for each col (neighborhood)
    for (size_t icell = 0; icell < grid_size; icell++){ 
        // index of address
        size_t counter = 0;
        // loop over neighborhood rows/cols  
        for (int irow = 0-nrow; irow <= nrow; irow++) {
            for (int icol = 0-ncol; icol <= ncol; icol++ ) {
                // skip self
                if (irow == 0 & icol == 0) continue;
                // index of this neighbor 
                // mod size of grid (wrap boundaries)
                mats.address[icell, counter] = (icell + irow + (icol*grid_nrow)) % (grid_size);
                counter++;
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
    const bool birth = true
) {
    size_t hood_to = 1;
    if (!birth) { 
        // death
        hood_to = -1;
    }
    //Rcpp::Rcout << "#" ;
    size_t nhood = mats.neighbor.nrow();
    for (size_t icell : changes) {
        //Rcpp::Rcout << "." ;
        // R-to-Cpp
        icell=icell-1;
        // loop through neighborhood
        for (size_t ihood = 0; ihood < nhood; ihood++) {
            size_t addr = mats.address[ihood, icell];
            mats.neighbor[addr] += hood_to;
        }
    }
};

// [[Rcpp::export]]
size_t cpp_steps(
    int nstep, 
    IntegerVector born_at, IntegerVector lives_at,
    double grow, double decay,
    Mats mats, List counts
) {
    size_t nbirth, ndeath;
    for (size_t istep = 0; istep < nstep; nstep++) {
        // store initial state
        mats.neighbor0 = clone(mats.neighbor);
        bool alive0;
        size_t ineigh;
        // reset transition counts
        nbirth = 0;
        ndeath = 0;
        size_t ntot = mats.alive.length();
        // run through whole matrix
        for (int ii = 0; ii < ntot; ii++) {
            //Rcpp::Rcout << "++" ;
            // order is important for alive0
            alive0 = mats.alive[ii];
            // number of neighbors as index
            ineigh = mats.neighbor0[ii];
            //Rcpp::Rcout << "# neigh " << ineigh << " born_at " << born_at << std::endl;
            //Rcpp::Rcout << "#" ;
            if ( !alive0 ) {
                // was dead, born
                //Rcpp::Rcout << "# neigh " << ineigh << " born_at " << born_at << std::endl;
                if( born_at[ineigh] ) {
                    nbirth++;
                    cpp_update(wrap(ii), mats);
                }
                mats.grid[ii] *= (1.0 - decay);
            } else {
                // alive
                if ( !(lives_at[ineigh])) {
                    ndeath++;
                    // was alive, dies
                    cpp_update(wrap(ii), mats, false);
                }
                mats.grid[ii] = (1.0 + mats.grid[ii] * grow);
            }
        }
    }
    // add totals for all steps also?
    counts["nbirth"] = nbirth;
    counts["ndeath"] = ndeath;
}
