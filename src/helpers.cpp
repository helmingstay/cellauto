#include <Rcpp.h>
#include "cellauto.h"

// [[Rcpp::plugins("cpp11")]]

using Rcpp::NumericVector;
using Rcpp::IntegerVector;
using Rcpp::LogicalVector;
using Rcpp::IntegerMatrix;

// [[Rcpp::export]]
void cpp_init_address(
    IntegerMatrix addr, 
    const size_t radius_row,
    const size_t radius_col,
    const size_t grid_nrow
) {
    size_t grid_size = addr.ncol();
    // for each col (neighborhood)
    for (size_t icell = 0; icell < grid_size; icell++){ 
        // index of address
        size_t counter = 0;
        // loop over neighborhood rows/cols  
        for (int jrow = 0-radius_row; jrow <= radius_row; jrow++) {
            for (int jcol = 0-radius_col; jcol <= radius_col; jcol++ ) {
                // skip self
                if (jrow == 0 & jcol == 0) continue;
                // index of this neighbor 
                // mod size of grid (wrap boundaries)
                addr(icell, counter) = (icell + jrow + (jcol*grid_nrow)) % (grid_size);
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
size_t cpp_update(
    const bool birth,
    const IntegerVector changes,
    List mats
) {
    size_t hood_to = 1;
    if (!birth) { 
        // death
        hood_to = -1
    }
    nhood = mats["neighbor"].nrow();
    for (auto icell : changes) {
        // R-to-Cpp
        icell=icell-1;
        // loop through neighborhood
        for (size_t ihood = 0; ihood < nhood; ihood++) {
            addr = (mats["address"])[ihood, icell];
            (mats["neighbor"])(addr) += hood_to;
        }
    }
    return changes.size();
};

// [[Rcpp::export]]
size_t cpp_steps(size_t nstep, List mats) {
    for (size_t istep = 0; istep < nstep; nstep++) {
        IntegerMatrix tmp_neighbor(mats["neighbor"]);
        bool alive0;
        size_t neighbor0;
        // reset transition counts
        size_t nbirth = 0;
        size_t ndeath = 0;
        size_t ntot = alive.size();
        // run through whole matrix
        for (int ii = 0; ii < ntot; ii++) {
            // order is important for alive0
            alive0 = mats["alive"](ii);
            neighbor0 = tmp_neighbor(ii);
            if ( !alive0 ) {
                // was dead, born
                if( born_at(neighbor0) ) {
                    nbirth++;
                    cpp_update(true, ii, mats);
                }
                mats["grid"](ii) *= (1.0 - decay);
            } else {
                // alive
                if ( !(lives_at(neighbor0))) {
                    ndeath++;
                    // was alive, dies
                    cpp_update(false, ii, mats);
                }
                mats["grid"](ii) = 1.0 + mats["grid"](ii) * grow;
            }
        }
    }
}
