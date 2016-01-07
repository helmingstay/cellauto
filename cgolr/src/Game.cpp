//

#include <RcppArmadillo.h>

using arma::fill::zeros;

// 0/1 square matrix of dim nn x nn 
// multiply by game grid to sum over neighborhood
class mask {                     
public:                         
    mask(int nn, int diam, int offset) :
        nhood(nn,nn,zeros)
    {
        make_mask(diam, offset);
    };
    void make_mask(int diam, int offset) {
        // default offset
        int center = (diam / 2) + 1 ;
        //int center = 0;
        // row mask
        for(int ii = 0; ii < nhood.n_rows; ii++) {
            for(int jj = 0; jj < nhood.n_rows; jj++) {
                // off-diagonal, wrap boundaries
                if  ( (ii + jj + center + offset) % nhood.n_rows < diam ) {
                    nhood(ii, (nhood.n_rows-1)-jj) = 1;
                }
            }
        }
    }
    // square matrix, fill w/0&1
    arma::umat nhood;
    // define width of neighborhood
    int diam;
    // handedness, for even diam
    bool right_hand;
};

// master class - grid, mask, and advancement rules
class cgolr {                     // 
public:                         // which have a getter/setter and getter
    cgolr(
        int nrow, int ncol, 
        int diam_row, int diam_col,
        int offset_row, int offset_col
    ) : 
        grow(1.0), decay(1.0),
        grid(nrow, ncol, zeros),
        neighbor(nrow, ncol, zeros),
        alive(nrow, ncol, zeros),
        rowmask(nrow, diam_row, offset_row),
        colmask(ncol, diam_col, offset_col)
    {};
    arma::uvec lives_at;
    arma::uvec born_at;
    double decay;
    double grow;
    // game grid, double (for decay)
    arma::mat grid;
    // counts of living neighbors
    arma::umat neighbor;
    // currently alive
    arma::umat alive;
    arma::umat alive_prev;
    // index vectors from find
    // records state transitions
    arma::uvec update_birth;
    arma::uvec update_death;
    mask rowmask;
    mask colmask;
    
    // reset counts on grid edit??
    void init_grid() {
        neighbor.zeros();
        alive.zeros();
        alive_prev.zeros();
        // seed history
        // alive == 2, delta==1
        alive = (grid >= 1.0);
        alive_prev = (grid >= 1.0);
        // recompute neighbors
        init_neighbor();
    }
    
    // recompute neighbor counts
    void init_neighbor() {
        // matrix mult - sum of living cells in neighborhood, including self
        neighbor = (rowmask.nhood * alive) * colmask.nhood;
        // subtract off self
        neighbor = neighbor - alive;
        // initialize empty bool comparison mat
    }

    // advance one step
    void step() {
        init_neighbor();
        // store last state, edit alive 
        alive_prev = alive;
        // find cells that fulfill keep-living conts
        for (int ii = 0; ii < lives_at.size(); ii++) {
            // tag members fulfilling condition
            alive(find( neighbor == lives_at(ii))).fill(3);
        }
        // Either was not alive prev 
        // or does not fulfill current conditions
        update_death = find(alive + alive_prev != 4);
        alive(update_death).zeros();
        //
        // find births 
        for (int ii = 0; ii < born_at.size(); ii++) {
            // tag members fulfilling condition
            alive(find(neighbor == born_at(ii))).fill(3);
        }
        // record birth
        update_birth = find(alive + alive_prev == 3);
        alive(update_birth).fill(1);
        //cleanup
        alive(find(alive==3)).ones();
        // update states
        // first, update living-in-previous
        //grid *= grow ;
        grid( update_death ) *= (1.0 - decay) ;
        // births
        grid( update_birth ).ones();
    }

    
    // getters/setters 
    arma::mat get_grid() {
        return grid;
    }
    void set_grid(arma::mat gr) {
        if ( arma::size(gr) != arma::size(grid) ) {
            throw std::range_error("Dimension of new grid must match old");
        }
        grid = gr;
        init_grid();
    }
    // getters/setters for masks
    // rows
    arma::umat get_rowmask() {
        return rowmask.nhood;
    }
    void set_rowmask(arma::umat nhood_) {
        if ( !nhood_.is_square() ) {
            throw std::range_error("Rowmask must be square matrix");
        }
        if ( nhood_.n_rows != grid.n_rows ) {
            throw std::range_error("Rowmask dim must match game grid nrows");
        }
        rowmask.nhood = nhood_;
    }
    // cols
    arma::umat get_colmask() {
        return colmask.nhood;
    }
    void set_colmask(arma::umat nhood_) {
        if ( !nhood_.is_square() ) {
            throw std::range_error("Colmask must be square matrix");
        }
        if ( nhood_.n_rows != grid.n_rows ) {
            throw std::range_error("Colmask dim must match game grid ncol");
        }
        colmask.nhood = nhood_;
    }

//private:
};


RCPP_MODULE(GameEx){
    using namespace Rcpp ;

    class_<cgolr>( "cgolr" )

    .constructor<int,int,int,int,int,int>("Set up new game: nrow (int); ncol (int); mask diam: (row int, col int); mask offset: (row int, col int)")

    .method("step", &cgolr::step, "Advance 1 step")
    //.field_readonly("rowmask", &cgolr::rowmask, "")
    //.field_readonly("colmask", &cgolr::colmask, "")
    //
    // user-accessible
    .field("lives_at", &cgolr::lives_at, "vector")
    .field("born_at", &cgolr::born_at, "vector")
    .field("decay", &cgolr::decay, "Numeric: per-time rate of decay of dead cells (1=instant death, 0=immortal)")
    .field("grow", &cgolr::grow, "Numeric: pert-time rate of growth of live cells")

    // read and write property
    .property( "grid", &cgolr::get_grid, &cgolr::set_grid)
    .property( "rowmask", &cgolr::get_rowmask, &cgolr::set_rowmask)
    .property( "colmask", &cgolr::get_colmask, &cgolr::set_colmask)

    // read-only property
    //.property( "y", &Num::getY )
    ;
}
