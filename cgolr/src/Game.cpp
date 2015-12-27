//

#include <RcppArmadillo.h>

using arma::fill::zeros;

// 0/1 square matrix of dim nn x nn 
// multiply by game grid to sum over neighborhood
class mask {                     
public:                         
    mask(int nn, int diam, int offset) :
        grid(nn,nn,zeros)
    {
        make_mask(diam, offset);
    };
    void make_mask(int diam, int offset) {
        // default offset
        int center = (diam / 2) + 1 ;
        //int center = 0;
        // row mask
        for(int ii = 0; ii < grid.n_rows; ii++) {
            for(int jj = 0; jj < grid.n_rows; jj++) {
                // off-diagonal, wrap boundaries
                if  ( (ii + jj + center + offset) % grid.n_rows < diam ) {
                    grid(ii, (grid.n_rows-1)-jj) = 1;
                }
            }
        }
    }
    // square matrix, fill w/0&1
    arma::mat grid;
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
        rowmask(nrow, diam_row, offset_row),
        colmask(ncol, diam_col, offset_col)
    {};
    arma::vec lives_at;
    arma::vec born_at;
    double decay;
    double grow;
    // game grid
    arma::mat grid;
    mask rowmask;
    mask colmask;

    // advance one step
    void step() {
        arma::umat now_alive = (grid >= 1.0);
        // matrix mult - sum of living cells in neighborhood, including self
        arma::mat counts = (rowmask.grid * now_alive) * colmask.grid;
        // subtract off self
        counts = counts - now_alive;
        // initialize empty bool comparison mat
        arma::umat tmp_lives(grid.n_rows, grid.n_cols, zeros);
        arma::umat tmp_born(grid.n_rows, grid.n_cols, zeros);
        // find keep-living 
        for (int ii = 0; ii < lives_at.size(); ii++) {
            tmp_lives.elem(find(counts == lives_at(ii))).ones();
        }
        // (only if currently alive)
        tmp_lives.elem(find(now_alive < 1 )).zeros();
        //
        // find births 
        for (int ii = 0; ii < born_at.size(); ii++) {
            tmp_born.elem(find(counts == born_at(ii))).ones();
        }
        // (only if currently not alive)
        tmp_born.elem(find(now_alive == 1 )).zeros();
        //
        // update states
        // first, update living-in-previous
        grid.elem( find(tmp_lives == 1)  ) *= grow ;
        grid.elem( find(tmp_lives != 1)  ) *= (1.0 - decay) ;
        // births
        grid.elem( find(tmp_born == 1)  ).ones();
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
    }
    // getters/setters for masks
    // rows
    arma::mat get_rowmask() {
        return rowmask.grid;
    }
    void set_rowmask(arma::mat gr) {
        if ( !gr.is_square() ) {
            throw std::range_error("Rowmask must be square matrix");
        }
        if ( gr.n_rows != grid.n_rows ) {
            throw std::range_error("Rowmask dim must match game grid nrows");
        }
        rowmask.grid = gr;
    }
    // cols
    arma::mat get_colmask() {
        return colmask.grid;
    }
    void set_colmask(arma::mat gr) {
        if ( !gr.is_square() ) {
            throw std::range_error("Colmask must be square matrix");
        }
        if ( gr.n_rows != grid.n_rows ) {
            throw std::range_error("Colmask dim must match game grid ncol");
        }
        colmask.grid = gr;
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
