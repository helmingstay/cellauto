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
    //bool right_hand;
};


// = int
typedef arma::uvec::elem_type & uvec_elem;
// functor for vec.for_each
// when a cell transitions, 
// update alive grid neighbor counts
// address(ii) is uvec index of neighbors of ii
class update_fun {
public:
    update_fun(arma::umat & _live, arma::umat & _neigh, std::vector<arma::uvec> & _addr, int _to, int _neigh_plus) : 
        live(_live), neigh(_neigh), addr(_addr), to(_to), neigh_plus(_neigh_plus)
    {}
    arma::umat & live;
    arma::umat & neigh;
    std::vector<arma::uvec> & addr;
    int to;
    int neigh_plus;
    void operator()(uvec_elem ii) {
        //Rf_PrintValue(Rcpp::wrap(addr[ii]));
        live(ii) = to;
        neigh(addr[ii]) += neigh_plus;
    }
};

// master class - grid, mask, and advancement rules
class cgolr {                     // 
public:                         // which have a getter/setter and getter
    cgolr(
        int nrow, int ncol, 
        int _radius_row, int _radius_col,
        int offset_row, int offset_col
    ) : 
        grow(1.0), decay(1.0),
        radius_row(_radius_row),
        radius_col(_radius_col),
        grid(nrow, ncol, zeros),
        neighbor(nrow, ncol, zeros),
        // each element of vector is uvec, filled w/indices of neighborhood
        // neighborhood nrow*ncol-1 (exclude self)
        address(nrow*ncol, arma::zeros<arma::uvec>((1+2*_radius_row)*(1+2*_radius_col)-1)),
        alive(nrow, ncol, zeros),
        //(arma::mat & _live, arma::umat & _neigh, arma::vec _addr, int to, int _neigh_plus) : 
        birth(alive, neighbor, address, 1, 1),
        death(alive, neighbor, address, 0, -1),
        // fix mask to take radius
        rowmask(nrow, 2*_radius_row+1, offset_row),
        colmask(ncol, 2*_radius_col+1, offset_col) 
    {
        init_address();
    };
    arma::uvec lives_at;
    arma::uvec born_at;
    double decay;
    double grow;
    // neighborhood 
    // must have L/R and U/D symmetric
    // vert and horizontal radius can differ
    int radius_col;
    int radius_row;
    // game grid, double (for decay)
    arma::mat grid;
    // counts of living neighbors
    arma::umat neighbor;
    // currently alive
    arma::umat alive;
    arma::umat alive_prev;
    // functors for mat.for_each
    update_fun birth;
    update_fun death;
    // index vectors from find
    // records state transitions
    arma::uvec index_birth;
    arma::uvec index_death;
    // vector of addresses
    std::vector<arma::uvec> address;
    mask rowmask;
    mask colmask;


    // fill address, do just once
     void init_address() {
        // index of address
        int counter;
        // foe each address
        for (int ii = 0; ii<address.size(); ii++) {
            // ref to current address to fill
            arma::uvec & this_addr = address[ii];
            counter = 0;
            // loop over neighborhood rows/cols  
            for (int irow = 0-radius_row; irow <= radius_row; irow++) {
                for (int icol = 0-radius_col; icol <= radius_col; icol++ ) {
                    // skip self
                    if (irow == 0 & icol == 0) continue;
                    // index of this neighbor 
                    // mod size of grid (wrap boundaries)
                    this_addr(counter) = (ii + irow + (icol*grid.n_rows)) % (grid.size());
                    counter++;
                }
            }
        }
    }

    // reset counts on grid edit??
    void init_grid() {
        neighbor.zeros();
        //alive.zeros();
        //alive_prev.zeros();
        // seed history
        alive = (grid >= 1.0);
        alive_prev.zeros();
        // recompute neighbors
        //
        arma::uvec start_alive = find(alive>0);
        // add up initial neighbors
        start_alive.for_each(birth);
        //init_neighbor();
    }
    
    // deprecated
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
        // init_neighbor();
        // store last state, edit alive 
        alive_prev = alive;
        // order is important 
        // births first
        // find births 
        for (int ii = 0; ii < born_at.size(); ii++) {
            // tag members fulfilling birth condition
            alive(find(neighbor == born_at(ii))).fill(3);
        }
        // record birth (not prev alive)
        index_birth = find(alive + alive_prev == 3);
        //alive(find(alive==3)).ones();
        //alive(index_birth).ones();
        //cleanup immediately after use -- FIXME
        //alive(find(alive==3)).ones();
        // find cells that fulfill keep-living conts
        for (int ii = 0; ii < lives_at.size(); ii++) {
            // tag members fulfilling condition
            alive(find( neighbor == lives_at(ii))).fill(2);
        }
        // Either was not alive prev 
        // or does not fulfill current conditions
        index_death = find(alive + alive_prev <= 2);
        //cleanup immediately after use -- FIXME
        //alive(index_death).ones();
        // 
        //
        alive(find(alive>1)).ones();
        //Rf_PrintValue(Rcpp::wrap(index_birth));
        // State transitions at end
        index_death.for_each(death);
        index_birth.for_each(birth);
        // update states
        // first, update living-in-previous
        //grid *= grow ;
        grid( index_death ) *= (1.0 - decay);
        // births
        grid( index_birth ).ones();
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

    .constructor<int,int,int,int,int,int>("Set up new game: nrow (int); ncol (int); mask rad: (row int, col int); mask offset: (row int, col int)")

    .method("step", &cgolr::step, "Advance 1 step")
    //.field_readonly("rowmask", &cgolr::rowmask, "")
    //.field_readonly("colmask", &cgolr::colmask, "")
    //
    // expose for debugging
    .field("alive", &cgolr::alive, "umat, currently alive")
    .field("neighbor", &cgolr::neighbor, "umat, number of neighbors")
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
