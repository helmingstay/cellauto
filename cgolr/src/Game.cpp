#include <RcppArmadillo.h>
#include <bitset>

#define MAXBITS 64 
typedef std::bitset<MAXBITS> type_bitset;

using arma::fill::zeros;

// = int
typedef arma::uvec::elem_type & uvec_elem;
typedef std::vector<arma::uvec> vec_uvec;

// functor to initialize bitsets 
// from arma::uvec lives_at / born_at
class init_bitset {
public:
    init_bitset(type_bitset & _setref) : setref(_setref){}
    //member
    type_bitset & setref;
    void operator()(uvec_elem ii) {
        setref.set(ii);
    }
};

// master class - grid, advancement rules
class cgolr {                     
public:                         
    // ctor - initialize to zero things of known dim
    cgolr(
        int nrow, int ncol
    ) : 
        grow(1.0), decay(1.0),
        grid(nrow, ncol, zeros),
        alive(nrow, ncol, zeros),
        lives_at(), born_at(),
        neighbor(nrow, ncol, zeros),
        ready(false)
    { };
    // publicly accessible members
    // play with at will
    double decay;
    double grow;
    // game grid, double (for decay)
    arma::mat grid;
    // counts of living neighbors
    arma::umat neighbor;
    // currently alive
    arma::umat alive;
    // vector of addresses
    vec_uvec address;

    private:
    // internal member variables
    // fiddly bits
    bool ready;
    type_bitset lives_at;
    type_bitset born_at;
    // neighborhood 
    // must have L/R and U/D symmetric
    // vert and horizontal radius can differ
    int radius_col;
    int radius_row;

    // state transition functions
    // toggle alive-state,
    // update neighbor counts
    // and update the history grid
    template<typename T>
    void birth(T ii) {
        alive(ii) = 1;
        //Rf_PrintValue(Rcpp::wrap(address[ii]));
        neighbor(address[ii]) += 1;
        grid(ii) = 1.0;
    };
    template<typename T>
    void death(T ii) {
        alive(ii) = 0;
        neighbor(address[ii]) -= 1;
        grid(ii) *= (1.0 - decay);
    };
    // called from init_rules
    // fill address, do just once per rule-spec
    // must have first specified rules
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
    // used by grid setter
    // reset all counts on grid edit
    void init_grid() {
        // need rules
        if (!ready) {
            throw(std::runtime_error("Please call init_rules to set up rules."));
        };
        neighbor.zeros();
        //alive.zeros();
        //alive_prev.zeros();
        // seed history
        alive = (grid >= 1.0);
        // recompute neighbors
        arma::uvec start_alive = arma::find(alive>0);
        for(int ii = 0; ii<start_alive.size(); ii++){
            birth(start_alive(ii));
        }
    }

    public:
    // required to run - 
    // specify neighborhood and transition rules
    // TODO?? implement offsets in init_address?
    void init_rules(arma::uvec _lives, arma::uvec _born, int _radius_row, int _radius_col, int _offset_row, int _offset_col) {
        // flip bit positions
        //arma::uvec tmp_lives(_lives_at);
        //arma::uvec tmp_born(_born_at);
        _lives.for_each(init_bitset(lives_at));
        _born.for_each(init_bitset(born_at));
        radius_row = _radius_row;
        radius_col = _radius_col;
        // each element of vector is uvec, filled w/indices of neighborhood
        // neighborhood nrow*ncol-1 (exclude self)
        // fill w/zeros
        address = vec_uvec(grid.n_rows*grid.n_cols, arma::uvec((1+2*_radius_row)*(1+2*_radius_col)-1));
        init_address();
        // toss an extra neighbor recompute for new game
        ready = true;
        init_grid();
    }

    // advance one step
    void step() {
        if (!ready) {
            throw(std::runtime_error("Please call init_rules to set up rules."));
        };
        // state transitions update neighbors
        // transitions only change self alive
        // but toggle other neighbors, so need a copy
        arma::umat tmp_neighbor(neighbor);
        int alive0;
        int neighbor0;
        // run through whole matrix
        for (int ii = 0; ii < alive.size(); ii++) {
            // order is important for alive0
            alive0 = alive(ii);
            neighbor0 = tmp_neighbor(ii);
            // was dead, born
            if ( alive0 == 0 && born_at.test(neighbor0) ) {
                birth(ii);
            } else if ( alive0 == 1 && !(lives_at.test(neighbor0))) {
                // was alive, dies
                death(ii);
            } else if ( alive0 == 0 && grid(ii)>0) {
                grid(ii) *= (1.0 - decay);
            }
        }
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
//private:
};

RCPP_MODULE(GameEx){
    using namespace Rcpp ;
    class_<cgolr>( "cgolr" )
    .constructor<int,int>("Set up new game: nrow (int); ncol (int)")
    .method("init_rules", &cgolr::init_rules, "Must run this before step: IntVec lives_at, IntVec born_at, mask radius: (row int, col int); mask offset: (row int, col int)")
    .method("step", &cgolr::step, "Advance 1 step (must run init_rules first")
    //
    // expose for debugging
    .field("alive", &cgolr::alive, "umat, currently alive")
    .field("neighbor", &cgolr::neighbor, "umat, number of neighbors")
    // user-accessible
    //.field("lives_at", &cgolr::lives_at, "vector")
    //.field("born_at", &cgolr::born_at, "vector")
    .field("decay", &cgolr::decay, "Numeric: per-time rate of decay of dead cells (1=instant death, 0=immortal)")
    .field("grow", &cgolr::grow, "Numeric: pert-time rate of growth of live cells")

    // read and write property
    .property( "grid", &cgolr::get_grid, &cgolr::set_grid)

    // read-only property
    //.property( "y", &Num::getY )
    ;
}
