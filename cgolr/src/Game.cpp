//

#include <RcppArmadillo.h>

using arma::fill::zeros;

class cgolr {                     // 
public:                         // which have a getter/setter and getter
    cgolr(int nrow, int ncol, int _diam) : 
        diam(_diam),
        grid(nrow, ncol, zeros),
        rowmask(nrow, nrow, zeros),
        colmask(ncol, ncol, zeros)
    {
        make_mask(nrow, rowmask);
        make_mask(ncol, colmask);
    } ;

    int diam;
    int lives_at;
    int born_at;
    double decay;
    // game grid
    arma::mat grid;
    arma::mat rowmask;
    arma::mat colmask;

private:
    void make_mask(int nn, arma::mat &mask) {
        // row mask
        for(int ii = 0; ii < nn; ii++) {
            for(int jj = 0; jj < nn; jj++) {
                if ( ( ( ii + jj ) % nn < diam ) ) {
                    mask(ii,jj) = 1;
                }
            }
        }
    }
};

RCPP_MODULE(GameEx){
    using namespace Rcpp ;

    class_<cgolr>( "cgolr" )

    .constructor<int,int,int>("Set up new game (int, int, int): nrow, ncol, diam of neighborhood")

    .field_readonly("rowmask", &cgolr::rowmask, "")
    .field_readonly("colmask", &cgolr::colmask, "")
    // read and write property
    //.property( "x", &Num::getX, &Num::setX )

    // read-only property
    //.property( "y", &Num::getY )
    ;
}
