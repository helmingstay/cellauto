#include <Rcpp.h>

using Rcpp::IntegerVector;
using Rcpp::LogicalMatrix;
using Rcpp::IntegerMatrix;
using Rcpp::NumericMatrix;
using Rcpp::List;
using Rcpp::as;
using Rcpp::wrap;

// list getter
// take Rcpp proxy, get SEXP, then construct 
template <class Tin, class Tout>
void setter(Tin in, Tout & set) {
    SEXP tmp(in);
    Tout ret(tmp);
    set = ret;
}

struct Mats {
    Mats(List in) {
        setter(in["grid"], grid);
        setter(in["alive"], alive);
        setter(in["neighbor"], neighbor);
        setter(in["neighbor0"], neighbor0);
        setter(in["address"], address);
    }
    NumericMatrix grid;
    LogicalMatrix alive;
    IntegerMatrix neighbor;
    IntegerMatrix neighbor0;
    IntegerMatrix address;
};
