#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}

// [[Rcpp::export]]
NumericVector workerFEupdate(List l) {
  NumericVector yr     = as<NumericVector>(l["yr"]);
  IntegerVector tin    = as<IntegerVector>(l["tin"]);
  NumericVector res    = as<NumericVector>(l["res"]);
  NumericVector count  = as<NumericVector>(l["count"]);
  // counter to go through
  int i=0;
  double m=0;
  double n=0;
  double mean=0;
  int tin_cur=-1;
  int tin_cur_start=0;
  for (i=0;i<yr.size();i++) {
    if (tin_cur != tin(i)) {
      //store the updated means
      mean = m/n;
      for (int i2=tin_cur_start;i2<i;i2++) res(i2) = mean;
      // reset for current worker
      m=0;
      n=0;
      tin_cur = tin(i);
      tin_cur_start=i;
    }
    
    // current i is always for cur_tin at this point
    m+=yr(i)*count(i);
    n+=count(i);
  }
  // store the last guy!
  mean = m/n;
  for (int i2=tin_cur_start;i2<i;i2++) res(i2) = mean;
  
  return res;
}

// [[Rcpp::export]] 
NumericVector firmFEupdate(List l) {
  NumericVector yr            = as<NumericVector>(l["yr"]);
  IntegerVector payer_tin     = as<IntegerVector>(l["payer_tin"]);
  NumericVector res           = as<NumericVector>(l["res"]);
  NumericVector weight        = as<NumericVector>(l["weight"]);
  NumericVector fval          = as<NumericVector>(l["fval"]);
  NumericVector fwei          = as<NumericVector>(l["fwei"]);
  
  // counter to go through
  int i=0;
  int payer_tinx = 0;
  //Rprintf("size of data: %i\\n",yr.size());
  //Rprintf("number of firms: %i\\n",fval.size());
  // compute the numerator and denomitor for each firm
  for (i=0;i<yr.size();i++) {
    payer_tinx = payer_tin(i);
    fval(payer_tinx) += yr(i)*weight(i);
    fwei(payer_tinx) += weight(i);
  }
  // compute the mean within firm
  for (int fi=0;fi<fval.size();fi++) {
    fval(fi) = fval(fi)/fwei(fi);
  }
  // save the mean in res
  for (i=0;i<yr.size();i++) {
    res(i) = fval(payer_tin(i));
  }
  
  return res;
}

