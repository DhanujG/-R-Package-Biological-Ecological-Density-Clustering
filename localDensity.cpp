#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector gaussianLocalDensity(NumericVector weights, NumericVector distance, int nrow, double dc) {
  int size = distance.size();
  NumericVector half(size);
  for (int i = 0; i < size; i++) {
    double combOver = distance[i] / dc;
    double negSq = pow(combOver, 2) * -1;
    half[i] = exp(negSq);
  }     
  int ncol = nrow;
  
  NumericVector result(nrow);
  
  int i = 0;
  for (int col = 0; col < ncol; col++) {
    for (int row = col + 1; row < nrow; row++) {
      double temp = half[i];
      result[row] += temp * (weights[col]);
      result[col] += temp * (weights[row]);
      i++;
    }
  }

  for (int j = 0; j <  nrow; j++){
    result[j] = result[j] + (weights[j] - 1);
  }
  
  return result;
}

// [[Rcpp::export]]
NumericVector nonGaussianLocalDensity(NumericVector weights, int truesize, NumericVector distance, int nrow, double dc) {
  int ncol = nrow;
  NumericVector result(nrow);
  int i = 0;
  for (int col = 0; col < ncol; col++) {
    for (int row = col + 1; row < nrow; row++) {
      if((i % 10000) == 0){
        // if(verbose){
        // Rcout << "index is " << i << " distance under the current index " << distance[i] << std::endl;
        // }
      }
      if(i > distance.size()){
        // Rcout << "Warning: index is larger than the length of the distance vector" << distance[i] << std::endl;
      }
      
      //add in neightbors with weights
      if (distance[i] < dc) {
        result[row] += weights[col];
        result[col] += weights[row];
      } else {
        // do nothing
      }
      i++;
    }
  }

  for (int j = 0; j <  nrow; j++){
    result[j] = result[j] + (weights[j] - 1);
  }
  // if(verbose){
  //  Rcout << "last index is " << i << " length of distance is " << distance.size() << "number of rows is " << nrow << "number of columns is " << ncol << std::endl;
  // }
  return result;
}

// [[Rcpp::export]]
int SumCutOff(NumericVector weights, NumericVector distance, int nrow, double dc) {
  int ncol = nrow;
  int sum = 0;
  int i = 0;
  for (int col = 0; col < ncol; col++) {
    for (int row = col + 1; row < nrow; row++) {
      
      
      //add in neightbors with weights
      if (distance[i] < dc) {
        sum = sum + (weights[col] * weights[row]);
      }
      if (i < (((nrow*(nrow-1))/2)-1)){
        i++;
      }
      
    }
  }
  
  
  // if(verbose){
  //  Rcout << "last index is " << i << " length of distance is " << distance.size() << "number of rows is " << nrow << "number of columns is " << ncol << std::endl;
  // }
  return sum;
}
