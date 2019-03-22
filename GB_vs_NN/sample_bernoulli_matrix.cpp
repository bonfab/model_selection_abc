
#include "Rcpp.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector bernoulli_matrix(NumericMatrix prob_matrix){

	int nrow = prob_matrix.nrow();
	int ncol = prob_matrix.ncol();
	int var_sum = 0;	

	int val, sum;
	double mean, variance;

	NumericVector variance_flags(ncol);

	for(int j=0; j<ncol; j++){

		sum = 0;
			
		for(int i=0; i<nrow; i++){
			
			val = ::unif_rand() < prob_matrix(i,j);
			
			prob_matrix(i, j) = val;
			
			sum += val;
			
		}

		if(sum != 0 && sum != nrow){
			
			mean = (double) sum/nrow;
		
			variance = ((mean*mean)*(nrow - sum) + (1 - mean)*(1-mean)*sum)/nrow;

			for(int i=0; i<nrow; i++) prob_matrix(i, j) = (prob_matrix(i, j) - mean) / variance;
			
			variance_flags[j] = 1;
			var_sum += 1;

		}	
	}

	NumericMatrix clean(nrow, var_sum);
	int old_index = 0;

	for(int j=0; j<var_sum; j++){

		while(!variance_flags[old_index]) old_index += 1;

		for(int i=0; i<nrow; i++) clean(i,j) = prob_matrix(i, old_index);

		old_index += 1;
	}

	return(clean);
}
