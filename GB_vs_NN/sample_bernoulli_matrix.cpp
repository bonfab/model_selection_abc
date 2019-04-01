
#include "Rcpp.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector bernoulli_matrix(NumericMatrix prob_matrix){

	int nrow = prob_matrix.nrow();
	int ncol = prob_matrix.ncol();
	int var_sum = 0;	

	int count1, count2, sum;
	double mean, variance, val;

	NumericVector variance_flags(ncol);

	for(int j=0; j<ncol; j++){

		count1 = 0;
		count2 = 0;
			
		for(int i=0; i<nrow; i++){
			
			if(::unif_rand() < prob_matrix(i,j)){
				val = 1.0;
			} else{
				val = 0.0;
			}
			if(::unif_rand() < prob_matrix(i,j)){
				val += 1.0;
			}

			if(val == 1) count1 += 1;

			if(val == 2) count2 += 1;
			
			prob_matrix(i, j) = val;
			
			
		}
	
		sum = count1 + count2 * 2;

		mean = (double) sum/nrow;

		if(sum != 0 && nrow != count1 && nrow != count2){
			
			//variance = ((mean*mean)*(nrow - (count1+count2)) + (1 - mean)*(1-mean)*count1 + (2-mean)*(2-mean)*count2) /nrow;

			for(int i=0; i<nrow; i++) prob_matrix(i, j) = (prob_matrix(i, j) - mean);// / variance;
			
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
