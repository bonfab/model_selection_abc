#include <stdlib.h>
#include <time.h>
#include "Rcpp.h"

using namespace Rcpp;

void sample_bernoulli_matrix(NumericMatrix Am){

	unsigned int seed = time(NULL);
	
	for(int i=0; i<nrow[0] + ncol[0]; i++){
		for(int j=0; j<ncol[0]; j++){

			if(rand_r(&seed) < RAND_MAX * matrix[i][j]) matrix[i][j] = 0;
			else matrix[i][j] = 1;
		}
	}
}
