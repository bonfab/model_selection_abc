#include <stdlib.h>
#include <time.h>


void sample_bernoulli_matrix(double* matrix, int* nrow, int* ncol){
	
	unsigned int seed = time(NULL);
	
	for(int i=0; i<nrow[0] + ncol[0]; i++){
		//for(int j=0; j<ncol[0]; j++){

			if(rand_r(&seed) < RAND_MAX * matrix[i]) matrix[i] = 0;
			else matrix[i] = 1;
				
			
		//}
	}
}
