#include <stdlib.h>
#include <time.h>


void sample_bernoulli_matrix(double** matrix, int* nrow, int* ncol){
	
	unsigned int seed = time(NULL);
	
	for(int i=0; i<nrow[0]; i++){
		for(int j=0; j<ncol[1]; j++){

			if(rand_r(&seed) < RAND_MAX * matrix[i][j]){
				matrix[i][j] = 0;
			}else{
				matrix[i][j] = 1;
			}
		}
	}
}
