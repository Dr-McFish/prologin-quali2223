#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <assert.h>

void afficher_array(int n, int* arr);

//args <nb accroshes> <max> <stab> <p>
int main(int argc, char* argv[]) {
	assert(argc == 5);
	int nb_accros = atoi(argv[1]);
	int lim = atoi(argv[2]);
	int nstabs = atoi(argv[3]);
	int p = atoi(argv[4]);

	srand(time(NULL));

	int* accroches = malloc(nb_accros*sizeof(int));

	for (int i = 0; i < nb_accros; i++)
	{
		accroches[i] = rand() % lim;
	}

	printf("%d\n", nb_accros);
	printf("%d\n", nstabs);
	printf("%d\n", p);
	afficher_array(nb_accros, accroches);
}

void afficher_array(int n, int* arr) {
	for (int i = 0; i < n; i++)
	{
		printf("%d", arr[i]);
		if(i != n-1) printf(" ");
	}
	printf("\n");	
}
