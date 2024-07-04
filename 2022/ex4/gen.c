#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdbool.h>

#define SIZE 100000

void afficher_redir(int n, int* n_redirection);
void knuth_shuffle(int a[], int n);
bool has_zero_length_loop(int* ar);


int main(int argc, char* arrgv[]){
	srand(time(NULL));

	int ar[SIZE];
	for (int i = 0; i < SIZE; i++)
	{
		ar[i] = SIZE - i;
	}
	
	knuth_shuffle(ar, SIZE);
	knuth_shuffle(ar, SIZE);
	knuth_shuffle(ar, SIZE);
	while (has_zero_length_loop(ar))
	{
		knuth_shuffle(ar, SIZE);
	}

	afficher_redir(SIZE, ar);
}

void afficher_redir(int n, int* n_redirection) {
	for (int i = 0; i < n; i++)
	{
		printf("%d", n_redirection[i]);
		if(i < n-1) printf(" ");
	}
	//printf("\n");	
}

bool has_zero_length_loop(int* ar) {
	for (int i = 0; i < SIZE; i++)
	{
		if(ar[i] == i+1)
			return true;
	}
	return false;
}


void swap(int a[], int i, int j) {
	int tmp = a[i];
	a[i] = a[j];
	a[j] = tmp;
}

void knuth_shuffle(int a[], int n) {
	for(int i =0; i < n; i++)
		swap(a, i, rand() % n);
}
