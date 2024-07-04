
#include <stdio.h>
#include <stdlib.h>

void mesort(int tab[], int taille);
// ------------------------------------ algo ------------------------------------

/// \param n Le nombre de boîtes et de restes
/// \param restes Liste des volumes des restes
/// \param boites Liste des volumes des boîtes
void mise_en_boite(int n, int* restes, int* boites) {
    /* TODO Afficher sur une ligne le nombre maximum de restes que l'on peut
    mettre en boîte. */
	int ri, bi;
	int res = 0;

	mesort(restes, n);
	mesort(boites, n);

	for (ri = bi = 0; ri < n && bi < n;) {
		if (restes[ri] <= boites[bi]) {
			res++;
			ri++;
			bi++;
		} else {
			bi++;
		}
	}
	
	printf("%d", res);
}

// ---------------------------------- mergesort ----------------------------------
void merge(int a1[], int a2[], int l, int m, int r) {
	int i = l, j = m;
	for(int k = l; k < r; k++) {
		if(i<m && (j ==r || a1[i] <= a1[j])) {
			a2[k] = a1[i];
			i++;
		} else {
			a2[k] = a1[j];
			j++;
		}
	}
}

void mergesort_rec(int a[], int tmp[], int l, int r){
	if (r -l <= 1) return;
	int m = l + (r - l) / 2;
	mergesort_rec(a, tmp, l, m);
	mergesort_rec(a, tmp, m, r);
	if (a[m-1] <= a[m]) return;
	for(int i = l; i < r; i++) tmp[i] = a[i];
	merge(tmp, a, l, m, r);
}

void mesort(int tab[], int taille){
	int *tmp = (int*)malloc(taille * sizeof(int));
	mergesort_rec(tab, tmp, 0, taille);
	free(tmp);
}

// ------------------------------------ main -------------------------------------

int main() {
    int n;
    scanf("%d", &n);
    int* restes = (int*)malloc(n * sizeof(int));
    for (int i = 0; i < n; ++i)
        scanf("%d", &restes[i]);
    int* boites = (int*)malloc(n * sizeof(int));
    for (int i = 0; i < n; ++i)
        scanf("%d", &boites[i]);
    mise_en_boite(n, restes, boites);

    return 0;
}
