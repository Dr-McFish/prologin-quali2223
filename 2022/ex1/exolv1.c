#include <stdio.h>
#include <stdlib.h>
#include <string.h>


// ----------------------------- fonction  principale ----------------------------- 
void swap(char** a, char** b);

/// \param adore liste des noms du film adoré de chaque personne
/// \param deteste liste des noms du film détesté de chaque personne
void nombre_films(char* adore[], char* deteste[]) {
	/* TODO Afficher, sur une ligne, le nombre de films qui sont uniquement
	adorés. */
	int resultat = 6;
	int t_adore = 6;
	
	// exclude duplicates addore
	for (int i = 0; i < t_adore; i++) {
		for (int j = i+1; j < t_adore; j++) {
			if(strcmp(adore[i], adore[j]) == 0) {
				resultat--;
				t_adore--;
				swap(&adore[j], &adore[t_adore]);
				j--;
			}
		}
	}

	// exclude deteste
	for (int i = 0; i < t_adore; i++) {
		for (int j = 0; j < 6; j++) {
			if(strcmp(adore[i], deteste[j]) == 0) {
				resultat--;
				break;
			}
		}
	}

	printf("%d\n", resultat);
}

// ------------------------------------- other ------------------------------------

// swap values of a and b
void swap(char** a, char** b) {
	char* tmp = *a;
	*a = *b;
	*b = tmp;
}

// ------------------------------------- main ------------------------------------- 

int main() {
    char** adore = (char**)malloc(6 * sizeof(char*));
    for (int i = 0; i < 6; ++i) {
        adore[i] = (char*)malloc((100 + 1) * sizeof(char));
        scanf("%[^\n]", adore[i]);
        getchar(); // \n
    }
    char** deteste = (char**)malloc(6 * sizeof(char*));
    for (int i = 0; i < 6; ++i) {
        deteste[i] = (char*)malloc((100 + 1) * sizeof(char));
        scanf("%[^\n]", deteste[i]);
        getchar(); // \n
    }
    nombre_films(adore, deteste);

    return 0;
}
