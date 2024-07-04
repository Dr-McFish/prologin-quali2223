#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>


void afficher_redir(int n, int* n_redirection);
int trajet_retour(int n, int* redirection, int idx_debut, bool* deja_visite, int* n_redir_apartir);

/// \param n le nombre de cinémas
/// \param redirection le lieu de redirection de chaque cinéma
void trajets_retour(int n, int* redirection) {
    /* TODO Afficher, sur une ligne et séparé par une espace, le nombre de
    redirections nécessaires en partant de chaque cinéma avant de retomber à
    nouveau sur un cinéma déjà visité.  */
	int* n_redir_apartir = (int*)calloc(n, sizeof(int));
	bool* deja_visite = (bool*)malloc(sizeof(bool)*(n+1));

	for(int i = 0; i < n; i++) {
		deja_visite[i] = false;
		n_redir_apartir[i] = -1;
	}
	for (int i = 0; i < n; i++) {
		if(n_redir_apartir[i] != -1)
			continue;; //skip, allready done
		
		for(int j = 0; j < n; j++) {
			deja_visite[j] = false;
		}

		int iloop = i;
		int count = 0;
		while (!deja_visite[iloop]) {
			//printf("a");
			deja_visite[iloop] = true;
			iloop = redirection[iloop] -1;
			if (n_redir_apartir[iloop] != -2){
				count += n_redir_apartir[iloop];
			}
			
			count++; //numbers go up!
		}
		
		int idx = i;
		if (iloop == idx) {
			n_redir_apartir[idx] = count;

		}
		while (iloop != idx) {
			deja_visite[idx] = false;
			//printf("b");
			n_redir_apartir[idx] = count;
			idx = redirection[idx] -1;
			count--;
		}
		

		do {
			//printf("c");
			deja_visite[idx] = false;
			idx = redirection[idx] -1;
			n_redir_apartir[idx] = count;
		} while (iloop != idx);
	}
	afficher_redir(n, n_redir_apartir);
	free(n_redir_apartir);
	free(deja_visite);
}

void afficher_redir(int n, int* n_redirection) {
	for (int i = 0; i < n; i++)
	{
		printf("%d", n_redirection[i]);
		if(i != n-1) printf(" ");
	}
	printf("\n");	
}

int main() {
    int n;
    scanf("%d", &n);
    int* redirection = (int*)malloc(n * sizeof(int));
    for (int i = 0; i < n; ++i)
        scanf("%d", &redirection[i]);
    trajets_retour(n, redirection);

    return 0;
}
