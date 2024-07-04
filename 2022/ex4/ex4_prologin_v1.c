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
	for (int i = 0; i < n; i++) {
		n_redir_apartir[i] = trajet_retour(n, redirection, i, deja_visite, n_redir_apartir);
	}
	afficher_redir(n, n_redir_apartir);
	free(n_redir_apartir);
	free(deja_visite);
}

/* retur le nombre de redirecions pout idx_debut comme debut */
int trajet_retour(int n, int* redirection, int idx_debut, bool* deja_visite, int* n_redir_apartir) {
	for(int i = 0; i < n; i++) {
		deja_visite[i] = false;
	}

	int idx = idx_debut;
	int count = 0;
	while (!deja_visite[idx]) {
		deja_visite[idx] = true;
		//if(n_redir_apartir[idx] != 0) {
		//	return count + n_redir_apartir[idx] -1; // optimization
		//}
		idx = redirection[idx] -1;
		count++; //numbers go up!
	}
	return count;
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
