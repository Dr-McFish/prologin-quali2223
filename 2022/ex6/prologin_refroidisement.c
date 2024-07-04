#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#define CH_DEBUT 0
#define CH_FIN 1
#define CH_REFROIDISEMENT 2
typedef int int3_t[3];

typedef struct chemins {
	int3_t* tab_chemins;
	int nombre_des_chemins;
} chemins_t;


int qcompare_tuyaux(void* tuyau1_v, void* tuyau2_v);
int max(int a, int b);
void afficher_array(int n, int* arr);


/// \param n Le nombre de points
/// \param m Le nombre de tuyaux
/// \param k Le nombre de degrés minimum de refroidissement
/// \param a Le point de départ
/// \param b Le point d'arrivée
/// \param tuyaux Les tuyaux orientés (point de départ, point d'arrivée, refroidissement)
int refroidissement(int n, int m, int k, int a, int b, int** tuyaux) {
	
	// -------------------------------- Pre-pre traitement ---------------------------------
	const int vrtx_debut = a -1;
	const int vrtx_fin   = b -1;
	for (int i = 0; i < m; i++)
	{
		//printf("patate ");
		tuyaux[i][CH_DEBUT]--;
		tuyaux[i][CH_FIN]--;
		//tuyaux[i][CH_FIN]--;
	}
	
	// ---------------------------------- Pre traitement ----------------------------------
	chemins_t* chemins_apartir_de = malloc((n+1)* sizeof(chemins_t));
	for (int i = 0; i < n;i++) {
		chemins_apartir_de[i].nombre_des_chemins = 0;
	}
	for (int j = 0; j < m; j++) {
		// TODO : Delete printf("%d, %d\n", tuyaux[j][CH_DEBUT], n);
		chemins_apartir_de[tuyaux[j][CH_DEBUT]].nombre_des_chemins++;
	}

	for (int i = 0; i < n;i++) {
		chemins_apartir_de[i].tab_chemins = 
			(int3_t*)calloc(chemins_apartir_de[i].nombre_des_chemins, 
							sizeof(int) * 3);
		//printf("%d, %d\n", chemins_apartir_de[i].nombre_des_chemins, -1);
		chemins_apartir_de[i].nombre_des_chemins = 0;
	}
	for (int j = 0; j < m; j++) {
		int nombre_chs = chemins_apartir_de[tuyaux[j][CH_DEBUT]].nombre_des_chemins;
		//printf("aa %d, %d\n", tuyaux[j][CH_DEBUT], n);
		//printf("bb %d, %d\n", nombre_chs, 0);
		chemins_apartir_de[tuyaux[j][CH_DEBUT]].tab_chemins[nombre_chs][CH_DEBUT] = tuyaux[j][CH_DEBUT];
		chemins_apartir_de[tuyaux[j][CH_DEBUT]].tab_chemins[nombre_chs][CH_FIN] = tuyaux[j][CH_FIN];
		chemins_apartir_de[tuyaux[j][CH_DEBUT]].tab_chemins[nombre_chs][CH_REFROIDISEMENT] = tuyaux[j][CH_REFROIDISEMENT];
		chemins_apartir_de[tuyaux[j][CH_DEBUT]].nombre_des_chemins++;
	}
	//*


	// ---------------------------------- Breadth first search ----------------------------------
#define PAS_ACCSESIBLE -1
	int* degre_max_pour_lde_chemin = (int*)malloc(n * sizeof(int));
	int* new_degre_max_pour_lde_chemin = (int*)malloc(n * sizeof(int));
	for (int i = 0; i < n; i++)
		degre_max_pour_lde_chemin[i] = PAS_ACCSESIBLE; // a longeur 0
	degre_max_pour_lde_chemin[vrtx_debut] = 0;// selement point de depart acceible

	int longeur_chemin = 0;

	bool cas_darret = false;
	//degre de pt arive est inferier que attentdue
	while(degre_max_pour_lde_chemin[vrtx_fin] < k && !cas_darret) {
		// afficher_array(n, degre_max_pour_lde_chemin);
		// printf("\n ");
		// TODO calul le nivou siuvant
		int* new_deg_max_pour_lchem = (int*)malloc(n * sizeof(int));
		for (int i = 0; i < n; i++)
			new_deg_max_pour_lchem[i] = PAS_ACCSESIBLE;
		
		bool tous_inacsesible = true;

		for (int vertex = 0; vertex < n; vertex++){
			if(chemins_apartir_de[vertex].nombre_des_chemins == 0 
				|| degre_max_pour_lde_chemin[vertex] == PAS_ACCSESIBLE) {
				continue;
			} else {
				assert(degre_max_pour_lde_chemin[vertex] != PAS_ACCSESIBLE);
				for (int chm_posible = 0; chm_posible < chemins_apartir_de[vertex].nombre_des_chemins; chm_posible++) {
					int next_vertex = chemins_apartir_de[vertex].tab_chemins[chm_posible][CH_FIN];
					//printf("%d ", next_vertex);
					int refroid = chemins_apartir_de[vertex].tab_chemins[chm_posible][CH_REFROIDISEMENT];
					new_deg_max_pour_lchem[next_vertex] =
						max(new_deg_max_pour_lchem[next_vertex],
							degre_max_pour_lde_chemin[vertex] + refroid);
				}	
				//printf("\n");

				tous_inacsesible = false;
			}
		}

		if(tous_inacsesible)
			cas_darret = true;

		free(degre_max_pour_lde_chemin); // TODO optimisation avec 2 buffers 
		degre_max_pour_lde_chemin = new_deg_max_pour_lchem;

		longeur_chemin++;
	}

	int resultat;

	if(degre_max_pour_lde_chemin[vrtx_fin] == PAS_ACCSESIBLE) {
		resultat = -1;
	} else if(degre_max_pour_lde_chemin[vrtx_fin] < k) {
		resultat = -1;
	} else
		resultat = longeur_chemin;

	// ------------------------------------- liberation Memoire -------------------------------------
	for (int i = 0; i < n; i++) {
		free(chemins_apartir_de[i].tab_chemins);
	}
	free(chemins_apartir_de);

	return resultat;
}

int main() {
	int n;
	scanf("%d", &n);
	int m;
	scanf("%d", &m);
	int k;
	scanf("%d", &k);
	int a;
	scanf("%d", &a);
	int b;
	scanf("%d", &b);
	int** tuyaux = (int**)malloc(m * sizeof(int*));
	for (int i = 0; i < m; ++i) {
		tuyaux[i] = (int*)malloc(3 * sizeof(int));
		for (int j = 0; j < 3; ++j)
			scanf("%d", &tuyaux[i][j]);
	}
	int longeur_chemin = refroidissement(n, m, k, a, b, tuyaux);

	printf("%d\n", longeur_chemin);

	for (int i = 0; i < m; ++i) {
		free(tuyaux[i]);
	}
	free(tuyaux);
	return 0;
}

int qcompare_tuyaux(void* tuyau1_v, void* tuyau2_v) {
	int* tuyau1 = *((int**)tuyau1_v);
	int* tuyau2 = *((int**)tuyau2_v);

	return (tuyau1[0] > tuyau2[0]) - (tuyau1[0] < tuyau2[0]);
}

int max(int a, int b) {
	return a > b ? a : b;
}

void afficher_array(int n, int* arr) {
	for (int i = 0; i < n; i++)
	{
		printf("%d", arr[i]);
		if(i != n-1) printf(" ");
	}
	printf("\n");	
}
