#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

int64_t max(int64_t a, int64_t b);
int64_t min(int64_t a, int64_t b);
int int_cmp(const void* a, const void* b);
int64_t fn_stabilite(int64_t a, int64_t b, int64_t c, int64_t d, int p);
int64_t fn_stab(int64_t a, int64_t a_plus4, int p);
int64_t* fn_stab_liste(int n_acroches, int64_t* accroches, int p);
int stab_max_rec2(int n_stabilités, int k_nstab, int p_idice_stab, int64_t* stabilités);
int stab_max_rec(int n_acroches, int k_nstab, int p_idice_stab, int64_t* accroches, int st);

void afficher_array(int n, int64_t* arr);

int64_t dbg_accroches[16] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

/// \param n_acroches nombre d'accroches
/// \param k_nstab nombre de stabilisateurs
/// \param p_idice_stab indice de stabilité parfaite
/// \param accroches hauteur de chaque accroche
void stabilite_maximale(int n_acroches, int k_nstab, int p_idice_stab, int64_t* accroches) {

	// on peut pas placer plus de stabilisatioer que il y a place
	// càd |_ n/4 _|
	k_nstab = min(k_nstab, n_acroches/4);
	if(k_nstab == 0) {
		printf("%d\n", 0);
		return;
	}
	//tri des accroches
	qsort(accroches, n_acroches, sizeof(int64_t), int_cmp);
	
	int64_t* stabilités = fn_stab_liste(n_acroches, accroches, p_idice_stab);
	
	//int res = stab_max_rec(n_acroches, k_nstab, p_idice_stab, accroches, 0);
	int res = stab_max_rec2(n_acroches - 3, k_nstab, p_idice_stab, stabilités);
	

	printf("%d\n", res);
}

int main() {
	int n_acroches;
	scanf("%d", &n_acroches);
	int k_nstab;
	scanf("%d", &k_nstab);
	int p_idice_stab;
	scanf("%d", &p_idice_stab);
	int64_t* accroches = (int64_t*)malloc(n_acroches * sizeof(int64_t));
	for (int i = 0; i < n_acroches; ++i)
		scanf("%lld", &accroches[i]);
	stabilite_maximale(n_acroches, k_nstab, p_idice_stab, accroches);

	return 0;
}

int64_t max(int64_t a, int64_t b) {
	return a > b ? a : b;
}

int64_t min(int64_t a, int64_t b) {
	return a < b ? a : b;
}

int int_cmp(const void* a, const void* b) {
	int aa = *((const int64_t*)a);
	int bb = *((const int64_t*)b);
	
	return (aa > bb) - (aa < bb);;
}

int64_t fn_stabilite(int64_t a, int64_t b, int64_t c, int64_t d, int p) {
	const int64_t l_p = (int64_t)p;

	int64_t mx = max(max(a, b), max(c, d));
	int64_t mn = min(min(a, b), min(c, d));
	
	int64_t desequilibre = (mx - mn)*(mx - mn);

	return l_p - desequilibre;
}

int64_t fn_stab(int64_t a, int64_t a_plus3, int p) {
	const int64_t l_p = (int64_t)p;
	int64_t desequilibre = (a - a_plus3)*(a - a_plus3);

	return l_p - desequilibre;
}


int stab_max_rec(int n_acroches, int k_nstab, int p_idice_stab, int64_t* accroches, int st) {
	if(k_nstab == 0 || n_acroches < 4)
		return 0;
	else {
		int stab_max = 0;
		int stab_max_idx = 0;
		for (int i = 0; i < n_acroches-3; i++)
		{
			int64_t stab_interne = fn_stabilite(accroches[i], accroches[i+1], accroches[i+2], accroches[i+3], p_idice_stab);
			if(stab_interne <= 0)
				continue;
			int64_t stab_totale = stab_interne + stab_max_rec(n_acroches-i-4, k_nstab-1, p_idice_stab, accroches+4+i, i + st);

			if(stab_max < stab_totale) {
				stab_max = stab_totale;
				stab_max_idx = i;
			}
		}
		
		//dbg_accroches[k_nstab-1] = accroches[stab_max_idx];
		dbg_accroches[k_nstab-1] = stab_max_idx + st;
		return stab_max;
	}
}

int64_t* fn_stab_liste(int n_acroches, int64_t* accroches, int p) {
	int64_t* rt = (int64_t*)malloc(sizeof(int64_t)*n_acroches-3);
	for (int i = 0; i < n_acroches-3; i++) {
		rt[i] = fn_stab(accroches[i], accroches[i+3], p);
	}
	return rt;
}

void afficher_array(int n, int64_t* arr) {
	for (int i = 0; i < n; i++)
	{
		printf("%lld", arr[i]);
		if(i != n-1) printf(" ");
	}
	printf("\n");	
}

int stab_max_rec2(int n_stabilités, int k_nstab, int p_idice_stab, int64_t* stabilités) {
	if(k_nstab == 0 || n_stabilités == 0)
		return 0;
	else {
		int stab_max = 0;
		int stab_max_idx = 0;
		for (int i = 0; i < n_stabilités; i++)
		{
			const int64_t stab_interne = stabilités[i];
			if(stab_interne <= 0)
				continue;
			const int64_t stab_totale = stab_interne + stab_max_rec2(n_stabilités-i, k_nstab-1, p_idice_stab, stabilités+4+i);

			if(stab_max < stab_totale) {
				stab_max = stab_totale;
				stab_max_idx = i;
			}
		}
		
		return stab_max;
	}
}
