#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

int64_t max(int64_t a, int64_t b);
int64_t min(int64_t a, int64_t b);
int int_cmp(const void* a, const void* b);
int64_t fn_stabilite(int64_t a, int64_t b, int64_t c, int64_t d, int p);
int stab_max_rec(int n_acroches, int k_nstab, int p_idice_stab, int64_t* accroches);

/// \param n_acroches nombre d'accroches
/// \param k_nstab nombre de stabilisateurs
/// \param p_idice_stab indice de stabilité parfaite
/// \param accroches hauteur de chaque accroche
void stabilite_maximale(int n_acroches, int k_nstab, int p_idice_stab, int64_t* accroches) {
	
	// on peut pas placer plus de stabilisatioer que il y a place
	// càd |_ n/4 _|
	k_nstab = min(k_nstab, n_acroches/4);
	//tri des accroches
	qsort(accroches, n_acroches, sizeof(int64_t), int_cmp);
	
	int res = stab_max_rec(n_acroches, k_nstab, p_idice_stab, accroches);
	
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


int stab_max_rec(int n_acroches, int k_nstab, int p_idice_stab, int64_t* accroches) {
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
			int64_t stab_totale = stab_interne + stab_max_rec(n_acroches-i-4, k_nstab-1, p_idice_stab, accroches+4+i);

			if(stab_max < stab_totale) {
				stab_max = stab_totale;
				stab_max_idx = i;
			}
		}
		
		return stab_max;
	}
}