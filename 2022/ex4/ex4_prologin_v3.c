#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

void afficher_array(int n, int* arr);
int next(int idx, int* redir);

/// \param n le nombre de cinémas
/// \param redirection le lieu de redirection de chaque cinéma
void trajets_retour(int n, int* redirection) {
    int* nombre_rdir = (int*)calloc(n, sizeof(int));
	bool* already_vis = (bool*)malloc(n * sizeof(bool));

	for (int i = 0; i < n; i++) {
		already_vis[i] = false;
	}

	for(int start = 0; start < n; start++) {
		// calc value of nombre_rdir[start]
		if(nombre_rdir[start] != 0)
			continue;
		
		//for (int i = 0; i < n; i++) {
		//	already_vis[i] = false;
		//}
		
		int count = 0;
		int pos = start;
		while (!already_vis[pos]) {
			already_vis[pos] = true;
			count++;
			pos = next(pos, redirection);
		}
		if(nombre_rdir[pos] != 0) {
			/* precalc tail case */
			int count2 = count + nombre_rdir[pos];
			int pos2 = start;

			for (int i = 0; i < count; i++) {
				//already_vis[pos2] = false;
				nombre_rdir[pos2] = count2;
				count2--;
				pos2 = next(pos2, redirection);
			} 
		} else {
			/* loop case */
			int loop_start_pos = start;
			while (pos != loop_start_pos)
			{
				//already_vis[loop_start_pos] = false;
				nombre_rdir[loop_start_pos] = count;
				count--;
				loop_start_pos = next(loop_start_pos, redirection);
			}
			int pos2 = loop_start_pos;
			for (int i = 0; i < count; i++)
			{
				//already_vis[pos2] = false;
				nombre_rdir[pos2] = count;
				pos2 = next(pos2, redirection);
			} 
		}
	}

	
	afficher_array(n, nombre_rdir);
	free(nombre_rdir);
	free(already_vis);
}

void afficher_array(int n, int* arr) {
	for (int i = 0; i < n; i++)
	{
		printf("%d", arr[i]);
		if(i != n-1) printf(" ");
	}
	printf("\n");	
}

int next(int idx, int* redir) {
	return redir[idx];
}

int main() {
    int n;
    scanf("%d", &n);
    int* redirection = (int*)malloc(n * sizeof(int));
    for (int i = 0; i < n; ++i)
        scanf("%d", &redirection[i]);
	
    for (int i = 0; i < n; ++i) {
		redirection[i]--;
	}

    trajets_retour(n, redirection);

    return 0;
}
