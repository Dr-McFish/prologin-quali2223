#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

#define MAX_WORD 1001

bool is_palindrome(char str[], int taille);
bool is_pas_malindrome(char str[], int taille);

/// \param n Le nombre de mots de passe contenus dans le fichier de mots de passe de Raphaël
/// \param mots La liste des mots de passe à décoder
void nb_pas_malin_drome(int n, char** mots) {
	int res = 0;

	for (int i = 0; i < n; i++)
	{
		if(is_pas_malindrome(mots[i], strlen(mots[i]) ))
			res++;
	}
	
	printf("%d\n", res);
}

bool is_palindrome(char str[], int taille) {
	for (int i = 0; i < ((taille/2)); i++) {
		if (str[i] != str[taille - 1 - i]) {
			return false;
		}
	}

	return true;
}


bool is_pas_malindrome(char str[], int taille) {
	char chif[MAX_WORD];
	char minu[MAX_WORD];
	char maju[MAX_WORD];
	int nchif, nminu, nmaju;

	//diviser le mot en son partie majuscul/minuscul/numerique
	nchif = nminu = nmaju = 0;
	for (int i = 0; i < taille; i++) {
		if(isdigit(str[i])) {
			chif[nchif] = str[i];
			nchif++;
		} else if(islower(str[i])) {
			minu[nminu] = str[i];
			nminu++;
		} else if(isupper(str[i])) {
			maju[nmaju] = str[i];
			nmaju++;
		}
	}

	// chaque partie est un palindrome
	return(is_palindrome(chif, nchif) 
		&& is_palindrome(minu, nminu)
		&& is_palindrome(maju, nmaju));
}

int main() {
	int n;
	scanf("%d", &n);
	getchar(); // \n
	char** mots = (char**)malloc(n * sizeof(char*));
	for (int i = 0; i < n; ++i) {
		mots[i] = (char*)malloc((1000 + 1) * sizeof(char));
		scanf("%[^\n]", mots[i]);
		getchar(); // \n
	}
	nb_pas_malin_drome(n, mots);

	return 0;
}
