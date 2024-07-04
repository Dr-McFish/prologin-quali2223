#include <stdio.h>

/// \param p Prix de la commande
/// \param m Montant payé par le client
void volvian(int p, int m) {
    /* TODO Afficher le nombre minimal de pièces rendues au client. */
	int billets = 0;
	int owed_money = m - p;
	while(owed_money >= 200) {
		billets++;
		owed_money -= 200;
	}
	while(owed_money >= 100) {
		billets++;
		owed_money -= 100;
	}
	while(owed_money >= 50) {
		billets++;
		owed_money -= 50;
	}
	while(owed_money >= 20) {
		billets++;
		owed_money -= 20;
	}
	while(owed_money >= 10) {
		billets++;
		owed_money -= 10;
	}
	while(owed_money >= 5) {
		billets++;
		owed_money -= 5;
	}
	while(owed_money >= 2) {
		billets++;
		owed_money -= 2;
	}
	while(owed_money >= 1) {
		billets++;
		owed_money -= 1;
	}

	printf("%d", billets);
}

int main() {
    int p;
    scanf("%d", &p);
    int m;
    scanf("%d", &m);
    volvian(p, m);

    return 0;
}
