#include <stdio.h>
#include <stdlib.h>

#define mod 10007
void citire_matrice(int ****vm, int **vl, int **vc, int *nrcm)
{
	int i, j;
	for (i = 0; i < (*vl)[*nrcm]; i++)
		for (j = 0; j < (*vc)[*nrcm]; j++)
			scanf("%d", &(*vm)[*nrcm][i][j]);
}

void alocare_matreice(int ****vm, int **vl, int **vc, int *nrtotm, int *nrcm)
{
	int n, m;
	int i;
	int ***vmat = (*vm), **mat;
	int *revl = (*vl);
	int *revc = (*vc);
	scanf("%d%d", &n, &m);
	if (*nrcm < *nrtotm) {
		(*vl)[*nrcm] = n;
		(*vc)[*nrcm] = m;
		mat = (int **)calloc(n, sizeof(int *));
		for (i = 0; i < n; i++)
			mat[i] = calloc(m, sizeof(int));
		(*vm)[*nrcm] = mat;
		citire_matrice(&vmat, &revl, &revc, nrcm);  /////////citirea de matrice
		*nrcm += 1;
	} else if (*nrcm >= *nrtotm) {
		vmat = realloc(vmat, 2 * *nrtotm * sizeof(int *));
		revc = realloc(revc, 2 * *nrtotm * sizeof(int *));
		revl = realloc(revl, 2 * *nrtotm * sizeof(int *));
		revl[*nrcm] = n;
		revc[*nrcm] = m;
		mat = (int **)calloc(n, sizeof(int *));
		for (i = 0; i < n; i++)
			mat[i] = calloc(m, sizeof(int));
		(*vc) = revc;
		(*vl) = revl;
		vmat[*nrcm] = mat;
		(*vm) = vmat;
		citire_matrice(&vmat, &revl, &revc, nrcm);  /////////citirea de matrice
		*nrcm += 1;
		*nrtotm = 2 * *nrtotm;
	}
}

void marimile_matricei(int **vl, int **vc, int *nrcm, int mtot)
{
	if (*nrcm >= mtot || *nrcm < 0) {
		printf("No matrix with the given index\n");
		return;
	}
	printf("%d %d\n", (*vl)[*nrcm], (*vc)[*nrcm]);
}

void afisare_matrice(int ****vm, int **vl, int **vc, int *nrcm, int mtot)
{
	int i, j;
	if (*nrcm >= mtot || *nrcm < 0) {
		printf("No matrix with the given index\n");
		return;
	}
	for (i = 0; i < (*vl)[*nrcm]; i++) {
		for (j = 0; j < (*vc)[*nrcm]; j++)
			printf("%d ", (*vm)[*nrcm][i][j]);
		printf("\n");
	}
}

void redimensionare_matrice(int ****vm, int **vl, int **vc, int *nrcm, int mtot)
{
	int newn, newm, i, j;
	scanf("%d", &newn);
	int *indl = calloc(newn, sizeof(int));
	for (i = 0; i < newn; i++)
		scanf("%d", &indl[i]);
	scanf("%d", &newm);
	int *indc = calloc(newm, sizeof(int));
	for (i = 0; i < newm; i++)
		scanf("%d", &indc[i]);
	if (*nrcm >= mtot || *nrcm < 0) {
		printf("No matrix with the given index\n");
		free(indl);
		free(indc);
		return;
	}
	int **aux = (int **)calloc(newn, sizeof(int *));
	for (i = 0; i < newn; i++)
		aux[i] = calloc(newm, sizeof(int));
	for (i = 0; i < newn; i++)
		for (j = 0; j < newm; j++)
			aux[i][j] = (*vm)[*nrcm][indl[i]][indc[j]];
	for (i = 0; i < (*vl)[*nrcm]; i++)
		free((*vm)[*nrcm][i]);
	free((*vm)[*nrcm]);
	(*vm)[*nrcm] = aux;
	(*vl)[*nrcm] = newn;
	(*vc)[*nrcm] = newm;
	free(indl);
	free(indc);
}

void im(int ****vm, int **vl, int **vc, int *nrm1, int *nrm2, int *nt, int *nc)
{
	int i, j, k;
	if ((*vc)[*nrm1] != (*vl)[*nrm2]) {
		printf("Cannot perform matrix multiplication\n");
		return;
	}
	int **mrez = (int **)calloc((*vl)[*nrm1], sizeof(int *));
	for (i = 0; i < (*vl)[*nrm1]; i++)
		mrez[i] = calloc((*vc)[*nrm2], sizeof(int));
	for (i = 0; i < (*vl)[*nrm1]; i++)
		for (j = 0; j < (*vc)[*nrm2]; j++)
			for (k = 0; k < (*vc)[*nrm1]; k++) {
				int ajim = (*vm)[*nrm1][i][k] * (*vm)[*nrm2][k][j];
				mrez[i][j] = (mrez[i][j] + ajim) % mod;
				if (mrez[i][j] < 0)
					mrez[i][j] = mod + mrez[i][j];
			}
	if (*nc < *nt) {
		(*vl)[*nc] = (*vl)[*nrm1];
		(*vc)[*nc] = (*vc)[*nrm2];
		(*vm)[*nc] = mrez;
		*nc += 1;
	} else if (*nc >= *nt) {
		int ***vmat = (*vm);
		int *revc = (*vc);
		int *revl = (*vl);
		vmat = realloc(vmat, 2 * *nt * sizeof(int *));
		revc = realloc(revc, 2 * *nt * sizeof(int *));
		revl = realloc(revl, 2 * *nt * sizeof(int *));
		revl[*nc] = (*vl)[*nrm1];
		revc[*nc] = (*vc)[*nrm2];
		(*vc) = revc;
		(*vl) = revl;
		vmat[*nc] = mrez;
		(*vm) = vmat;
		*nt = 2 * *nt;
		*nc += 1;
	}
}

void sortare_matrice(int ****vm, int **vl, int **vc, int *nrcm)
{
	int i, j, k;
	int *vsume = calloc(*nrcm, sizeof(int));
	for (k = 0; k < *nrcm; k++) {
		vsume[k] = 0;
		for (i = 0; i < (*vl)[k]; i++)
			for (j = 0; j < (*vc)[k]; j++) {
				vsume[k] = (vsume[k] + (*vm)[k][i][j]) % mod;
				if (vsume[k] < 0)
					vsume[k] = mod + vsume[k];
			}
	}
	for (i = 0; i < (*nrcm - 1); i++)
		for (j = (i + 1); j < *nrcm; j++)
			if (vsume[i] > vsume[j]) {
				int aux, **auxm, vlaux, vcaux;
				aux = vsume[i];
				auxm = (*vm)[i];
				vlaux = (*vl)[i];
				vcaux = (*vc)[i];
				vsume[i] = vsume[j];
				(*vm)[i] = (*vm)[j];
				(*vl)[i] = (*vl)[j];
				(*vc)[i] = (*vc)[j];
				vsume[j] = aux;
				(*vm)[j] = auxm;
				(*vl)[j] = vlaux;
				(*vc)[j] = vcaux;
			}
	free(vsume);
}

void transpunere_matrice(int ****vm, int **vl, int **vc, int *nrcm, int mtot)
{
	int i, j;
	if (*nrcm >= mtot || *nrcm < 0) {
		printf("No matrix with the given index\n");
		return;
	}
	int **auxm = (int **)calloc((*vc)[*nrcm], sizeof(int *));
	for (i = 0; i < (*vc)[*nrcm]; i++)
		auxm[i] = (int *)calloc((*vl)[*nrcm], sizeof(int));
	for (i = 0; i < (*vl)[*nrcm]; i++)
		for (j = 0; j < (*vc)[*nrcm]; j++)
			auxm[j][i] = (*vm)[*nrcm][i][j];
	for (i = 0; i < (*vl)[*nrcm]; i++)
		free((*vm)[*nrcm][i]);
	free((*vm)[*nrcm]);
	(*vm)[*nrcm] = auxm;
	int aux = (*vl)[*nrcm];
	(*vl)[*nrcm] = (*vc)[*nrcm];
	(*vc)[*nrcm] = aux;
}

void inmultmataux(int ***mat1, int ***mat2, int ***aux, int masmat)
{
	int i, j, k;
	for (i = 0; i < masmat; i++)
		for (j = 0; j < masmat; j++)
			for (k = 0; k < masmat; k++) {
				int ajim = (*mat1)[i][k] * (*mat2)[k][j];
				(*aux)[i][j] = ((*aux)[i][j] + ajim) % mod;
				if ((*aux)[i][j] < 0)
					(*aux)[i][j] = mod + (*aux)[i][j];
			}
}

void ridicare_putere_mat(int ****vm, int **vl, int **vc, int *nrcm, int mtot)
{
	int i, j, k;
	int putere;
	scanf("%d", &putere);
	if (*nrcm >= mtot || *nrcm < 0) {
		printf("No matrix with the given index\n");
		return;
	}
	if (putere < 0) {
		printf("Power should be positive\n");
		return;
	}
	if ((*vl)[*nrcm] != (*vc)[*nrcm]) {
		printf("Cannot perform matrix multiplication\n");
		return;
	}
	int masmat = (*vl)[*nrcm];
	int **matrez = (int **)calloc((*vl)[*nrcm], sizeof(int *));
	for (i = 0; i < (*vl)[*nrcm]; i++)
		matrez[i] = (int *)calloc((*vc)[*nrcm], sizeof(int));
	for (i = 0; i < masmat; i++)
		for (j = 0; j < masmat; j++)
			if (i == j)
				matrez[i][j] = 1;
			else
				matrez[i][j] = 0;
	int **aux = calloc(masmat, sizeof(int *));
	for (i = 0; i < masmat; i++)
		aux[i] = calloc(masmat, sizeof(int));
	while (putere) {
		if (putere % 2 == 1) {
			inmultmataux(&matrez, &(*vm)[*nrcm], &aux, masmat);
			for (i = 0; i < masmat; i++)
				for (j = 0; j < masmat; j++) {
					matrez[i][j] = aux[i][j];
					aux[i][j] = 0;
				}
		}
		for (i = 0; i < masmat; i++)
			for (j = 0; j < masmat; j++)
				for (k = 0; k < masmat; k++) {
					int ajim = (*vm)[*nrcm][i][k] * (*vm)[*nrcm][k][j];
					aux[i][j] = (aux[i][j] + ajim) % mod;
					if (aux[i][j] < 0)
						aux[i][j] = mod + aux[i][j];
				} // se ridica matricea mea salvatam in vm!!!!
		for (i = 0; i < masmat; i++)
			for (j = 0; j < masmat; j++) {
				(*vm)[*nrcm][i][j] = aux[i][j];
				aux[i][j] = 0;
			}
		putere /= 2;
	}
	for (i = 0; i < masmat; i++)
		free(aux[i]);
	free(aux);
	for (i = 0; i < masmat; i++)
		free((*vm)[*nrcm][i]);
	free((*vm)[*nrcm]);
	(*vm)[*nrcm] = matrez;
}

void stergere_matrice(int ****vm, int **vl, int **vc, int *nrcm, int *nrultm)
{
	int i;
	if (*nrcm >= *nrultm || *nrcm < 0) {
		printf("No matrix with the given index\n");
		return;
	}
	for (i = 0; i < (*vl)[*nrcm]; i++)
		free((*vm)[*nrcm][i]);
	free((*vm)[*nrcm]);
	for (i = *nrcm; i < *nrultm - 1; i++) {
		(*vm)[i] = (*vm)[i + 1];
		(*vl)[i] = (*vl)[i + 1];
		(*vc)[i] = (*vc)[i + 1];
	}
	*nrultm = *nrultm - 1;
}

void dealocare_resurse(int ****vm, int **vl, int **vc, int *nrcm)
{
	int i, k;
	for (k = 0; k < *nrcm; k++) {
		for (i = 0; i < (*vl)[k]; i++)
			free((*vm)[k][i]);
		free((*vm)[k]);
	}
	free((*vm));
	free((*vl));
	free((*vc));
}

// Strassen
void buildm1(int ***aux1, int ***aux2, int ***mat1, int ***mat2, int mas)
{
	int i, j;  // mas == n / 2!!!!!!!!!
	for (i = 0; i < mas; i++)
		for (j = 0; j < mas; j++) {
			(*aux1)[i][j] = ((*mat1)[i][j] + (*mat1)[i + mas][j + mas]) % mod;
			if ((*aux1)[i][j] < 0)
				(*aux1)[i][j] = mod + (*aux1)[i][j];
		}
	for (i = 0; i < mas; i++)
		for (j = 0; j < mas; j++) {
			(*aux2)[i][j] = ((*mat2)[i][j] + (*mat2)[i + mas][j + mas]) % mod;
			if ((*aux2)[i][j] < 0)
				(*aux2)[i][j] = mod + (*aux2)[i][j];
		}
}

void buildm2(int ***aux1, int ***aux2, int ***mat1, int ***mat2, int mas)
{
	int i, j;
	for (i = 0; i < mas; i++)
		for (j = 0; j < mas; j++) {
			(*aux1)[i][j] = 0;
			(*aux2)[i][j] = 0;
		}
	for (i = 0; i < mas; i++)
		for (j = 0; j < mas; j++) {
			int ajad = (*mat1)[i + mas][j] + (*mat1)[i + mas][j + mas];
			(*aux1)[i][j] = ajad % mod;
			if ((*aux1)[i][j] < 0)
				(*aux1)[i][j] = mod + (*aux1)[i][j];
		}
	for (i = 0; i < mas; i++)
		for (j = 0; j < mas; j++)
			(*aux2)[i][j] = (*mat2)[i][j];
}

void buildm3(int ***aux1, int ***aux2, int ***mat1, int ***mat2, int mas)
{
	int i, j;
	for (i = 0; i < mas; i++)
		for (j = 0; j < mas; j++) {
			(*aux1)[i][j] = 0;
			(*aux2)[i][j] = 0;
		}
	for (i = 0; i < mas; i++)
		for (j = 0; j < mas; j++)
			(*aux1)[i][j] = (*mat1)[i][j];
	for (i = 0; i < mas; i++)
		for (j = 0; j < mas; j++) {
			int ajad = (*mat2)[i][j + mas] - (*mat2)[i + mas][j + mas];
			(*aux2)[i][j] = ajad % mod;
			if ((*aux2)[i][j] < 0)
				(*aux2)[i][j] = mod + (*aux2)[i][j];
		}
}

void buildm4(int ***aux1, int ***aux2, int ***mat1, int ***mat2, int mas)
{
	int i, j;
	for (i = 0; i < mas; i++)
		for (j = 0; j < mas; j++) {
			(*aux1)[i][j] = 0;
			(*aux2)[i][j] = 0;
		}
	for (i = 0; i < mas; i++)
		for (j = 0; j < mas; j++)
			(*aux1)[i][j] = (*mat1)[i + mas][j + mas];
	for (i = 0; i < mas; i++)
		for (j = 0; j < mas; j++) {
			(*aux2)[i][j] = ((*mat2)[i + mas][j] - (*mat2)[i][j]) % mod;
			if ((*aux2)[i][j] < 0)
				(*aux2)[i][j] = mod + (*aux2)[i][j];
		}
}

void buildm5(int ***aux1, int ***aux2, int ***mat1, int ***mat2, int mas)
{
	int i, j;
	for (i = 0; i < mas; i++)
		for (j = 0; j < mas; j++) {
			(*aux1)[i][j] = 0;
			(*aux2)[i][j] = 0;
		}
	for (i = 0; i < mas; i++)
		for (j = 0; j < mas; j++) {
			(*aux1)[i][j] = ((*mat1)[i][j] + (*mat1)[i][j + mas]) % mod;
			if ((*aux1)[i][j] < 0)
				(*aux1)[i][j] = mod + (*aux1)[i][j];
		}
	for (i = 0; i < mas; i++)
		for (j = 0; j < mas; j++)
			(*aux2)[i][j] = (*mat2)[i + mas][j + mas];
}

void buildm6(int ***aux1, int ***aux2, int ***mat1, int ***mat2, int mas)
{
	int i, j;
	for (i = 0; i < mas; i++)
		for (j = 0; j < mas; j++) {
			(*aux1)[i][j] = 0;
			(*aux2)[i][j] = 0;
		}
	for (i = 0; i < mas; i++)
		for (j = 0; j < mas; j++) {
			(*aux1)[i][j] = ((*mat1)[i + mas][j] - (*mat1)[i][j]) % mod;
			if ((*aux1)[i][j] < 0)
				(*aux1)[i][j] = mod + (*aux1)[i][j];
		}
	for (i = 0; i < mas; i++)
		for (j = 0; j < mas; j++) {
			(*aux2)[i][j] = ((*mat2)[i][j] + (*mat2)[i][j + mas]) % mod;
			if ((*aux2)[i][j] < 0)
				(*aux2)[i][j] = mod + (*aux2)[i][j];
		}
}

void buildm7(int ***aux1, int ***aux2, int ***mat1, int ***mat2, int mas)
{
	int i, j;
	for (i = 0; i < mas; i++)
		for (j = 0; j < mas; j++) {
			(*aux1)[i][j] = 0;
			(*aux2)[i][j] = 0;
		}
	for (i = 0; i < mas; i++)
		for (j = 0; j < mas; j++) {
			int ajad = (*mat1)[i][j + mas] - (*mat1)[i + mas][j + mas];
			(*aux1)[i][j] = ajad % mod;
			if ((*aux1)[i][j] < 0)
				(*aux1)[i][j] = mod + (*aux1)[i][j];
		}
	for (i = 0; i < mas; i++)
		for (j = 0; j < mas; j++) {
			int ajad = (*mat2)[i + mas][j] + (*mat2)[i + mas][j + mas];
			(*aux2)[i][j] = ajad % mod;
			if ((*aux2)[i][j] < 0)
				(*aux2)[i][j] = mod + (*aux2)[i][j];
		}
}

void build_matrez(int ****vmaux, int ***aux, int mas)
{
	int i, j;
	for (i = 0; i < mas * 2; i++)
		for (j = 0; j < mas * 2; j++) {
			if (i < mas && j < mas) {
				int m1 = (*vmaux)[0][i][j];
				int m4 = (*vmaux)[3][i][j];
				int m5 = (*vmaux)[4][i][j];
				int m7 = (*vmaux)[6][i][j];
				(*aux)[i][j] = (m1 + m4 - m5 + m7) % mod;
				if ((*aux)[i][j] < 0)
					(*aux)[i][j] = mod + (*aux)[i][j];
			}
			if (i < mas && j >= mas) {
				int m3 = (*vmaux)[2][i][j - mas];
				int m5 = (*vmaux)[4][i][j - mas];
				(*aux)[i][j] = (m3 + m5) % mod;
				if ((*aux)[i][j] < 0)
					(*aux)[i][j] = mod + (*aux)[i][j];
			}
			if (i >= mas && j < mas) {
				int m2 = (*vmaux)[1][i - mas][j];
				int m4 = (*vmaux)[3][i - mas][j];
				(*aux)[i][j] = (m2 + m4) % mod;
				if ((*aux)[i][j] < 0)
					(*aux)[i][j] = mod + (*aux)[i][j];
			}
			if (i >= mas && j >= mas) {
				int m1 = (*vmaux)[0][i - mas][j - mas];
				int m2 = (*vmaux)[1][i - mas][j - mas];
				int m3 = (*vmaux)[2][i - mas][j - mas];
				int m6 = (*vmaux)[5][i - mas][j - mas];
				(*aux)[i][j] = (m1 - m2 + m3 + m6) % mod;
				if ((*aux)[i][j] < 0)
					(*aux)[i][j] = mod + (*aux)[i][j];
			}
		}
}

int **strassen(int ***A, int ***B, int masmat)
{
	if (masmat == 1) {
		int **aux = (int **)calloc(1, sizeof(int *));
		aux[0] = calloc(1, sizeof(int));
		aux[0][0] = (*A)[0][0] * (*B)[0][0];
		return aux;
	}
	int n;
	int **mat1, **mat2;
	int ***vmaux;
	int **aux1, **aux2;
	int i, j;
	// Zona de alocare
	n = masmat;
	mat1 = (*A);
	mat2 = (*B);
	vmaux = (int ***)calloc(7, sizeof(int *));
	aux1 = (int **)calloc(n / 2, sizeof(int *));
	for (i = 0; i < (n / 2); i++)
		aux1[i] = calloc(n / 2, sizeof(int));
	aux2 = (int **)calloc(n / 2, sizeof(int *));
	for (i = 0; i < (n / 2); i++)
		aux2[i] = calloc(n / 2, sizeof(int));
	// Constructie M1
	buildm1(&aux1, &aux2, &mat1, &mat2, n / 2);
	vmaux[0] = strassen(&aux1, &aux2, n / 2);
	// Constructie M2
	buildm2(&aux1, &aux2, &mat1, &mat2, n / 2);
	vmaux[1] = strassen(&aux1, &aux2, n / 2);
	// Constructie M3
	buildm3(&aux1, &aux2, &mat1, &mat2, n / 2);
	vmaux[2] = strassen(&aux1, &aux2, n / 2);
	// Constructie M4
	buildm4(&aux1, &aux2, &mat1, &mat2, n / 2);
	vmaux[3] = strassen(&aux1, &aux2, n / 2);
	// Constructie M5
	buildm5(&aux1, &aux2, &mat1, &mat2, n / 2);
	vmaux[4] = strassen(&aux1, &aux2, n / 2);
	// Constructie M6
	buildm6(&aux1, &aux2, &mat1, &mat2, n / 2);
	vmaux[5] = strassen(&aux1, &aux2, n / 2);
	// Constructie M7
	buildm7(&aux1, &aux2, &mat1, &mat2, n / 2);
	vmaux[6] = strassen(&aux1, &aux2, n / 2);
	// elibarare matrice auxiliare
	for (i = 0; i < n / 2; i++) {
		free(aux1[i]);
		free(aux2[i]);
	}
	free(aux1);
	free(aux2);
	// creare matrice rezultat
	int **aux = (int **)calloc(n, sizeof(int *));
	for (i = 0; i < n; i++)
		aux[i] = calloc(n, sizeof(int));
	build_matrez(&vmaux, &aux, n / 2);
	for (i = 0; i < 7; i++) {
		for (j = 0; j < n / 2; j++)
			free(vmaux[i][j]);
		free(vmaux[i]);
	}
	free(vmaux);
	return aux;
}

void mS(int ****vm, int **vl, int **vc, int *nrm1, int *nrm2, int *nt, int *nc)
{
	int n = (*vl)[*nrm1];
	int **mat1, **mat2;
	int **mrez;
	if ((*vc)[*nrm1] != (*vl)[*nrm2]) {
		printf("Cannot perform matrix multiplication\n");
		return;
	}
	// Zona transfer
	mat1 = (*vm)[*nrm1];
	mat2 = (*vm)[*nrm2];
	// Rulare Strassen
	mrez = strassen(&mat1, &mat2, n);
	if (*nc < *nt) {
		(*vl)[*nc] = (*vl)[*nrm1];
		(*vc)[*nc] = (*vc)[*nrm2];
		(*vm)[*nc] = mrez;
		*nc += 1;
	} else if (*nc >= *nt) {
		int ***vmat = (*vm);
		int *revc = (*vc);
		int *revl = (*vl);
		vmat = realloc(vmat, 2 * *nt * sizeof(int *));
		revc = realloc(revc, 2 * *nt * sizeof(int *));
		revl = realloc(revl, 2 * *nt * sizeof(int *));
		revl[*nc] = (*vl)[*nrm1];
		revc[*nc] = (*vc)[*nrm2];
		(*vc) = revc;
		(*vl) = revl;
		vmat[*nc] = mrez;
		(*vm) = vmat;
		*nt = *nt * 2;
		*nc = *nc + 1;
	}
}

int main(void)
{  // nrcm se va afla la urmatoarea pozitie dupa ultima matrice
									// alocata/citita.
	int *vl = calloc(100, sizeof(int *));
	int *vc = calloc(100, sizeof(int *));
	int ***vm = (int ***)calloc(100, sizeof(int *));
	int nrtotm = 99, nrcm = 0, idm1, idm2, indexmat;
	char comanda[1];
	scanf(" %c", &comanda[0]);
	while (comanda[0] != 'Q') {
		switch (comanda[0]) {
		case 'L':
			alocare_matreice(&vm, &vl, &vc, &nrtotm, &nrcm);
			break;
		case 'D':
			scanf("%d", &indexmat);
			marimile_matricei(&vl, &vc, &indexmat, nrcm);
			break;
		case 'P':
			scanf("%d", &indexmat);
			afisare_matrice(&vm, &vl, &vc, &indexmat, nrcm);
			break;
		case 'C':
			scanf("%d", &indexmat);
			redimensionare_matrice(&vm, &vl, &vc, &indexmat, nrcm);
			break;
		case 'M':
			scanf("%d %d", &idm1, &idm2);
			if ((idm1 >= nrcm || idm1 < 0) || (idm2 >= nrcm || idm2 < 0)) {
				printf("No matrix with the given index\n");
				break;
			}
			im(&vm, &vl, &vc, &idm1, &idm2, &nrtotm, &nrcm);
			break;
		case 'O':
			sortare_matrice(&vm, &vl, &vc, &nrcm);
			break;
		case 'T':
			scanf("%d", &indexmat);
			transpunere_matrice(&vm, &vl, &vc, &indexmat, nrcm);
			break;
		case 'F':
			scanf("%d", &indexmat);
			stergere_matrice(&vm, &vl, &vc, &indexmat, &nrcm);
			break;
		case 'R':
			scanf("%d", &indexmat);
			ridicare_putere_mat(&vm, &vl, &vc, &indexmat, nrcm);
			break;
		case 'S':
			scanf("%d %d", &idm1, &idm2);
			if ((idm1 >= nrcm || idm1 < 0) || (idm2 >= nrcm || idm2 < 0)) {
				printf("No matrix with the given index\n");
				break;
			}
			mS(&vm, &vl, &vc, &idm1, &idm2, &nrtotm, &nrcm);
			break;
		default:
			printf("Unrecognized command\n");
			break;
		}
		scanf(" %c", &comanda[0]);
	}
	dealocare_resurse(&vm, &vl, &vc, &nrcm);
	return 0;
}
