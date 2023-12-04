	DATA TABLE_SAS;                      /*Exercice 1 */

	infile "C:\Users\Louis\OneDrive\Bureau\Utile\FAC MASTER1\SAS\donnees_projet_SAS_2022.csv" dlm=";" firstobs = 2;
	length NUM_ASS $8 DATE_SIN 8 GARANTIE $1 COUT_SIN 8.  PMT_SIN 8.  REC_SIN 8. PRIME_ANN 8. ;       

	input NUM_ASS DATE_SIN GARANTIE COUT_SIN PMT_SIN REC_SIN PRIME_ANN;
	format DATE_SIN DDMMYY10. ;    

	/* Exercice 2 */

	length BRANCHE $30;																				/* Création d'une variable branche */
	If  GARANTIE >= "A" AND GARANTIE <= "R" THEN BRANCHE = 'NON VIE';                   			/* Permet de savoir la nature de la garantie */
	ELSE IF GARANTIE > "R" AND GARANTIE <= "Z" THEN BRANCHE = 'Santé';   
 
	/* Exercice 3 */  

	ID_SIN = CAT(year(DATE_SIN), GARANTIE, SUBSTR(NUM_ASS,5,3));				   /* Fonction Cat pour la concaténation de caractère ||SUBSTR(NUM_ASS,5,3) = Commencer a partir du 5eme caractère et le prendre ainsi que les 2 suivants */

	/* Exercice 4 */

	length PROV_SIN PROV_SIN_NETTE 8.;											
	PROV_SIN = sum(COUT_SIN, -PMT_SIN);																/* Sum : permet de faire la sum de x élements */
	PROV_SIN_NETTE = sum(COUT_SIN, -PMT_SIN, -REC_SIN);							
	IF PROV_SIN_NETTE < 0 THEN PROV_SIN_NETTE = 0.;

	/* Exercice 5.a */

	SIN_COT = divide(COUT_SIN, PRIME_ANN);												/* Divide : diviser les deux éléments entre eux */
RUN;

PROC SUMMARY DATA = TABLE_SAS NWAY MISSING;										/* Exercice 5.b */      
	VAR SIN_COT;																							/* Variable à traiter */
	CLASS GARANTIE;																							/* Référence pour la variable à traiter */
	OUTPUT OUT = Synthese_Garantie (drop=_TYPE_FREQ_) MEAN=;												/* MEAN, afin de faire la moyenne */
RUN;
	
PROC SUMMARY DATA = TABLE_SAS NWAY MISSING;										/* Exercice 5.c */
	VAR SIN_COT;
	CLASS BRANCHE;
	OUTPUT OUT = Synthese_Branche (drop=_TYPE_FREQ_) MEAN=;							
RUN;																			/* Exercice 5.d, Commentaire : La Branche Santé est plus intéressante que celle non vie */


DATA TOP_SIN (where = (COUT_SIN > 2000));						/* Exercice 6.a */				/* Ici on créé une nouvelle table qui ne prend en compte que les cout_sin strictement supérieur à 2000 */
	set TABLE_SAS(KEEP = GARANTIE ID_SIN COUT_SIN);												/* Table de référence et KEEP pour prendre juste les variables dont on a besoin */
run;

PROC SORT data = TOP_SIN out = TOP_SIN;							/* Exercice 6.b */
	by descending COUT_SIN;																		/* Descending pour trier par ordre décroissant */
RUN;

PROC SUMMARY DATA = TOP_SIN NWAY MISSING;         				/* Exercice 7 */
	VAR COUT_SIN;
	CLASS GARANTIE;
	OUTPUT OUT = SIN_MEDIANS (drop=_TYPE_FREQ_) MEDIAN=;							/* Median : Pour calculer la mediane */
RUN;

/* Exercice 8 */

PROC SUMMARY DATA = TABLE_SAS NWAY MISSING;		
	VAR PROV_SIN_NETTE;
	CLASS BRANCHE;
	OUTPUT OUT = PROV_PAR_BRANCHE (drop=_TYPE_FREQ_) SUM=;						/* SUM : Pour calculer la somme afin d'avoir prov_totale */
RUN;	

data PROV_PAR_BRANCHE;
	set PROV_PAR_BRANCHE;
	rename PROV_SIN_NETTE = PROV_TOTALE;											/* Pour renomer Prov_SIN_Nette en Prov_Totale */
RUN;

/* Exercice 9 */

proc delete data = SIN_MEDIANS;						/* Supprime la table SIN_MEDIANS */
RUN; 

/* Exercice 10 */

data SASUSER.ANCIENNETE_SIN;
	set TABLE_SAS;												/* appel de la table TABLE_SAS */	
	DATE_OBS = "22MAR2022"d;									/* Création d'une variable date */				
	ANCIENNETE = INTCK('year', DATE_SIN, DATE_OBS);    			/*INTCK('year') pour avoir la différence d'année entre deux années */
	IF ANCIENNETE < 4 THEN TRANCHE_ANC = 'Moins de 4 ans';          		/* Comparaison pour savoir si l'ancienneté est supérieur ou égale à 4 ans ou bien inférieur à 4 ans   	*/
	ELSE IF ANCIENNETE >= 4 THEN TRANCHE_ANC = '4 ans et plus';
run;

/* Exercice 11 */

PROC SUMMARY DATA = SASUSER.Anciennete_SIN NWAY MISSING;		
	CLASS BRANCHE GARANTIE TRANCHE_ANC ;							/* Classement selon les 3 critères */
	OUTPUT OUT = Synthese (drop=_TYPE_FREQ_ rename=(_Freq_ = Nombre_Sinistres));						/* Freq représente les nombres de sinistres */
RUN;	


/* Autre version, je n'ai pas comprit la question 11 donc j'ai hésité entre ces deux versions */

data SYNTHESE_A;																
	set SYNTHESE_GARANTIE;
	keep GARANTIE _FREQ_;
	rename _FREQ_ = Nb_Sinistres_G;												/* Nombre de sinistre selon chaque garanties */
run;

data SYNTHESE_B;
	set SYNTHESE_BRANCHE;
	keep BRANCHE _FREQ_;
	rename _FREQ_ = Nb_Sinistres_B;												/* Nombre de sinistre selon chaque Branche */
run;

proc summary data = SASUSER.Anciennete_SIN nway missing;
	CLASS TRANCHE_ANC;
	OUTPUT OUT = Synthese_C (drop = _TYPE_FREQ_ rename = (_FREQ_ = Nb_Sinistres_T) drop = _Type_);			/* Nombre de sinistre selon chaque Tranche */
run;

data Other_Synthese;																/* Fusion des 3 Syntheses précédentes */
	merge Synthese_A Synthese_B Synthese_C;
run;
