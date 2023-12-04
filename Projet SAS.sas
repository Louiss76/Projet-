	DATA TABLE_SAS;                      /*Exercice 1 */

	infile "C:\Users\Louis\OneDrive\Bureau\Utile\FAC MASTER1\SAS\donnees_projet_SAS_2022.csv" dlm=";" firstobs = 2;
	length NUM_ASS $8 DATE_SIN 8 GARANTIE $1 COUT_SIN 8.  PMT_SIN 8.  REC_SIN 8. PRIME_ANN 8. ;       

	input NUM_ASS DATE_SIN GARANTIE COUT_SIN PMT_SIN REC_SIN PRIME_ANN;
	format DATE_SIN DDMMYY10. ;    

	/* Exercice 2 */

	length BRANCHE $30;																				/* Cr�ation d'une variable branche */
	If  GARANTIE >= "A" AND GARANTIE <= "R" THEN BRANCHE = 'NON VIE';                   			/* Permet de savoir la nature de la garantie */
	ELSE IF GARANTIE > "R" AND GARANTIE <= "Z" THEN BRANCHE = 'Sant�';   
 
	/* Exercice 3 */  

	ID_SIN = CAT(year(DATE_SIN), GARANTIE, SUBSTR(NUM_ASS,5,3));				   /* Fonction Cat pour la concat�nation de caract�re ||SUBSTR(NUM_ASS,5,3) = Commencer a partir du 5eme caract�re et le prendre ainsi que les 2 suivants */

	/* Exercice 4 */

	length PROV_SIN PROV_SIN_NETTE 8.;											
	PROV_SIN = sum(COUT_SIN, -PMT_SIN);																/* Sum : permet de faire la sum de x �lements */
	PROV_SIN_NETTE = sum(COUT_SIN, -PMT_SIN, -REC_SIN);							
	IF PROV_SIN_NETTE < 0 THEN PROV_SIN_NETTE = 0.;

	/* Exercice 5.a */

	SIN_COT = divide(COUT_SIN, PRIME_ANN);												/* Divide : diviser les deux �l�ments entre eux */
RUN;

PROC SUMMARY DATA = TABLE_SAS NWAY MISSING;										/* Exercice 5.b */      
	VAR SIN_COT;																							/* Variable � traiter */
	CLASS GARANTIE;																							/* R�f�rence pour la variable � traiter */
	OUTPUT OUT = Synthese_Garantie (drop=_TYPE_FREQ_) MEAN=;												/* MEAN, afin de faire la moyenne */
RUN;
	
PROC SUMMARY DATA = TABLE_SAS NWAY MISSING;										/* Exercice 5.c */
	VAR SIN_COT;
	CLASS BRANCHE;
	OUTPUT OUT = Synthese_Branche (drop=_TYPE_FREQ_) MEAN=;							
RUN;																			/* Exercice 5.d, Commentaire : La Branche Sant� est plus int�ressante que celle non vie */


DATA TOP_SIN (where = (COUT_SIN > 2000));						/* Exercice 6.a */				/* Ici on cr�� une nouvelle table qui ne prend en compte que les cout_sin strictement sup�rieur � 2000 */
	set TABLE_SAS(KEEP = GARANTIE ID_SIN COUT_SIN);												/* Table de r�f�rence et KEEP pour prendre juste les variables dont on a besoin */
run;

PROC SORT data = TOP_SIN out = TOP_SIN;							/* Exercice 6.b */
	by descending COUT_SIN;																		/* Descending pour trier par ordre d�croissant */
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
	DATE_OBS = "22MAR2022"d;									/* Cr�ation d'une variable date */				
	ANCIENNETE = INTCK('year', DATE_SIN, DATE_OBS);    			/*INTCK('year') pour avoir la diff�rence d'ann�e entre deux ann�es */
	IF ANCIENNETE < 4 THEN TRANCHE_ANC = 'Moins de 4 ans';          		/* Comparaison pour savoir si l'anciennet� est sup�rieur ou �gale � 4 ans ou bien inf�rieur � 4 ans   	*/
	ELSE IF ANCIENNETE >= 4 THEN TRANCHE_ANC = '4 ans et plus';
run;

/* Exercice 11 */

PROC SUMMARY DATA = SASUSER.Anciennete_SIN NWAY MISSING;		
	CLASS BRANCHE GARANTIE TRANCHE_ANC ;							/* Classement selon les 3 crit�res */
	OUTPUT OUT = Synthese (drop=_TYPE_FREQ_ rename=(_Freq_ = Nombre_Sinistres));						/* Freq repr�sente les nombres de sinistres */
RUN;	


/* Autre version, je n'ai pas comprit la question 11 donc j'ai h�sit� entre ces deux versions */

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

data Other_Synthese;																/* Fusion des 3 Syntheses pr�c�dentes */
	merge Synthese_A Synthese_B Synthese_C;
run;
