
/* Exercice 1 */

/* Import pour Risques et Sinistres */
PROC IMPORT DATAFILE="/home/u63665692/sasuser.v94/risques.xlsx"
		    OUT=risques
		    DBMS=XLSX
		    REPLACE;
RUN;

PROC IMPORT DATAFILE="/home/u63665692/sasuser.v94/sinistres.xlsx"
		    OUT=sinistres
		    DBMS=XLSX
		    REPLACE;
		    
RUN;


/* Avant de commencer, on précise que toutes nos tables dans le ppt on été extraites à l'aide de cette macro*/
%macro imprim(table) ;
	proc print data=&table style(head)={backgroundcolor=grey color=white fontsize=3} noobs;
	run;
%mend;

proc format ;
   picture myfmt (round)
      0 - high  = "000 000 009,99 €" (mult=100)
   ;
run ;

/* Question 1 */ 

/* On crée unte table avec uniquement le numéro du client sans doublons grâce à nodupkey*/

proc sort data=risques (keep=num_soc) out=risques_trier nodupkey; 
	by num_soc;
run;

/* On somme avec proc summary notre nombre de sociétaire*/
/* On pourrait également utiliser proc mean par exemple */

proc summary data=risques_trier nway missing;
	output out=nbre_client (drop=_type_ rename=(_freq_=nbre_client));
run; /* Il y a 9993 clients */

/* On somme le nombre de contrat par client*/
proc summary data=risques nway missing;
	class num_soc ;
	output out=nbre_cont_par_client (drop=_type_ rename=(_freq_=cont_client));
run;

/* On somme le nombre de client par nombre de contrat*/
proc summary data=nbre_cont_par_client nway missing;
	class cont_client;
	output out=nbre_cont_par_client_2 (drop=_type_ rename=(cont_client="Nombre de contrats"n _freq_="Nombre de clients"n));
run;
%imprim(nbre_cont_par_client_2);


/* Question 2 */

proc summary data=risques nway missing;
class num_soc sexe;
output out=risques_age;
run;

proc summary data=risques_age nway missing;
	class sexe;
	output out=repartition (drop=_type_ rename=(_freq_=Nombre));
run;
%imprim(repartition);



/* Question 3 */

/* On change le format de dat_nais pour le transformer en date*/
data age;
	set risques ;
	format dat_nais DDMMYY10.;
	age=2010-year(dat_nais);
run;

/* On enlÃ¨ve les doublons pour Ã©viter que la mÃªme personne soit compter plusieurs fois */
proc sort data=age (keep= num_soc sexe dat_nais age) out=age_trier nodupkey;
	by num_soc;
run;
	

proc means data=age_trier mean;
	class sexe;
	var age;
	output out=moy_sexe (drop=_type_ _freq_) mean=moyenne ;
	format moyenne 8.2;
run;

%imprim(moy_sexe);




/* Question 4 */

data mois_souscription;
	set risques (keep = num_soc dat_deb);
	format dat_deb DDMMYY10.;
	mois=month(dat_deb);
run;


proc summary data=mois_souscription nway missing;
	class mois;
	output out=nbre_sousc_mois (drop=_type_ rename=(_freq_="Nombre de souscription"n));
run;

proc gplot data=nbre_sousc_mois;
	plot "nombre de souscription"n * mois;
	symbol v=circle interpol=line c=red;
	title "Souscription des clients par mois";
run;
quit;



/* Question 5 */
/* Marque de voiture */

proc gchart data=risques;
	vbar marque;
	title "Répartition des marques de voitures";
run;
quit;

/* On voit bien sur le graphique que le maximum est RENAULT mais le min est moins implicite*/
proc summary data=risques max nway missing;
	class marque;
	output out=marque (drop=_type_ rename=(_freq_=nombre)) ;
run;

proc sort data=marque out=marque_min_max;
	by nombre ;
run;
%imprim(marque_min_max);

/* Les véhicules AUTOBIAN LEXUS SANTANA sont les moins sinistrÃ©s avec seulement 1 assuré. */ 

/* Par zone gÃ©ographiques */

proc gchart data=risques;
	vbar zone;
	title "Répartition des zones";
run;
quit;

/* La zone 6 est la zone avec le plus de sinistre et la 0 avec le moins d'assurés */



/* Question 6 */ 

proc summary data=risques nway missing;
class num_soc dat_deb;
output out=nombre_contrat_soc (drop=_type_ rename=(_freq_=nbre_ctrt_sum_soc));
run;

proc sort data=nombre_contrat_soc (where=(nbre_ctrt_sum_soc>1)) out=nbre_contrat_soc_trier(drop=dat_deb nbre_ctrt_sum_soc) nodupkey;
by num_soc;
run;
%imprim(nbre_contrat_soc_trier);


proc summary data=risques nway missing;
class num_soc marque groupe classe;
output out=changement_marque (drop=_type_);
run;

proc summary data=changement_marque  nway missing;
class num_soc;
output out=changement_soc (drop=_type_ rename=(_freq_=i));
run;

proc sort data=changement_soc (where=(i>1)) out=changement_soc_trier nodupkey;
by num_soc;
run;

proc summary data=changement_soc_trier nway missing;
output out=Nbre_chang (drop=_type_ rename=(_freq_="Nombre changements"n));
run;
%imprim(Nbre_chang);



/* Question 7 */

/* On tri de maniÃ¨re dÃ©croissante les dat_deb pour avoir l'usage le plus rÃ©cent */
proc sort data=risques (keep=num_soc sexe usage dat_deb) out=usage_date  ;
	by num_soc descending dat_deb;
run;

/* On enlÃ¨ve les doublons pour avoir chaques clients avec leur usage le plus rÃ©cents */
proc sort data=usage_date  out=usage_recent nodupkey;
	by num_soc sexe;
run;


proc gchart data=usage_recent;
	vbar USAGE;
	title "Usage des assurés";
run;
quit;

/* L'usage priviligié est le déplacement trajet travail */



/* Question 8 */

/* On a le mÃªme problÃ¨me qu'avec la Question 7, on a des bonus/malus diffÃ©rents pour le mÃªme client */
/* On prend donc encore le bonus/malus le plus rÃ©cent */

proc sort data=risques (keep=num_soc coeff dat_deb) out=coeff_date  ;
	by num_soc descending dat_deb;
run;

proc sort data=coeff_date  out=coeff_recent nodupkey;
	by num_soc;
run;

data coeff_rec;
	set coeff_recent;
	if 50 <= coeff < 60 then Coefficients="50-59";
	if 60 <= coeff < 70 then Coefficients="60-69";
	if 70 <= coeff < 80 then Coefficients="70-79";
	if 80 <= coeff < 90 then Coefficients="80-89";
	if 90 <= coeff < 100 then Coefficients="90-99";
	if 100 <= coeff  then Coefficients="99+";
run;


proc summary data=coeff_rec nway missing;
	class Coefficients;
	output out=repartition_coeff (drop=_type_ rename=(_freq_="Nombre de clients"n));
run;
%imprim(repartition_coeff);




/* Question 9 */

/* Le nombre total de risque correspond au nombre total de contrat */

data nb_risq2;
	set risques (keep=num_soc dat_fin dat_deb nb_risq);
	diff= dat_fin-dat_deb;
	nb_risq2=divide(diff,365);
	difference=round(nb_risq-nb_risq2,1);
run;
/* La Table est très grande donc nous préférons la laissé en commentaire 
%imprim(nb_risq2);*/


/* Question 10 */

/* On redimensionne la variable interv car elle va prendre la dimension de sa 1Ã¨re valeur qui est de petite dimension */

data age_couverture;
	set risques (keep=num_soc dat_nais nb_risq);
	length interv $50;
	format dat_nais DDMMYY10.;
	age=2010-year(dat_nais);
	if age<25 then interv="-25 ans";
	if 25<=age<35 then interv="25-34 ans";
	if 35<=age<45 then interv="35-44 ans";
	if 45<=age then interv="45 ans et +";
run;

proc sort data=age_couverture out=age_couverture_trier nodupkey;
by num_soc;
run;

proc summary data=age_couverture_trier nway missing;
	class interv;
	output out=tranche_age_couv (drop=_type_ rename=(_freq_=nbre_annees_couv));
run;

proc gchart data=age_couverture_trier;
	vbar interv;
	title "Nombres d'années de couverture par tranche d'âge";
run;
quit;







/* Exercice 2 */

/* Question 1 */




/* On enlève les sinistres clos sans règlement */
data sin (where=(i^=1));
set sinistres;
if sitsin="CLO" and cout_bdg=0 and cout_vol=0 and cout_dom=0 then i=1;
cout = sum(cout_bdg,cout_vol,cout_dom);
run;


proc summary data=sin nway missing;
class clerisq;
output out=rsin (drop=_type_ rename=(_freq_="Nombre de sinistre"n))
sum(cout) = cout;
run;
%imprim(rsin);

/* Question 2 */
/* Base risques */

data risques1;
set risques ;
garantie = gar_bdg||" "||gar_vol||" "||gar_dom;
cotisation = sum(cotis_bdg,cotis_vol,cotis_dom);
run;

proc summary data=risques1 nway missing;
class clerisq num_soc garantie;
output out=risques2
sum(cotisation)=cotisation
sum(nb_risq)=nb_risq;
run;

/* Base finale */
data finale;
merge risques2 (in=A) rsin(in=B);
by clerisq;
run;


/* Analyses pour le portefeuille global */

proc summary data=finale nway missing;
  output out=global_stats (drop=_type_ 
  	rename=(
  		_freq_ = "Nombre contrat global"n
  		cotisation = "Cotisation global"n
  		cout = "Cout global"n 
  		nb_risq = "Nombre risque global"n
  		"Nombre de sinistre"n = "Nombre sin global"n))
	sum(cotisation)=cotisation
    sum(cout)=cout
	sum(nb_risq)=nb_risq                
    sum("Nombre de sinistre"n)="Nombre de sinistre"n;
    format 
  		"Frequence sin global"n "Nombre risque global"n 8.2
  		"Cout moyen sin global"n
  		"Prime pure global"n myfmt.
  		"Ratio sc global %"n 8.2;
run;
%imprim(global_stats);



/* Analyses pour le portefeuille par garantie */


proc summary data=finale nway missing;
  class garantie;
  var cotisation cout "Nombre de sinistre"n nb_risq;
  output out=garantie_stats (drop=_type_ 
  rename=(
  _freq_ = "Nombre contrat garantie"n
  cotisation = "Cotisation garantie"n
  cout = "Cout garantie"n 
  nb_risq = "Nombre risque garantie"n
  "Nombre de sinistre"n = "Nombre sin garantie"n))
    sum(cotisation)=cotisation
    sum(cout)=cout
    sum(nb_risq)=nb_risq
    sum("Nombre de sinistre"n)="Nombre de sinistre"n;
   format "Cotisation garantie"n "Cout garantie"n myfmt. "Nombre risque garantie"n 8.2 ;
run;
%imprim(garantie_stats);




/* Question 3 */


/* Calculs pour le portefeuille global */
data global_result (drop="Nombre contrat global"n "Cotisation global"n "Cout global"n "Nombre sin global"n "Nombre risque global"n);
  set global_stats;

  /* Calculs pour la fréquence de sinistralité */
  "Frequence sin global"n = "Nombre sin global"n / "Nombre contrat global"n;

  /* Calculs pour le coût moyen des sinistres */
  "Cout moyen sin global"n = "Cout global"n / "Nombre sin global"n;

  /* Calculs pour la prime pure */
  "Prime pure global"n = "Frequence sin global"n  * "Cout moyen sin global"n; 
  
  /* Calculs pour le ratio de Sinistres sur Cotisations (S/C ou S/P) */
  "Ratio sc global %"n= "Cout global"n / "Cotisation global"n *100; 
  format 
  "Frequence sin global"n 8.2
  "Cout moyen sin global"n
  "Prime pure global"n myfmt.
  "Ratio sc global %"n 8.2;
  
run;

%imprim(global_result);

/* Calculs pour chaque formule de garantie */
data garantie_result (drop="Nombre contrat garantie"n "Cotisation garantie"n "Cout garantie"n  "Nombre sin garantie"n "Nombre risque garantie"n);
  set garantie_stats;

  /* Calculs pour la fréquence de sinistralité par garantie */
  "Frequence sin garantie"n = "Nombre sin garantie"n / "Nombre contrat garantie"n;

  /* Calculs pour le coût moyen des sinistres par garantie */
  "Cout moyen sin garantie"n = "Cout garantie"n  / "Nombre sin garantie"n;
  
  /* Calculs pour la prime pure par garantie */
  "Prime pure garantie"n = "Frequence sin garantie"n * "Cout moyen sin garantie"n;

  /* Calculs pour le ratio de Sinistres sur Cotisations (S/C ou S/P) par garantie */
  "Ratio sc garantie %"n = "Cout garantie"n  / "Cotisation garantie"n *100;
    format 
  "Frequence sin garantie"n 8.2
  "Cout moyen sin garantie"n
  "Prime pure garantie"n myfmt.
  "Ratio sc garantie %"n 8.2;

run;

%imprim(garantie_result);




/* Exercice 3 : BONUS */

/* Question 1 */                 

data finale2; 
merge age (in=A) rsin(in=B); 
by clerisq;
run;


/* Créer la variable tranche_age en fonction de l'âge */
data finale2_tranche_age;
  set finale2;
  length tranche_age $20;
  garantie = gar_bdg||" "||gar_vol||" "||gar_dom; 
 
  /* Créer la variable tranche_age en fonction de l'âge */
  if age < 25 then tranche_age = '- 25 ans'; 
  else if 25 <= age < 35 then tranche_age = '25-34 ans';
  else if 35 <= age < 45 then tranche_age = '35-44 ans';
  else tranche_age = '45 ans et plus';
run;


proc gchart data=finale2_tranche_age;
	vbar tranche_age;
	title "Répartition des sinistres par tranches d'âge";
run;
quit; 


/* Question 2 */

proc summary data=finale2_tranche_age nway missing;
  class num_soc sexe tranche_age garantie;
  output out=nb_contrats_tranche_age (drop=_type_ rename=(_freq_="Nombre de contrat"n))
  sum("Nombre de sinistre"n)="Nombre de sinistre"n;
run;

proc summary data=nb_contrats_tranche_age nway missing;
	class tranche_age garantie ;
	output out=tranche_age_garantie (drop=_freq_ _type_
	rename=(tranche_age="tranche d'âge"n))
	sum("Nombre de sinistre"n)="Nombre de sinistre"n
	sum("Nombre de contrat"n)="Nombre de contrat"n;
run;

data sinistralité;
	set tranche_age_garantie;
	"Sinistralité"n=round("Nombre de sinistre"n/"Nombre de contrat"n,0.01);
run;
%imprim(sinistralité);



/* Question 3 */













