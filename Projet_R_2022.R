##########################################################################################################
#                         Partie 1 : projection du compte de resultat                                    #
##########################################################################################################


library(readxl)

# importation des donnees en utilisant "import dataset" comme vue en cours

MP_passif = read_excel("Model_Point_passif.xlsx")
table_mortalite_proj = read_excel("Table_mortalite_proj.xlsx")
table_rachat_struct_proj = read_excel("Table_rachat_struct_proj.xlsx")
Risk_free_rate = read_excel("Risk_free_rate.xlsx")


#on retient les vecteurs qui vont nous interesser
table_mortalite_proj = table_mortalite_proj[, 3:ncol(table_mortalite_proj)]
table_rachat_struct_proj = table_rachat_struct_proj[, 2:ncol(table_rachat_struct_proj)]


#View(MP_passif)
#View(table_mortalite_proj)
#View(table_rachat_struct_proj)

#on demande ? R de reconnaitre le nom des colonnes qui sont dans excel
attach(MP_passif)
attach(Risk_free_rate)

#on stocke les differents vecteurs colonnes de la table de passif dans des variables qu'on va manipuler tout au long du code
#on separe le fond EURO et UC afin de bien comprendre les calculs, mais un calcul effectu? sur l'EURO s'effectue ?galement sur l'UC sauf la revalorisation des PM
#qui n'est pas effectu?e pour l'UC par soucis de simplification car complexe ? mettre en oeuvre
#ainsi chaque variable se terminera par EURO ou UC en vue de faire la precision du fond sur lequel le calcul est fait
#si aucun mention EURO ou UC alors elle est utilis?e pour les deux fonds EURO et UC
#c'est le cas des variable si dessous elles sont identique aux deux fonds

#EURO et UC
nb_polices_initial = Nb_Polices_Ouverture
age_MP = Age
sexe_MP = Sexe
anciennete_fisc_MP = Anciennete_fiscale_MP
tx_chgt_prime = Chargement_sur_prime

#EURO
PM_ouverture_EURO_initiale = PM_ouverture_euro
prime_annuelle_EURO_initiale = Prime_annuelle_euro
tx_chgt_encours_EURO = Taux_chgt_encours_euro
tx_PB_cible_EURO = Taux_PB_cible #uniquement sur l'EURO
tmg_EURO = TMG #uniquement sur l'EURO
#UC
PM_ouverture_UC_initiale = PM_ouverture_UC
prime_annuelle_UC_initiale = Prime_annuelle_UC
tx_chgt_encours_UC = Taux_chgt_encours_UC


#le facteur d'actualisation qui servira ? actualiser les prestations pour le calcul du Best estimate (BE)
#EURO et UC
discount_factor = Discount_Factor

#Construction des matrices maille models points(MP)
#on contruit des matrices car les calculs sont effectu?s MP par MP cad par groupe d'assur?s (ligne) et par ann?e (colonne)
#car on fait une projection sur plusieurs ann?es dans le future, on choisit ici comme horizon de projection 40
#donc toutes les matrices seront de taille nombre_MP * nombre_annee_prjection

#on fixe l'horizon de prection
horizon_proj = 40
#on recup?re le nombre de MP ?gale au nombre de ligne de la table MP_passif en utilisant la fonction "nrow()" d?j? vue en cours
#EURO et UC
nb_MP = nrow(MP_passif)

#les matrices de PM ouverture et cloture et nb_polices ont besoin d'etre initialis?e avec leur valeur en ann?e 0 raison pour laquelle
#leur nombre de colonne "ncol = horizon_proj+1" ainsi partout o? ces matrices interviennent on oublira pas de faire +1 ? l'indice d'incr?mentation de la colonne
#pourles matrices calcul?es sans besoin d'?tre initialis?e on verra pas de "ncol = horizon_proj+1"
#pour chaque matrice on construit un vecteur qui va agr?ger les valeurs sur tous les MP(lignes) par ann?e pour donner les valeurs annuelles
#ainsi la taille du vecteur est ?gale au nombre de colonne de la matrice mais elle n'est pas ? presicer
#la fonction "c()" a ?t? utilis?e pour cr?er les vecteurs sans pr?ciser la taille car elle d?pend du nombre de colonne de la matrice dont on agrege les valeurs



#les differentes etapes de la modelisation sont:


#etape 1 : calcul des PM ouverture

#EURO
PM_ouverture_EURO_MP = matrix(nrow = nb_MP,ncol = horizon_proj + 1)
PM_ouverture_EURO = c()
#UC
nb_UC_ouverture_MP = matrix(nrow = nb_MP,ncol = horizon_proj + 1)
nb_UC_ouverture = c()
#EURO+UC
PM_ouverture_totale = c()


#etape 2 : calcul des prestations deces et rachats

#EURO
DC_EURO_MP = matrix(nrow = nb_MP,ncol = horizon_proj)
DC_EURO = c()
rachats_EURO_MP = matrix(nrow = nb_MP,ncol = horizon_proj)
rachats_EURO = c()
prestations_EURO = c() #deces+rachats
#UC
DC_UC_MP = matrix(nrow = nb_MP,ncol = horizon_proj)
DC_UC = c()
rachats_UC_MP = matrix(nrow = nb_MP,ncol = horizon_proj)
rachats_UC = c()
prestations_UC = c() #deces+rachats
#EURO+UC
DC_totaux = c() #deces euro+deces uc
rachats_totaux = c() #rachat euro+rachats uc
prestations_totales = c() #deces+rachats
#le nombre de polices apres prestations
nb_polices_ouv_MP = matrix(nrow = nb_MP,ncol = horizon_proj + 1)
nb_polices_ouv = c()
nb_polices_cloture_MP = matrix(nrow = nb_MP,ncol = horizon_proj + 1)
nb_polices_cloture = c()


#etape 3 : calcul des primes brutes et netttes

#NB : pour la presentation du compte de resultat on retiendra que la prime brute

#EURO
prime_brute_annuelle_EURO_MP = matrix(nrow = nb_MP,ncol = horizon_proj)
prime_nette_annuelle_EURO_MP = matrix(nrow = nb_MP,ncol = horizon_proj)
prime_brute_annuelle_EURO = c()
prime_nette_annuelle_EURO = c()
#UC
prime_brute_annuelle_UC_MP = matrix(nrow = nb_MP,ncol = horizon_proj)
prime_nette_annuelle_UC_MP = matrix(nrow = nb_MP,ncol = horizon_proj)
prime_brute_annuelle_UC = c()
prime_nette_annuelle_UC = c()
#EURO+UC
prime_brute_annuelle_totale = c()
prime_nette_annuelle_totale = c()


#etape 4 : calcul des chargements sur prime et encours(PM ouverture)

#EURO
chgt_prime_EURO_MP = matrix(nrow = nb_MP,ncol = horizon_proj)
chgt_prime_EURO = c()
chgt_encours_EURO_MP = matrix(nrow = nb_MP,ncol = horizon_proj)
chgt_encours_EURO = c()
chgt_totaux_EURO= c() #chgt prime+chgt encours
#UC
chgt_prime_UC_MP = matrix(nrow = nb_MP,ncol = horizon_proj)
chgt_prime_UC = c()
chgt_encours_UC_MP = matrix(nrow = nb_MP,ncol = horizon_proj)
chgt_encours_UC = c()
chgt_totaux_UC = c() #chgt prime+chgt encours
#EURO+UC
chgt_prime_totaux = c()
chgt_encours_totaux = c()
chgt_totaux = c() #chgt prime+chgt encours


#etape 5 : revalorisation

#PM apres prestations
#EURO
PM_ap_presta_EURO_MP = matrix(nrow = nb_MP,ncol = horizon_proj)
PM_ap_presta_EURO = c()
#UC
nb_UC_ap_presta_MP = matrix(nrow = nb_MP,ncol = horizon_proj)
nb_UC_ap_presta = c()
#EURO+UC
PM_ap_presta_totale = c()

#montants de revalo euro et uc
#EURO
montants_TMG_EURO_MP = matrix(nrow = nb_MP,ncol = horizon_proj)
montants_TMG_EURO = c()
montants_PB_cible_EURO_MP = matrix(nrow = nb_MP,ncol = horizon_proj)
montants_PB_cible_EURO = c()
tx_revalo_reel_EURO_MP = matrix(nrow = nb_MP,ncol = horizon_proj)
#UC : ces montants seront mis ? 0 car pas de revalo sur l'UC
montants_TMG_UC_MP = matrix(nrow = nb_MP,ncol = horizon_proj)
montants_TMG_UC = c()
montants_PB_cible_UC_MP = matrix(nrow = nb_MP,ncol = horizon_proj)
montants_PB_cible_UC = c()
#EURO+UC
montants_TMG = c()
montants_PB_cible = c()

#calcul des PM clotures(PM revalo)
#EURO
PM_cloture_EURO_MP = matrix(nrow = nb_MP,ncol = horizon_proj + 1)
PM_cloture_EURO = c()
variation_PM_EURO = c()
#UC
nb_UC_cloture_MP = matrix(nrow = nb_MP,ncol = horizon_proj + 1)
nb_UC_cloture = c()
variation_nb_UC = c()
#EURO + UC
PM_cloture_totale = c()
variation_PM_totale = c()



#etape 6 : calcul du Best Estimate (BE)

tx_frais_reels = 0.25/100
tx_inflation = 0/100
choc_inflation = 1/100
tx_frais_acquisition = 4/100

#EURO
frais_reels_EURO = c()
frais_acquisition_EURO = c()
frais_totaux_EURO = c() #frais reels + frais acquisition
flux_BE_EURO = c()
flux_BE_EURO_actualise = c()
#UC
frais_reels_UC = c()
frais_acquisition_UC = c()
frais_totaux_UC = c() #frais reels + frais acquisition
flux_BE_UC = c()
flux_BE_UC_actualise = c()
#EURO+UC
frais_reels_totaux = c()
frais_acquisition_totaux = c()
frais_totaux = c() #frais reels + frais acquisition
flux_BE_total = c() #flux BE EURO + flux BE UC
flux_BE_total_actualise = c()

#fin des etapes


#on recup?re les table de mortalit? et de rachats projet?es
mortalite_MP = matrix(nrow = nb_MP,ncol = horizon_proj)
tx_rachat_struct_MP = matrix(nrow = nb_MP,ncol = horizon_proj)
mortalite_MP = table_mortalite_proj
tx_rachat_struct_MP = table_rachat_struct_proj

#initialisation de la PM euro et UC et le nombre de polices ? la maille model point en ann?e 0
for(mp in 1:nb_MP){
  PM_ouverture_EURO_MP[mp, 1]= PM_ouverture_EURO_initiale[mp]
  nb_UC_ouverture_MP[mp, 1] = PM_ouverture_UC_initiale[mp]
  PM_cloture_EURO_MP[mp, 1] = PM_ouverture_EURO_initiale[mp]
  nb_UC_cloture_MP[mp, 1] = PM_ouverture_UC_initiale[mp]
  nb_polices_ouv_MP[mp, 1] = nb_polices_initial[mp]
  nb_polices_cloture_MP[mp, 1] = nb_polices_initial[mp]
  
}



#c'est à partir d'ici que vous aller ecrire vos codes en utilisant exactement les variables que j'ai d?j? cr?es

#####################################
#       question 1 : projection     #
#####################################

#completer le code ci dessous en suivant les ?tapes du fichier excel
#pour chaque etape se referer ? l'onglet du fichier excel correpondant


for(j in 1:horizon_proj){ #boucle sur les annee de projection
  
  for(mp in 1:nb_MP){ #boucle sur les MP
    
    
    #etape 1 : calculer les provisions ouvertures
    
    #provisions euro ouverture annee j egale provions euro cluture annee j-1
    #de meme nombre uc ouverture annee j egale nombre uc cluture annee j-1
    #idem pour le nombe de polices
    
    PM_ouverture_EURO_MP[mp, j+1] = PM_cloture_EURO_MP[mp, j]
    nb_UC_ouverture_MP[mp, j+1] = nb_UC_cloture_MP[mp, j]
    nb_polices_ouv_MP[mp, j+1] = nb_polices_cloture_MP[mp, j]
    print(mb)

    #etape 2 : calculer prestations
    
    #EURO
    DC_EURO_MP[mp,j+1] = PM_ouverture_EURO_MP[mp, j+1] * min(mortalite_MP[mp,j+1],1)
    rachats_EURO_MP[mp,j+1] = as.numeric(tx_rachat_struct_MP[mp,j+1]) * (PM_ouverture_EURO_MP[mp, j+1] - DC_EURO_MP[mp,j+1])
    #prestations_EURO_MP[mp,j+1] = DC_EURO_MP[mp,j+1] + rachats_EURO_MP[mp,j+1]   #deces + rachats
    
    #UC
    DC_UC_MP[mp,j+1] = nb_UC_ouverture_MP[mp, j+1] * min(mortalite_MP[mp,j+1],1)
    rachats_UC_MP[mp,j+1] = as.numeric(tx_rachat_struct_MP[mp,j+1]) * (nb_UC_ouverture_MP[mp, j+1] - DC_UC_MP[mp,j+1])
    #prestations_UC_MP[mp,j+1] = DC_UC_MP[mp,j+1] + rachats_UC_MP[mp,j+1] #deces + rachats
    
    # calculer le nombre de polices
    nb_polices_cloture_MP[mp,j+1] = nb_polices_ouv_MP[mp,j+1] * (1 - mortalite_MP[mp,j+1])*(1 - as.numeric(tx_rachat_struct_MP[mp,j+1]))
                                                                                            
    
    #etape 3 : calculer les primes
    #EURO
    prime_brute_annuelle_EURO_MP[mp,j+1] = prime_annuelle_EURO_initiale[mp]*nb_polices_ouv_MP[mp,j+1]/nb_polices_ouv_MP[mp,1]
    prime_nette_annuelle_EURO_MP[mp,j+1] = prime_brute_annuelle_EURO_MP[mp,j+1]*(1- tx_chgt_encours_EURO[mp]) 
    #UC
    prime_brute_annuelle_UC_MP[mp,j+1] = prime_annuelle_UC_initiale[mp]*nb_polices_ouv_MP[mp,j+1]/nb_polices_ouv_MP[mp,1]
    prime_nette_annuelle_UC_MP[mp,j+1] = prime_brute_annuelle_UC_MP[mp,j+1]*(1- tx_chgt_encours_UC[mp]) 
    
    #etape 4 : calculer les Chargements
    #EURO
    chgt_encours_EURO_MP[mp,j+1] = PM_ouverture_EURO_MP[mp,j+1] * tx_chgt_encours_EURO[mp]
    chgt_prime_EURO_MP[mp,j+1] = prime_brute_annuelle_EURO_MP[mp,j+1] * tx_chgt_encours_EURO[mp]
    
    #UC
    chgt_encours_UC_MP[mp,j+1] = nb_UC_ouverture_MP[mp,j+1] * tx_chgt_encours_UC[mp]
    chgt_prime_UC_MP[mp,j+1] = prime_brute_annuelle_UC_MP[mp,j+1] * tx_chgt_encours_UC[mp]
    
    
    #etape 5 : Revalorisation
    
    #calculer les PM apres prestations et nombre d'uc apres prestations
    
    #EURO
    PM_ap_presta_EURO_MP[mp,j+1] = PM_ouverture_EURO_MP[mp,j+1] - rachats_EURO_MP[mp,j+1] - DC_EURO_MP[mp,j+1]
    
    #UC
    nb_UC_ap_presta_MP[mp,j+1] = nb_UC_ouverture_MP[mp,j+1] - rachats_UC_MP[mp,j+1] - DC_UC_MP[mp,j+1]
    
    #calculer les montants de revalo
    
    #EURO
    montants_TMG_EURO_MP[mp,j+1] = PM_ap_presta_EURO_MP[mp,j+1] * tmg_EURO[mp]
    montants_PB_cible_EURO_MP[mp,j+1] = PM_ap_presta_EURO_MP[mp,j+1] * tx_PB_cible_EURO[mp]
    tx_revalo_reel_EURO_MP[mp,j+1] = tmg_EURO[mp] + tx_PB_cible_EURO[mp]
    
    #UC
    montants_TMG_UC_MP[mp,j+1] = 0
    montants_PB_cible_UC_MP[mp,j+1] = 0
                        #Il n’y a pas de taux de revalorisation réel sur l’UC
    
    
    #calculer les PM cloture et nombre d'UC cloture
    #EURO
    PM_cloture_EURO_MP[mp,j+1] = PM_ap_presta_EURO_MP[mp,j+1] * (1 + tx_revalo_reel_EURO_MP[mp,j+1])
    
    #UC
    nb_UC_cloture_MP[mp,j+1] = nb_UC_ap_presta_MP[mp,j+1]
    
  }
  
  #on somme sur tous les MP pour avoir les vecteurs des valeurs annuelles
  
  #etape 1 : caculer les PM ouverture annuelles euro, uc et euro+uc
  #EURO
  PM_ouverture_EURO[j+1] = PM_ouverture_EURO_MP[mp,j+1] + PM_ouverture_EURO[j]
  
  #UC
  nb_UC_ouverture[j+1] = nb_UC_ouverture_MP[mp,j+1] + nb_UC_ouverture[j]
  
  #EURO+UC
  PM_ouverture_totale[j+1] = PM_ouverture_EURO_MP[j+1] + nb_UC_ouverture_MP[j+1] 
  
  #etape 2 : calculer les prestations annuelles deces et rachats
  #EURO
  DC_EURO[j+1] = DC_EURO_MP[mp,j+1] + DC_EURO[j]
  rachats_EURO[j+1] = rachats_EURO_MP[mp,j+1] + rachats_EURO[j]
  prestations_EURO[j+1] =  rachats_EURO[j+1] + DC_EURO[j+1] 
  
  #UC
  DC_UC[j+1] = DC_UC_MP[mp,j+1] + DC_UC[j]
  rachats_UC[j+1] = rachats_UC_MP[mp,j+1] + rachats_UC[j]
  prestations_UC[j+1] = rachats_UC[j+1] + DC_UC[j+1] 
  
  #EURO+UC
  DC_totaux[j+1] = DC_EURO[j+1] + DC_UC[j+1]
  rachats_totaux[j+1] = rachats_EURO[j+1] + rachats_UC[j+1]
  prestations_totales[j+1] = prestations_EURO[j+1] + prestations_UC[j+1]
  
  #calculer le nombre de polices annuelles
  nb_polices_ouv[j+1] = nb_polices_ouv_MP[mp,j+1] + nb_polices_ouv[j]
  nb_polices_cloture[j+1] = nb_polices_cloture_MP[mp,j+1] + nb_polices_cloture[j]
  
  #etape 3 : calculer les Primes annuelles
  #EURO
  prime_brute_annuelle_EURO[j+1] = prime_brute_annuelle_EURO_MP[mp,j+1] + prime_brute_annuelle_EURO[j]
  prime_nette_annuelle_EURO[j+1] = prime_nette_annuelle_EURO_MP[mp,j+1] + prime_nette_annuelle_EURO[j]

  
  #UC
  prime_brute_annuelle_UC[j+1] = prime_brute_annuelle_UC_MP[mp,j+1] + prime_brute_annuelle_UC[j]
  prime_nette_annuelle_UC[j+1] = prime_nette_annuelle_UC_MP[mp,j+1] + prime_nette_annuelle_UC[j]

  #EURO+UC
  prime_brute_annuelle_totale[j+1] = prime_brute_annuelle_EURO[j+1] + prime_brute_annuelle_UC[j+1]
  prime_nette_annuelle_totale[j+1] = prime_nette_annuelle_EURO[j+1] + prime_nette_annuelle_UC[j+1]
  
  
  #etape 4 : calculer les Chargements
  #EURO
  chgt_encours_EURO[j+1] = chgt_encours_EURO_MP[mp,j+1] + chgt_encours_EURO[j]
  chgt_prime_EURO[j+1] = chgt_prime_EURO_MP[mp,j+1] + chgt_prime_EURO[j]
  chgt_totaux_EURO[j+1] = chgt_encours_EURO[j+1] + chgt_prime_EURO[j+1]
  
  #UC
  chgt_encours_UC[j+1] = chgt_encours_UC_MP[mp,j+1] + chgt_encours_UC[j]
  chgt_prime_UC[j+1] = chgt_primes_UC_MP[mp,j+1] + chgt_prime_UC[j]
  chgt_totaux_UC[j+1] = chgt_encors_UC[j+1] + chgt_prime_UC[j+1]
  
  #EURO+UC
  chgt_encours_totaux[j+1] = chgt_encours_EURO[j+1] + chgt_encours_UC[j+1]
  chgt_prime_totaux[j+1] = chgt_prime_EURO[j+1] + chgt_prime_UC[j+1]
  chgt_totaux[j+1] = chgt_prime_totaux[j+1] + chgt_encours_totaux[j+1]
  
  
  #etape 5 : revalorisation
  
  #PM apres prestations annuelles
  #EURO
  PM_ap_presta_EURO[j+1] = PM_ap_presta_EURO_MP[mp,j+1] + PM_ap_presta_EURO[j]
  
  #UC
  nb_UC_ap_presta[j+1] = nb_UC_ap_presta_MP[mp,j+1] + nb_UC_ap_presta[j]
  
  #EURO+UC
  PM_ap_presta_totale[j+1] = PM_ap_presta_EURO[j+1] + nb_UC_ap_presta[j+1]
  
  #montants revalo annuels
  #EURO
  montants_TMG_EURO[j+1] = montants_TMG_EURO_MP[mp,j+1] + montants_TMG_EURO[j]
  montants_PB_cible_EURO[j+1] = montants_PB_cible_EUR_MP[mp,j+1] + montants_PB_cible_EURO[j]
  
  #UC
  montants_TMG_UC[j+1] = montants_TMG_UC_MP[mp,j+1] + montants_TMG_UC[j]
  montants_PB_cible_UC[j+1] = montants_PB_cible_UC_MP[mp,j+1] + montants_PB_cible_UC[j]
  #il n'y a toujours pas de taux de revalorisation réel sur l'UC
  
  #EURO+UC
  montants_TMG[j+1] = montants_TMG_EURO[j+1] + montants_TMG_UC[j+1]
  montants_PB_cible[j+1] = montants_PB_cible_EURO[j+1] + montants_PB_cible_UC[j+1]
  
  #PM cloture annuelles
  #EURO
  PM_cloture_euro[j+1] = PM_cloture_EURO_MP[mp,j+1] + PM_cloture_EURO[j]
  variation_PM_EURO[j+1] = PM_cloture_EURO[j+1] - PM_ouverture_EURO[j+1]
  
  #UC
  nb_UC_cloture[j+1] = nb_UC_cloture_MP[mp,j+1] + nb_UC_cloture[j]
  variation_nb_UC[j+1] = nb_UC_cloture[j+1] - nb_UC_ouverture[j+1]
  
  #EURO+UC
  PM_cloture_totale[j+1] = PM_cloture_euro[j+1] + nb_UC_cloture[j+1]
  variation_PM_totale = variation_PM_EURO[j+1] + variation_nb_UC[j+1]
  
  
  #etape 6 : calculer les BE
  
  #EURO
  frais_reels_EURO[j+1] = PM_ouverture_EURO[j+1] * tx_frais_reels * ((1 + tx_inflation + choc_inflation)^(t-1))
  frais_acquisition_EURO[j+1] = prime_brute_annuelle_EURO[j+1] * tx_frais_acquisition * ((1 + tx_inflation + choc_inflation)^(t-1))
  frais_totaux_EURO[j+1] = frais_reels_EURO[j+1] + frais_acquisition_EURO[j+1]
  
  #UC
  frais_reels_UC[j+1] = PM_ouverture_UC[j+1] * tx_frais_reels * ((1 + tx_inflation + choc_inflation)^(t-1))
  frais_acquisition_UC[j+1] = prime_brute_annuelle_UC[j+1] * tx_frais_acquisition * ((1 + tx_inflation + choc_inflation)^(t-1))
  frais_totaux_UC[j+1] = frais_reels_UC[j+1] + frais_acquisition_UC[j+1]
  
  #EURO+UC
  frais_reels_totaux[j+1] = PM_ouverture_totaux[j+1] * tx_frais_reels * ((1 + tx_inflation + choc_inflation)^(t-1))
  frais_acquisition_totaux[j+1] = prime_brute_annuelle_totaux[j+1] * tx_frais_acquisition * ((1 + tx_inflation + choc_inflation)^(t-1))
  frais_totaux = frais_reels_totaux[j+1] + frais_acquisition_totaux[j+1]
  
  
}


#concatenation des differents vecteurs pour avoir le compte de resultat
#vous n'avez juste qu'à compiler pour visualiser vos resultats


#EURO
#compte de reusultat
compte_resultat_EURO = rbind(prime_brute_annuelle_EURO, prestations_EURO, nb_polices_ouv, DC_EURO, rachats_EURO, nb_polices_cloture, 
                             chgt_totaux_EURO, chgt_prime_EURO, chgt_encours_EURO, PM_ouverture_EURO, PM_ap_presta_EURO,
                             montants_TMG_EURO, montants_PB_cible_EURO, variation_PM_EURO, PM_cloture_EURO, frais_totaux_EURO,
                             frais_reels_EURO, frais_acquisition_EURO, flux_BE_EURO, discount_factor, flux_BE_EURO_actualise)
View(compte_resultat_EURO) 
#BE ?gale somme des flux actualis?s
BE_EURO = sum(flux_BE_EURO_actualise)
BE_EURO # le resultat ? retrouver est 1286377167

#UC
compte_resultat_UC = rbind(prime_brute_annuelle_UC, prestations_UC, nb_polices_ouv, DC_UC, rachats_UC, nb_polices_cloture, chgt_totaux_UC, chgt_prime_UC,
                           chgt_encours_UC, nb_UC_ouverture, nb_UC_ap_presta, montants_TMG_UC, montants_PB_cible_UC, variation_nb_UC,
                           nb_UC_cloture, frais_totaux_UC, frais_reels_UC, frais_acquisition_UC, flux_BE_UC, discount_factor, flux_BE_UC_actualise)
View(compte_resultat_UC)

BE_UC = sum(flux_BE_UC_actualise)
BE_UC # le resultat ? retrouver est  202692358


#EURO + UC
compte_resultat = rbind(prime_brute_annuelle_totale, prestations_totales, nb_polices_ouv, DC_totaux, rachats_totaux, nb_polices_cloture, chgt_totaux, chgt_prime_totaux,
                        chgt_encours_totaux, PM_ouverture_totale, PM_ap_presta_totale, montants_TMG, montants_PB_cible, variation_PM_totale, PM_cloture_totale,
                        frais_totaux, frais_reels_totaux, frais_acquisition_totaux, flux_BE_total, discount_factor, flux_BE_total_actualise)
View(compte_resultat)

BE = sum(flux_BE_total_actualise)
BE # 1489069525


#controle  : si les calculs sont justes on doit avoir BE-(BE_EURO+BE-UC)=0 et 
#compte_resultat - (compte_resultat_EURO - compte_resultat_UC) = 0 sur toutes les lignes sauf les lignes nb_polices et discount_factor
#parce qu'elles sont identiques dans les trois compte de resultat

round(BE - (BE_EURO + BE_UC)) # on doit avoir o logiquement car BE = BE_EURO + BE_UC
View(round(compte_resultat - (compte_resultat_EURO + compte_resultat_UC)))


#####################################
#       question 2 : fonction       #
#####################################

#construire une fonction qui prend en entree les parametres BE et renvoie une liste contenant le BE_EURO, BE_UC et BE(EURO+UC)
#le corps de la fonction doit commencer au niveau de la question 1 precedante
#pas besoin de recreer les differentes matrices ? l'interieurs de la fonction  comme je l'ai d?ja fait elle les reconnaitra une fois quelles sont compil?es
#toute fois vous pouvez les recreer dans la fonction si vous le souhaiter (juste un copier-coler suffira)
#le fonction doit commencer au niveau de la question 1
#le but est de comparer les BE en fonction des parametes
#calculer les BE pour chaques param?tres et recapituler le tout dans un tableau a 7 lignes et 3 colonnes
#les 4 premieres lignes pour les parametre et les 3 dernieres pour les BE
#la premi?re colonne pour les valeurs param1 ainsi de suite jusqu'? param3


# 1 : completer la fonction suivante pour avoir la fonction recherchee

calcul_BE = function(param_tx_frais_reels, param_tx_infl, param_choc_infl, param_tx_frais_acq){
  
  #completer ici
  
  
  
  
  
  BE_euro = sum() #vous sommez les flux de BE actualis?es pour avoir le BE
  BE_uc = sum()
  BE_tot = sum()
  #jusqu'ici
  
  param_BE = rbind(param_tx_frais_reels, param_tx_infl, param_choc_infl, param_tx_frais_acq)
  BE_calcule = rbind(BE_euro, BE_uc, BE_tot)
  
  resultat = rbind(param_BE, BE_calcule)
  return(resultat)
}

#caclul des BE en fonction de l'inflation en gardant les autre parametres inchang?s
#calcul des BE pour les param suivants
tx_frais_reels = 0.25/100
tx_inflation = 0/100
choc_inflation = 1/100
tx_frais_acquisition = 4/100

calcul1 = calcul_BE(param_tx_frais_reels = tx_frais_reels, param_tx_infl = tx_inflation, param_choc_infl = choc_inflation,
                    param_tx_frais_acq = tx_frais_acquisition)
calcul1

#calcul des BE pour inflation2
tx_inflation2 = 1/100

calcul2 = calcul_BE(param_tx_frais_reels = tx_frais_reels, param_tx_infl = tx_inflation2, param_choc_infl = choc_inflation,
                    param_tx_frais_acq = tx_frais_acquisition)
calcul2

#calcul des BE inflation3
tx_inflation3 = 2/100

calcul3 = calcul_BE(param_tx_frais_reels = tx_frais_reels, param_tx_infl = tx_inflation3, param_choc_infl = choc_inflation,
                    param_tx_frais_acq = tx_frais_acquisition)
calcul3
#tableau recap
tableau_recap = data.frame(calcul1, calcul2, calcul3)
tableau_recap

# 2: comment evoluent les BE en fonction de l'inflation ?




############################################################################################################
#                                            Partie 2                                                       #
#############################################################################################################

# NB: toutes les tables qui vous ont ?t? dans la premi?re partie ont ?t? simplifi? pour permettre ? tout de partir sur une meme base et pour
#vous faciliter un peu la tache,donc dans cette partie il s'agira pour vous de reconstituer les tables de mortalite et rachat projet?e ? partir
#des tables initiales "Table_mortalite_F.xlsx", "Table_mortalite_H.xlsx" et "Table_rachat_struct.xlsx"

#tables initiales
library(readxl)
table_mortalite_Femmes = read_excel("Table_mortalite_F.xlsx")
table_mortalite_Hommes = read_excel("Table_mortalite_H.xlsx")
table_rachat_struct = read_excel("Table_rachat_struct.xlsx")

#visuliser les tables
View(table_mortalite_Femmes)
View(table_mortalite_Hommes)
View(table_rachat_struct)

#pour demander ? R de reconnaitre le nom des colonnes des tables
attach(table_mortalite_Femmes)
attach(table_mortalite_Hommes)
attach(table_rachat_struct)


#on recup?re l'age des assur?s(MP) de la partie 1
#NB: chaque age augmente de 1 ? chaque ann?e de projection, cette subtilit? est ? prendre en compte dans votre code
#c'est ? dire si un MP ? un age x, quand on sera ? l'ann?e de projection j son age sera x+j
#c'est ainsi que vous recupererez les bons taux de mortalit?
#NB : chaque anciennet? va evoluer comme l'age, ? chaque annee de projection j, l'anciennete augmente de +j
age_initial_MP = age_MP
mortalite_TH_initiale = Mortalite_TH #taux de mortalit? des hommes par age qui se trouve dans la table de mortalite des hommes, R reconnait le nom du vecteur
mortalite_TF_initiale = Mortalite_TF #idem comme pour les hommes
anciennete_fisc_initiale_MP = anciennete_fisc_MP # le vecteur d'anciennete de la table MP_passif


#on definit les matrice qui vont contenir les mortalit? et taux de rachats proj?t?e reconstitu?es
#? la fin on les comparera ? celle donn?es dans la partie 1
#pour vous aider referez vous aux formules des l'onglets "TH_TF_Proj" et "Taux_rachat_struct_proj" du fichier compte de resulat

mortalite_proj_reconstituee_MP = matrix(nrow = nb_MP, ncol = horizon_proj)
tx_rachat_struct_proj_reconstituee_MP = matrix(nrow = nb_MP, ncol = horizon_proj)

#mortalit?

mortalite_proj_reconstituee_MP = matrix(nrow = nb_MP, ncol = horizon_proj)
for(j in 1:horizon_proj){
  
  for(mp in 1:nb_MP){
    #indication : on cr?e une variable "age_proj_MP" qui va contenir l'age initial de chaque assur? (mp) + l'annee de projection
    #qui servira d'indice pour recup?rer la mortalit? de l'age correspondante
    age_proj_MP = age_initial_MP[mp]+j+1
    
    #completer
    
    
  }
}
View(mortalite_proj_reconstituee_MP)
sum(mortalite_MP - mortalite_proj_reconstituee_MP) # doit donner 0


#rachats

tx_rachat_struct_proj_reconstituee_MP = matrix(nrow = nb_MP, ncol = horizon_proj)
for(j in 1:horizon_proj){
  
  for(mp in 1:nb_MP){
    #indication : on cr?e une variable "anc_mp_proj" qui va contenir l'anciennet? initiale de chaque assur? (mp) + l'annee de projection
    #qui servira d'indice pour recup?rer le taux de rachat correspondant ? l'anciennete
    anc_proj_MP = anciennete_fisc_initiale_MP[mp] + j
    
    #completer
  }
}
View(tx_rachat_struct_proj_reconstituee_MP)
sum(tx_rachat_struct_MP - tx_rachat_struct_proj_reconstituee_MP) # 0




###############
#     BONUS   #
##############

#Comme precedemmment reconstinuer le discount factor qui sert ? calculer en se referent ? l'onglet "Taux Risk Free"
#cette fois je ne donne aucune indication

discount_factor_initial = discount_factor
discount_factor_reconstitue = c()




#control
discount_factor_reconstitue - discount_factor_initial #doit donner 0
