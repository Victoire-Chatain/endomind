# -----------------------------------------------------------------------------#
############ ESTIMATION des coûts différenciés selon les femmes ################
# -----------------------------------------------------------------------------#

# Packages-------------
library(writexl)
library(readxl)
library(ggplot2)
library(dplyr)
library(stargazer)


# Données profil des femmes ----------

## Nombre femmes concernées ----------
nb_femmes_endo <- 2.5e6         
# 2.5 millions de femmes concernées
#hypothèse forte

##  Data gravité de l'endométriose  ----------
gravite <- data.frame(
  Gravite = c("Légère", "Modérée", "Grave"),
  Pourcentage = c(9, 61, 30)
)
#source : enquête endo-travail

##  Data statut professionnel  ----------
statut_pro <- data.frame(
  Statut_Professionnel = c("En emploi", "Au chômage", "En formation/études", "Arrêt longue maladie", "Inactive"),
  Pourcentage = c(78, 8, 4, 5, 3), # source : enquête endo-travail
  Coef_Statut = c(1, 0.3, 0.2, 0.5, 0.1)  # Coefficients de coût selon l’impact économique
  # Trouver un chiffre fiable et vérifier la source
)

# Statut professionnel
# Une femme en emploi va engendrer des coûts indirects plus visibles (absentéisme, perte de productivité).
#  Une femme au chômage ou inactive peut ne pas générer de coût direct pour l'employeur, 
# mais peut générer des coûts sociaux ou médicaux.
# L’impact économique diffère fortement entre ces statuts.
# On va prendre des coef, en l'absence de coef précis, on va les déterminer arbitrairement

##  Data CSP  ----------
csp <- data.frame(
  Categorie_SocioPro = c("Cadres", "Intermédiaires", "Employées", "Indépendantes"),
  Pourcentage = c(23, 10, 57, 8),  #source : enquête endo-travail
  Coef_CSP = c(1.5, 1.2, 1, 1.1) # Plus le poste est stratégique, plus le coût est élevé
  # Trouver un chiffre fiable et vérifier la source
)

# CSP
# Une cadre absente coûte en général plus cher qu’une employée, 
# en raison du salaire plus élevé et du rôle stratégique.
# Le temps d'absence ou l'impact d’un poste difficile à remplacer peut
# faire varier les coûts indirects (remplacement, surcharge d'équipe…).

##  Data type de contrat  ----------
contrat <- data.frame(
  Type_Contrat = c("CDI ou assimilé", "CDD, intérim...", "Non précisé"),
  Pourcentage = c(72, 21, 7),#source : enquête endo-travail
  Coef_Contrat = c(1.2, 0.8, 0.6)  # CDI = impact plus long terme
  # Trouver un chiffre fiable et vérifier la source
)

# Type de contrat
# Une personne en CDI implique un engagement long terme, donc un coût d’absence plus significatif.
# Une personne en CDD ou intérim peut être plus facilement remplacée, 
# donc un impact économique moindre, ou du moins plus souple.



# Construction de la population des femmes concernées ----------------

profil <- expand.grid(
  Gravite = gravite$Gravite,
  Statut_Professionnel = statut_pro$Statut_Professionnel,
  Categorie_SocioPro = csp$Categorie_SocioPro,
  Type_Contrat = contrat$Type_Contrat
) |>
  left_join(gravite, by = "Gravite") |>
  left_join(statut_pro, by = "Statut_Professionnel") |>
  left_join(csp, by = "Categorie_SocioPro") |>
  left_join(contrat, by = "Type_Contrat") |>
  rename(
    P_Gravite = Pourcentage.x,       
    P_Statut = Pourcentage.y,        
    P_CSP = Pourcentage.x.x,         
    P_Contrat = Pourcentage.y.y      
  ) |>
  mutate(
    P_Total = P_Gravite * P_Statut * P_CSP * P_Contrat / 100^4,  # Calcul de la pondération
    Femmes_Profil = round(nb_femmes_endo * P_Total)  # Estimation du nombre de femmes par profil
  )



# Ajout du coût unitaire modulé en fonction des caractéristiques (via coef) -----------------------
profil_base_cout <- profil |>
  mutate(
    Base_Cout = case_when(
      Gravite == "Légère" ~ 1000,   
      Gravite == "Modérée" ~ 2000,  
      Gravite == "Grave" ~ 3000,    
      TRUE ~ 0  # Coût si absence d'endométriose
    ),
    Cout_Unitaire_Mod = round(Base_Cout * Coef_Statut * Coef_CSP * Coef_Contrat),  # Coût unitaire modulé
    Cout_Total = Cout_Unitaire_Mod * Femmes_Profil  # Calcul du coût total pour chaque profil
  )



# Total coûts par gravité -------------------------
profil_sum <- profil_base_cout |>
  group_by(Gravite) |>
  summarise(
    Femmes = sum(Femmes_Profil), 
    Cout_Total = sum(Cout_Total),  
    .groups = "drop"
  ) |>
  mutate(
    Femmes = format(Femmes, big.mark = " ", scientific = FALSE),
    Cout_Total = paste0(format(Cout_Total, big.mark = " ", scientific = FALSE), " €") 
  )

#On voit dans le tableau que les femmes qui ont une endométriose légère coûtent bien moins cher.
#On pourrait faire une estimation de dépistage qui permettrait d'éviter de tendre l'endométriose vers 
#grave

#et ensuite comparer les deux totaux.



