#Simuler des données pour ENDOMIND#



# Packages-------------

# Charger le package
library(writexl)
library(readxl)
library(ggplot2)
library(dplyr)
library(stargazer)


# Générer le jeu de données-----------

# Statut professionnel
statut_pro <- data.frame(
  Statut_Professionnel = c("En emploi", "Au chômage", "En formation/études", "Arrêt longue maladie", "Inactive"),
  Pourcentage = c(78, 8, 4, 5, 3)
)

# Catégories socio-professionnelles
csp <- data.frame(
  Categorie_SocioPro = c("Cadres", "Intermédiaires", "Employées", "Indépendantes"),
  Pourcentage = c(23, 10, 57, 8)
)

# Type de contrat
contrat <- data.frame(
  Type_Contrat = c("CDI ou assimilé", "CDD, intérim...", "Non précisé"),
  Pourcentage = c(72, 21, 7)
)

# Gravité de l’endométriose
gravite <- data.frame(
  Gravite_Endometriose = c("Légère", "Modérée", "Grave"),
  Pourcentage = c(9, 61, 30)
)



doc <- write_xlsx(
  list(
    "Statut_Pro" = statut_pro,
    "Categorie_SocioPro" = csp,
    "Type_Contrat" = contrat,
    "Gravite_Endometriose" = gravite
  ),
  path = "Endotravail_synthese.xlsx"
)



data_statut <- read_excel("Endotravail_synthese.xlsx", sheet = "Statut_Pro")

#View(data_statut)







#Simulation---------


## Taille de l'échantillon---------
n <- 1986

## Génération des variables--------------------

set.seed(123) 

# Statut socio-professionnel
statut_pro <- sample(
  c("Cadre", "Intermédiaire", "Employée"),
  size = n,
  replace = TRUE,
  prob = c(0.23, 0.10, 0.57)
)

# Type de contrat
type_contrat <- sample(
  c("CDI", "Précaire", "Indépendante"),
  size = n,
  replace = TRUE,
  prob = c(0.72, 0.21, 0.08)
)

# Situation d'emploi
situation_emploi <- sample(
  c("En emploi", "Chômage", "Formation/Études", "Arrêt maladie", "Inactive"),
  size = n,
  replace = TRUE,
  prob = c(0.78, 0.08, 0.04, 0.05, 0.03)
)

# Gravité de l'endométriose
gravite <- sample(
  c("Légère", "Modérée", "Grave"),
  size = n,
  replace = TRUE,
  prob = c(0.09, 0.61, 0.30)
)

# Âge des répondantes (moyenne 34, entre 16 et 58 ans)
age <- round(rnorm(n, mean = 34, sd = 6))
age[age < 16] <- 16
age[age > 58] <- 58

# Délai de diagnostic : moyenne 9 ans
delai_diagnostic <- round(rnorm(n, mean = 9, sd = 3))
delai_diagnostic[delai_diagnostic < 1] <- 1

# Âge au diagnostic
age_diagnostic <- age - delai_diagnostic
age_diagnostic[age_diagnostic < 12] <- 12


## data frame -----
endo_data <- data.frame(
  ID = 1:n,
  Age = age,
  Age_Diagnostic = age_diagnostic,
  Delai_Diagnostic = delai_diagnostic,
  Statut_Pro = statut_pro,
  Type_Contrat = type_contrat,
  Situation_Emploi = situation_emploi,
  Gravite_Endometriose = gravite
)


colnames(endo_data)

















# Visualisation-------

endo_colors <- c("#95364e", "#d5a2ae", "#ddae4d")




ggplot(endo_data, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = endo_colors[1], color = "black") +
  labs(title = "Distribution de l'âge des répondantes", x = "Âge", y = "Fréquence") +
  theme_minimal()

ggplot(endo_data, aes(x = Delai_Diagnostic)) +
  geom_histogram(binwidth = 0.5, fill = endo_colors[1], color = "black") +
  labs(title = "Distribution du délai de diagnostic de l'endométriose", 
       x = "Délai de diagnostic (en années)", y = "Fréquence") +
  theme_minimal()



ggplot(endo_data, aes(x = "", fill = Gravite_Endometriose)) +
  geom_bar(width = 1, stat = "count", color = "black") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = endo_colors) +
  labs(title = "Répartition des répondantes selon la gravité de l'endométriose") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(), 
        plot.title = element_text(color = endo_colors[1], size = 16))



ggplot(endo_data, aes(x = Gravite_Endometriose, fill = Statut_Pro)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = endo_colors) +
  labs(title = "Gravité de l'endométriose par statut professionnel", 
       x = "Gravité de l'endométriose", y = "Nombre de répondantes") +
  theme_minimal() +
  theme(plot.title = element_text(color = "black", size = 16))








#avec %

endo_data |> 
  count(Gravite_Endometriose) |> 
  mutate(Percentage = n / sum(n) * 100) -> gravite_percentages

ggplot(gravite_percentages, aes(x = "", y = Percentage, fill = Gravite_Endometriose)) +
  geom_bar(stat = "identity", color = "black", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = endo_colors) +
  labs(title = "Répartition des répondantes selon la gravité de l'endométriose") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(), 
        plot.title = element_text(color = endo_colors[1], size = 16)) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5))



# Calcul des pourcentages de chaque métier dans chaque niveau de gravité
gravite_statut_percentages <- endo_data |> 
  count(Statut_Pro, Gravite_Endometriose) |>      
  group_by(Gravite_Endometriose) |>               
  mutate(Percentage = n / sum(n) * 100)            

# Création du graphique
ggplot(gravite_statut_percentages, aes(x = Gravite_Endometriose, y = Percentage, fill = Statut_Pro)) +
  geom_bar(stat = "identity", position = "dodge") +  
  scale_fill_manual(values = endo_colors) +           
  labs(title = "Répartition des métiers par gravité de l'endométriose", 
       x = "Gravité de l'endométriose", y = "Pourcentage des métiers") +
  theme_minimal() +
  theme(plot.title = element_text(color = "black", size = 16)) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),   
            position = position_dodge(width = 0.9), vjust = -0.25)   















# Import data INSEE-----------


demo_insee <- read_excel("demo_insee.xlsx")

emploi_insee <- read_excel("emploi_insee.xlsx", sheet = 2)

colnames(emploi_insee)
colnames(emploi_insee_femmes)

emploi_insee_femmes <- emploi_insee |> 
  slice(-1) |> 
  select(1,3) 


#View(emploi_insee_femmes)






#super, maintenant  on va les lier avec l'ensemble de la population


prevalence_endo <- 0.10



# Ajouter une colonne "Endometriose" à notre dataframe en fonction de la prévalence

population_feminine_france$Endometriose <- sample(c("Oui", "Non"), 
                                                  n, 
                                                  replace = TRUE, 
                                                  prob = c(prevalence_endo, 1 - prevalence_endo))




# Visualiser un échantillon des données avec la colonne "Endometriose"
head(population_feminine_france)

# 1. Visualiser la répartition des femmes atteintes d'endométriose par région
ggplot(population_feminine_france, aes(x = Region, fill = Endometriose)) +
  geom_bar(position = "fill") +  # Utilisation de "fill" pour visualiser les proportions
  scale_fill_manual(values = c("#95364e", "#d5a2ae")) +
  labs(title = "Répartition des femmes atteintes d'endométriose par région", 
       x = "Région", 
       y = "Proportion de femmes") +
  theme_minimal() +
  theme(plot.title = element_text(color = "#95364e", size = 16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Visualiser la répartition des femmes atteintes d'endométriose par niveau d'éducation
ggplot(population_feminine_france, aes(x = Niveau_Education, fill = Endometriose)) +
  geom_bar(position = "fill") +  # Utilisation de "fill" pour visualiser les proportions
  scale_fill_manual(values = c("#95364e", "#d5a2ae")) +
  labs(title = "Répartition des femmes atteintes d'endométriose par niveau d'éducation", 
       x = "Niveau d'éducation", 
       y = "Proportion de femmes") +
  theme_minimal() +
  theme(plot.title = element_text(color = "#95364e", size = 16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Visualiser la répartition des femmes atteintes d'endométriose par statut professionnel
ggplot(population_feminine_france, aes(x = Statut_Pro, fill = Endometriose)) +
  geom_bar(position = "fill") +  # Utilisation de "fill" pour visualiser les proportions
  scale_fill_manual(values = c("#95364e", "#d5a2ae")) +
  labs(title = "Répartition des femmes atteintes d'endométriose par statut professionnel", 
       x = "Statut professionnel", 
       y = "Proportion de femmes") +
  theme_minimal() +
  theme(plot.title = element_text(color = "#95364e", size = 16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#on reprend tout les graphiques

# Calcul des pourcentages par statut professionnel et gravité
gravite_statut_percentages <- endo_data %>%
  count(Statut_Pro, Gravite_Endometriose) %>%     # Comptage des occurrences par statut et gravité
  group_by(Gravite_Endometriose) %>%               # Groupement par gravité (niveau de douleur)
  mutate(Percentage = n / sum(n) * 100)            # Calcul du pourcentage dans chaque groupe de gravité



# Graphique en barres empilées pour la répartition des métiers par gravité de l'endométriose
ggplot(gravite_statut_percentages, aes(x = Gravite_Endometriose, y = Percentage, fill = Statut_Pro)) +
  geom_bar(stat = "identity") +  # Affichage des barres empilées
  scale_fill_manual(values = endo_colors) +  # Utilisation des couleurs définies
  labs(title = "Répartition des métiers par gravité de l'endométriose", 
       x = "Gravité de l'endométriose", y = "Pourcentage des métiers") +
  theme_minimal() +
  theme(plot.title = element_text(color = "black", size = 16)) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),   # Affichage des pourcentages sur les barres
            position = position_stack(vjust = 0.5))  # Positionnement du texte au centre des barres




# Création du graphique avec des barres côte à côte pour chaque statut et gravité de l'endométriose
ggplot(gravite_statut_percentages, aes(x = Statut_Pro, y = Percentage, fill = Gravite_Endometriose)) +
  geom_bar(stat = "identity", position = "dodge") +    # Barres côte à côte pour chaque statut
  scale_fill_manual(values = endo_colors) +             # Utilisation des couleurs pour chaque gravité
  labs(title = "Répartition de l'endométriose par gravité et statut professionnel", 
       x = "Statut Professionnel", y = "Pourcentage de répondantes") +
  theme_minimal() +
  theme(plot.title = element_text(color = "black", size = 16)) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),   # Affichage des pourcentages
            position = position_dodge(width = 0.9), vjust = -0.25)  # Positionner les textes au-dessus des barres

# Calcul des pourcentages de chaque gravité dans chaque statut professionnel
gravite_statut_percentages <- endo_data %>%
  count(Statut_Pro, Gravite_Endometriose) %>%       # Compte des réponses par statut et gravité
  group_by(Statut_Pro) %>%                         # Regroupement par statut professionnel
  mutate(Percentage = n / sum(n) * 100)             # Calcul des pourcentages dans chaque groupe




# Calcul des pourcentages par statut professionnel et gravité
gravite_statut_percentages <- endo_data |> 
  count(Statut_Pro, Gravite_Endometriose) |> 
  group_by(Statut_Pro) |> 
  mutate(Percentage = n / sum(n) * 100)  # Pourcentage de chaque gravité dans chaque statut professionnel




# on a cette représentation des femmes dans le monde du travail

# Cadres	19,7
# Professions intermédiaires	27,7
# Employés	38,8

# Groups:   Statut_Pro [3]
# Statut_Pro    Gravite_Endometriose     n Percentage
# <chr>         <chr>                <int>      <dbl>
#   1 Cadre         Grave                  162      31.7 
# 2 Cadre         Légère                  46       9.00
# 3 Cadre         Modérée                303      59.3 
# 4 Employée      Grave                  391      30.6 
# 5 Employée      Légère                  98       7.68
# 6 Employée      Modérée                787      61.7 
# 7 Intermédiaire Grave                   49      24.6 
# 8 Intermédiaire Légère                  23      11.6 
# 9 Intermédiaire Modérée                127      63.8






# Données CSP / gravité
csp_percentages <- data.frame(
  Statut_Pro = c("Cadre", "Cadre", "Cadre", "Employée", "Employée", "Employée", "Intermédiaire", "Intermédiaire", "Intermédiaire"),
  Gravite_Endometriose = c("Grave", "Légère", "Modérée", "Grave", "Légère", "Modérée", "Grave", "Légère", "Modérée"),
  Percentage = c(31.7, 9.0, 59.3, 30.6, 7.68, 61.7, 24.6, 11.6, 63.8)
)

# Coefficients CSP
coeff_csp <- data.frame(
  Statut_Pro = c("Cadre", "Intermédiaire", "Employée"),
  Coeff = c(1.5, 1.2, 1.0)
)

# Coût unitaire moyen par gravité
couts <- data.frame(
  Gravite_Endometriose = c("Légère", "Modérée", "Grave"),
  Cout_Unitaire = c(1260, 2520, 3780)
)

# Nombre de femmes par gravité
nb_femmes <- data.frame(
  Gravite_Endometriose = c("Légère", "Modérée", "Grave"),
  Nb_Femmes = c(225000, 1525000, 750000)
)

# Fusionner les données
df <- merge(csp_percentages, coeff_csp, by = "Statut_Pro")
df <- merge(df, couts, by = "Gravite_Endometriose")
df <- merge(df, nb_femmes, by = "Gravite_Endometriose")

# Calcul coût total par groupe
df$Cout_Total <- df$Nb_Femmes * (df$Percentage / 100) * df$Coeff * df$Cout_Unitaire

# Voir le résultat
df[, c("Gravite_Endometriose", "Statut_Pro", "Percentage", "Coeff", "Cout_Unitaire", "Nb_Femmes", "Cout_Total")]


















# Données : Proportions des femmes dans chaque catégorie professionnelle
statut_proportion <- data.frame(
  Statut_Pro = c("Cadre", "Intermédiaire", "Employée"),
  Proportion = c(0.197, 0.277, 0.388)  # Proportions des catégories professionnelles
)

# Données : Prévalence de l'endométriose selon la gravité pour chaque statut
gravite_endometriose <- data.frame(
  Statut_Pro = c("Cadre", "Employée", "Intermédiaire"),
  Grave_Percentage = c(31.7, 30.6, 24.6),   # Prévalence de l'endométriose grave
  Legere_Percentage = c(9.0, 7.68, 11.6),   # Prévalence de l'endométriose légère
  Moderee_Percentage = c(59.3, 61.7, 63.8)  # Prévalence de l'endométriose modérée
)

# Fusionner les deux dataframes sur la colonne "Statut_Pro"
data_ponderee <- left_join(statut_proportion, gravite_endometriose, by = "Statut_Pro")

# Calcul des prévalences pondérées de chaque type d'endométriose
data_ponderee <- data_ponderee |> 
  mutate(
    Prevalence_Grave = Proportion * Grave_Percentage,
    Prevalence_Legere = Proportion * Legere_Percentage,
    Prevalence_Moderee = Proportion * Moderee_Percentage
  )

# Calculer la prévalence totale pondérée pour chaque type d'endométriose
prevalence_totale_grave <- sum(data_ponderee$Prevalence_Grave)
prevalence_totale_legere <- sum(data_ponderee$Prevalence_Legere)
prevalence_totale_moderee <- sum(data_ponderee$Prevalence_Moderee)

# Afficher les résultats de la pondération
print(data_ponderee)
cat("Prévalence totale pondérée d'endométriose grave dans le monde du travail : ", prevalence_totale_grave, "%\n")
cat("Prévalence totale pondérée d'endométriose légère dans le monde du travail : ", prevalence_totale_legere, "%\n")
cat("Prévalence totale pondérée d'endométriose modérée dans le monde du travail : ", prevalence_totale_moderee, "%\n")





# Reprise 14 avril-------------

endo_data


library(ggplot2)

# Comparaison de l'âge en fonction de la gravité de l'endométriose
ggplot(endo_data, aes(x = Gravite_Endometriose, y = Age, fill = Gravite_Endometriose)) +
  geom_boxplot() +
  labs(
    title = "Comparaison de l'âge en fonction de la gravité de l'endométriose",
    x = "Gravité de l'endométriose",
    y = "Âge"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Comparaison du délai de diagnostic en fonction de la gravité de l'endométriose
ggplot(endo_data, aes(x = Gravite_Endometriose, y = Delai_Diagnostic, fill = Gravite_Endometriose)) +
  geom_boxplot() +
  labs(
    title = "Comparaison du délai de diagnostic en fonction de la gravité de l'endométriose",
    x = "Gravité de l'endométriose",
    y = "Délai de diagnostic (en années)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Barplot empilé montrant la répartition de la gravité de l'endométriose par type de contrat
ggplot(endo_data, aes(x = Type_Contrat, fill = Gravite_Endometriose)) +
  geom_bar(position = "fill") +  # position = "fill" permet de normaliser chaque barre à 1 (pourcentage)
  labs(
    title = "Répartition de la gravité de l'endométriose par type de contrat",
    x = "Type de contrat",
    y = "Proportion"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)



# Barplot montrant la répartition de la situation d'emploi en fonction de la gravité de l'endométriose
ggplot(endo_data, aes(x = Gravite_Endometriose, fill = Situation_Emploi)) +
  geom_bar(position = "fill") +  # position = "fill" pour obtenir des proportions
  labs(
    title = "Répartition de la situation d'emploi en fonction de la gravité de l'endométriose",
    x = "Gravité de l'endométriose",
    y = "Proportion"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)



# Transformation de la variable Situation_Emploi en binaire (En emploi = 1, autre = 0)
endo_data$Emploi_Binaire <- ifelse(endo_data$Situation_Emploi == "En emploi", 1, 0)

# Régression logistique
model_employment <- glm(Emploi_Binaire ~ Gravite_Endometriose + Age + Statut_Pro, data = endo_data, family = binomial)

# Résumé du modèle
summary(model_employment)


install.packages("corrplot")

library(corrplot)

# Calcul de la matrice de corrélation entre les variables numériques
cor_matrix <- cor(endo_data[, c("Age", "Delai_Diagnostic")], use = "complete.obs")

# Affichage de la heatmap de la matrice de corrélation
corrplot(cor_matrix, method = "color", tl.cex = 1, addCoef.col = "black", diag = FALSE)



