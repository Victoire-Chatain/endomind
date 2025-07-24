# -----------------------------------------------------------------------------#
############ ESTIMATION des coûts différenciés selon les femmes ################
# -----------------------------------------------------------------------------#



# Packages ---------
library(writexl)
library(readxl)
library(ggplot2)
library(dplyr)
library(stargazer)


# Data -----------

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


# Simulation -------
set.seed(123) 

# Taille de l'échantillon
n <- 1986

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


# Data frame endo_data -----
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


# Réprésentation avec %

endo_data |> 
  count(Gravite_Endometriose) |> 
  mutate(Percentage = n / sum(n) * 100) -> gravite_percentages

gravite_statut_percentages <- endo_data |> 
  count(Statut_Pro, Gravite_Endometriose) |>      
  group_by(Gravite_Endometriose) |>               
  mutate(Percentage = n / sum(n) * 100)       



ggplot(gravite_percentages, aes(x = "", y = Percentage, fill = Gravite_Endometriose)) +
  geom_bar(stat = "identity", color = "black", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = endo_colors) +
  labs(title = "Répartition des répondantes selon la gravité de l'endométriose") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(), 
        plot.title = element_text(color = endo_colors[1], size = 16)) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5))


     
ggplot(gravite_statut_percentages, aes(x = Gravite_Endometriose, y = Percentage, fill = Statut_Pro)) +
  geom_bar(stat = "identity", position = "dodge") +  
  scale_fill_manual(values = endo_colors) +           
  labs(title = "Répartition des métiers par gravité de l'endométriose", 
       x = "Gravité de l'endométriose", y = "Pourcentage des métiers") +
  theme_minimal() +
  theme(plot.title = element_text(color = "black", size = 16)) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),   
            position = position_dodge(width = 0.9), vjust = -0.25)   



ggplot(gravite_statut_percentages, aes(x = Gravite_Endometriose, y = Percentage, fill = Statut_Pro)) +
  geom_bar(stat = "identity") +  
  scale_fill_manual(values = endo_colors) +  
  labs(title = "Répartition des métiers par gravité de l'endométriose", 
       x = "Gravité de l'endométriose", y = "Pourcentage des métiers") +
  theme_minimal() +
  theme(plot.title = element_text(color = "black", size = 16)) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),   
            position = position_stack(vjust = 0.5)) 


###########################################

# Création du graphique avec des barres côte à côte pour chaque statut et gravité de l'endométriose
ggplot(gravite_statut_percentages, aes(x = Statut_Pro, y = Percentage, fill = Gravite_Endometriose)) +
  geom_bar(stat = "identity", position = "dodge") +    # Barres côte à côte pour chaque statut
  scale_fill_manual(values = endo_colors) +             # Utilisation des couleurs pour chaque gravité
  labs(title = "Répartition de l'endométriose par gravité et statut professionnel", 
       x = "Statut Professionnel", y = "Pourcentage de répondantes") +
  theme_minimal() +
  theme(plot.title = element_text(color = "black", size = 16)) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),   
            position = position_dodge(width = 0.9), vjust = -0.25)  


# A partir de ces données on passe à la matrice

# Matrice ----------


# Données CSP / gravité
csp_percentages <- data.frame(
  Statut_Pro = c("Cadre", "Cadre", "Cadre", "Employée", "Employée", "Employée", "Intermédiaire", "Intermédiaire", "Intermédiaire"),
  Gravite_Endometriose = c("Grave", "Légère", "Modérée", "Grave", "Légère", "Modérée", "Grave", "Légère", "Modérée"),
  Percentage = c(31.7, 9.0, 59.3, 30.6, 7.68, 61.7, 24.6, 11.6, 63.8)
)



# Coefficients CSP
coeff_csp <- data.frame(
  Statut_Pro = c("Cadre", "Intermédiaire", "Employée"),
  Coeff = c(1.15, 1.1, 1.0)
)

# Justification :
      # une cadre aurait plus d'occasion de télétravail
      # mais d'un autre côté, on suppose que son travail plus productif en termes de valeur
      # perdre 1h de travail d'un cadre couterait + cher qu'1h d'un employé
      # Donc on estime de coefficient
  # de même pour les autres avec différentes raisons et motifs...


# La fixation de coefficients est arbitraire
# Il faut encore préciser, il y a 9 cas différents pour l'instant, 
# mais on pourrait ajouter les différentes catégories de pertes de productivités


# Coût unitaire moyen par gravité
couts <- data.frame(
  Gravite_Endometriose = c("Légère", "Modérée", "Grave"),
  Cout_Unitaire = c(1260, 2520, 3780)
)

# Montants inspirés de la littérature

# Nombre de femmes par gravité
nb_femmes <- data.frame(
  Gravite_Endometriose = c("Légère", "Modérée", "Grave"),
  Nb_Femmes = c(164250, 1112200, 547550)  # 73% du total initial qui était (225000, 1525000, 750000)
)

# Sachant qu'on réduit déja les nombres avec les 73% de l'endométriose handicapante 
# Selon une enquête Endovie.


# Source INSEE + Endotravail + Endovie-EndoFrance


# Fusionner les données
df <- merge(csp_percentages, coeff_csp, by = "Statut_Pro")
df <- merge(df, couts, by = "Gravite_Endometriose")
df <- merge(df, nb_femmes, by = "Gravite_Endometriose")


# Calcul coût total par groupe
df$Cout_Total <- df$Nb_Femmes * (df$Percentage / 100) * df$Coeff * df$Cout_Unitaire

df[, c("Gravite_Endometriose", "Statut_Pro", "Percentage", "Coeff", "Cout_Unitaire", "Nb_Femmes", "Cout_Total")]


# On note des approfondissements à faire dans les calculs des coefficients, et la quantité de catégories.
# On a encore des données non utilisées pour cette matrice, notamment des sources 

# Endolife
# Enquête anglaise




# Recalcul des pourcentages sur l'ensemble de la base
gravite_statut_percentages_global <- endo_data |> 
  count(Statut_Pro, Gravite_Endometriose) |> 
  mutate(Percentage = n / sum(n) * 100)


# Total estimé de la population concernée (somme des femmes)
total_femmes <- sum(nb_femmes$Nb_Femmes)


# Fusionner les données globales
df <- merge(gravite_statut_percentages_global, coeff_csp, by = "Statut_Pro")
df <- merge(df, couts, by = "Gravite_Endometriose")
df <- merge(df, nb_femmes, by = "Gravite_Endometriose")


# Coût total basé sur pourcentage global appliqué à la population totale
df$Cout_Total <- total_femmes * (df$Percentage / 100) * df$Coeff * df$Cout_Unitaire


df[, c("Gravite_Endometriose", "Statut_Pro", "Percentage", "Coeff", "Cout_Unitaire", "Cout_Total")]


sum(df$Cout_Total)





