# -----------------------------------------------------------------------------#
###########################  Table DATA anglaises  #################################
# -----------------------------------------------------------------------------#

# Packages-------------
library(ggplot2)
library(dplyr)
library(readr)
library(ipfp)
library(readxl)
library(tidyverse)



# Données profil des femmes ----------
data_anglaises_endom <- read_excel("data_anglaises_endom.xlsx", sheet = "Table_2", skip = 5)

data_travail_2023 <- read_csv2("data_travail_2023.csv")



# Nettoyage --------------------
data_anglaises_endom_clean <- data_anglaises_endom |> 
  rename(
    category = Category,
    subcategory = Subcategory,
    n_endo = `Count, with an endometriosis diagnosis`,
    perc_endo = `Percentage, with an endometriosis diagnosis`,
    n_no_endo = `Count, no endometriosis diagnosis`,
    perc_no_endo = `Percentage, no endometriosis diagnosis`,
    perc_total = `Percentage, total`
  ) |> 
  mutate(
    category = tolower(category),
    subcategory = tolower(subcategory)
  ) |> 
  filter(
    category %in% c("age on census day", "ethnic group (detailed)", 
                    "household ns-sec", "highest level of qualification", "main language")
  )

# View(data_anglaises_endom



# Création table de comparaison UK-Fr ------------------------
##    CSP  ----------------

data_anglaises_nssec <- data_anglaises_endom_clean |> 
  filter(category == "household ns-sec") |> 
  select(subcategory, n_endo, perc_endo, perc_total)


mapping_csp_nssec <- tibble(
  nssec_class = c(
    "class 1", "class 2", "class 3", "class 4", 
    "class 5", "class 6", "class 7", "class 8", 
    "students", "not classified"
  ),
  csp_france = c(
    "Cadres supérieurs",         
    "Professions intermédiaires",  
    "Employés",                   
    "Artisans, commerçants",      
    "Ouvriers qualifiés",         
    "Ouvriers non qualifiés",       
    "Ouvriers non qualifiés",       
    "Inactifs",                    
    "Étudiants",                   
    "Non classé"                   
  )
)

data_anglaises_nssec_mapped <- data_anglaises_nssec |> 
  left_join(mapping_csp_nssec, by = c("subcategory" = "nssec_class"))



##    Niveau d'étude ----------------

data_anglaises_educ <- data_anglaises_endom_clean |> 
  filter(category == "highest level of qualification") |> 
  select(subcategory, n_endo, perc_endo, perc_total)



mapping_educ_nssec <- tibble(
  educ_level_uk = c(
    "no academic or professional qualifications",
    "level 1",
    "level 2",
    "apprenticeship",
    "level 3",
    "level 4 and above",
    "other qualifications",
    "not classified"
  ),
  educ_level_fr = c(
    "Aucun diplôme",            
    "Brevet / CAP / BEP",        
    "Bac général ou pro",        
    "CAP/BEP avec apprentissage",
    "Bac+1 / Bac+2",             
    "Licence, Master, Doctorat",
    "Autres diplômes",           
    "Non classé"                 
  )
)


data_anglaises_educ_mapped <- data_anglaises_educ |> 
  left_join(mapping_educ_nssec, by = c("subcategory" = "educ_level_uk"))

#View(data_anglaises_educ_mapped)



## Age --------------

data_anglaises_age <- data_anglaises_endom_clean |> 
  filter(category == "age on census day") |> 
  select(subcategory, n_endo, perc_endo, perc_total)

mapping_age <- tibble(
  age_uk = c(
    "0 to 9 years",
    "10 to 14 years",
    "15 to 19 years",
    "20 to 24 years",
    "25 to 29 years",
    "30 to 34 years",
    "35 to 39 years",
    "40 to 44 years",
    "45 to 49 years",
    "50 to 54 years",
    "55 to 59 years",
    "60 to 64 years",
    "65 to 69 years",
    "70 to 74 years",
    "75 to 79 years",
    "80 years and over"
  ),
  tranche_age_fr = c(
    "0-9 ans",
    "10-14 ans",
    "15-19 ans",
    "20-24 ans",
    "25-29 ans",
    "30-34 ans",
    "35-39 ans",
    "40-44 ans",
    "45-49 ans",
    "50-54 ans",
    "55-59 ans",
    "60-64 ans",
    "65-69 ans",
    "70-74 ans",
    "75-79 ans",
    "80 ans et plus"
  )
)

data_anglaises_age_mapped <- data_anglaises_age |> 
  left_join(mapping_age, by = c("subcategory" = "age_uk"))

#View(data_anglaises_age_mapped)




data_anglaises_age_mapped
data_anglaises_educ_mapped
data_anglaises_nssec_mapped





# Data INSEE ----------------

## Nettoyage --------------------

data_travail_2023_femmes <- data_travail_2023 |> 
  filter(SEX == "F") |> 
  rename(age = AGE,
         emploi = EMPFORM,
         statut = EMPSTA,
         csp = PCS,
         educ = EDUC,
         activity = ACTIVITY,
         sous_emploi = UNDEREMP,
         duree_chom = UNEMPDUR,
         halo_chom = COMPOHALO,
         chom_tot = ANCSORFI2,
         immigration = IMMI
  ) |> 
  mutate(OBS_VALUE = as.numeric(OBS_VALUE))

#View(data_travail_2023_femmes)



# Nettoyage de data_travail_2023_femmes


library(dplyr)

data_femmes <- data_travail_2023 |> 
  filter(SEX == "F")


# Selon l'âge
age_summary <- data_femmes |> 
  group_by(AGE) |> 
  summarise(
    nb_femmes = sum(UNIT_MULT, na.rm = TRUE)
  ) |> 
  mutate(
    pourcentage = nb_femmes / sum(nb_femmes) * 100
  )
age_mapping <- tibble::tibble(
  AGE = c(
    "Y15", "Y15T19", "Y15T24", "Y15T29", "Y15T64", "Y15T74", "Y15T89", 
    "Y16", "Y17", "Y18", "Y19", "Y20", "Y21", "Y22", "Y23", "Y24", "Y25", "Y26", "Y27", "Y28", "Y29"
    # etc (on peut continuer avec tous tes âges disponibles)
  ),
  tranche_age = c(
    "15 ans", "15-19 ans", "15-24 ans", "15-29 ans", "15-64 ans", "15-74 ans", "15-89 ans",
    "16 ans", "17 ans", "18 ans", "19 ans", "20 ans", "21 ans", "22 ans", "23 ans", "24 ans", "25 ans", "26 ans", "27 ans", "28 ans", "29 ans"
    # etc
  )
)


# Selon la CSP (PCS)
csp_summary <- data_femmes |> 
  group_by(PCS) |> 
  summarise(
    nb_femmes = sum(UNIT_MULT, na.rm = TRUE)
  ) |> 
  mutate(
    pourcentage = nb_femmes / sum(nb_femmes) * 100
  )
csp_mapping <- tibble::tibble(
  PCS = c("10", "20", "21", "22", "23", "2_NP", "30", "31", "33", "34"),
  csp_simplifiee = c(
    "Agriculteurs exploitants", 
    "Artisans", 
    "Commerçants", 
    "Chefs d'entreprise", 
    "Cadres supérieurs", 
    "Professions libérales", 
    "Professions intermédiaires", 
    "Employés administratifs", 
    "Employés de commerce", 
    "Personnel de service"
  )
)


# Selon type de contrat (EMPFORM)
contrat_summary <- data_femmes |> 
  group_by(EMPFORM) |> 
  summarise(
    nb_femmes = sum(UNIT_MULT, na.rm = TRUE)
  ) |> 
  mutate(
    pourcentage = nb_femmes / sum(nb_femmes) * 100
  )

contrat_mapping <- tibble::tibble(
  EMPFORM = c("11", "2", "211", "22", "23T25", "26", "271", "_T"),
  type_contrat = c(
    "CDI", 
    "CDD", 
    "Apprentissage", 
    "Intérim", 
    "Autre contrat court", 
    "Stage", 
    "Contrat aidé", 
    "Non renseigné"
  )
)


# Selon statut d'emploi (EMPSTA)
statut_summary <- data_femmes |> 
  group_by(EMPSTA) |> 
  summarise(
    nb_femmes = sum(UNIT_MULT, na.rm = TRUE)
  ) |> 
  mutate(
    pourcentage = nb_femmes / sum(nb_femmes) * 100
  )
statut_mapping <- tibble::tibble(
  EMPSTA = c("1", "2", "3", "_T"),
  statut_emploi = c(
    "Salarié", 
    "Indépendant", 
    "Aide familial", 
    "Non renseigné"
  )
)


# Selon niveau d'éducation (EDUC)
education_summary <- data_femmes |> 
  group_by(EDUC) |> 
  summarise(
    nb_femmes = sum(UNIT_MULT, na.rm = TRUE)
  ) |> 
  mutate(
    pourcentage = nb_femmes / sum(nb_femmes) * 100
  )

education_mapping <- tibble::tibble(
  EDUC = c("0T2", "3A", "3_X_353", "4T5", "6T8", "_T"),
  niveau_educ = c(
    "Aucun diplôme / CAP-BEP", 
    "Bac", 
    "Bac", 
    "Bac+2 à Bac+5", 
    "Master, Doctorat", 
    "Non renseigné"
  )
)




