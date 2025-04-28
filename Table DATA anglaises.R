# -----------------------------------------------------------------------------#
###########################  Table DATA anglaises  #################################
# -----------------------------------------------------------------------------#

# Packages-------------
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyverse)



# Données profil des femmes ----------
data_anglaises_endom <- read_excel("data_anglaises_endom.xlsx", sheet = "Table_2", skip = 5)
View(data_anglaises_endom)



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


data_travail_2023_femmes |> select(csp) |> distinct()

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




##    dNiveau d'étude ----------------












#visualisation diplôme
data_anglaises_endom_clean |> 
  filter(category == "highest level of qualification") |> 
  arrange(desc(n_endo))

data_anglaises_endom_clean |> 
  filter(category == "highest level of qualification") |> 
  ggplot(aes(x = reorder(subcategory, n_endo), y = n_endo)) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(x = "Niveau de diplôme", y = "Nombre de cas endométriose", title = "Cas par niveau de diplôme")


# Data endométriose-----------------



