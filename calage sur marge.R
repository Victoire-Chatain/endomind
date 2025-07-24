# -----------------------------------------------------------------------------#
########################### Calage sur marge  #################################
# -----------------------------------------------------------------------------#

# Packages-------------
library(ggplot2)
library(dplyr)
library(readr)
library(ipfp)
library(readxl)
library(tidyverse)



# Données profil des femmes ----------


data_travail_2023 <- read_csv2("data_travail_2023.csv")



# Nettoyage --------------------

## INSEE---------------
colnames(data_travail_2023)
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








