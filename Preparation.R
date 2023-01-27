# Importation de la table consommation
conso <-  read.csv("bilan-electrique-transpose.csv",
                  header = TRUE,sep = ";",encoding = "UTF-8")

# Trie de la table par jour et changement de noms de variable
conso_trier <- conso %>% arrange(Jour) %>% 
  rename(Puissance_moy = Puissance.moyenne.journalière..W.,
         Categorie.client = Catégorie.client )

# Puissance en GWh
conso_trier <- mutate(conso_trier, 
                      Puissance.moy = round(Puissance_moy/1e9,2))  %>% select(-Puissance_moy)


# Importation données sur la température
bilan <-  read.csv("bilan-electrique-jour.csv",
                   header = TRUE, sep=";")

# Modification des noms de variables
bilan <- bilan %>% rename(Temperature = TempÃ.rature.moyenne.journaliÃ.re.rÃ.alisÃ.e.lissÃ.e..Â.C., 
                          Jour = ï..Jour)
