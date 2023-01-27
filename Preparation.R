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


# Temperation
Tempe <- bilan %>% arrange(Jour) %>% select(Jour,Temperature) %>% filter(Jour <= "2021-12-31")


# Consommation avant et pendant covid

avant <- conso_trier %>% filter(Jour <= "2020-03-17"  & Jour>="2018-01-14")
pendant <- conso_trier %>% filter(Jour >= "2020-03-18" & Jour <= "2021-12-30" )

# Temperation avant et pendant covid
Temp_av_covid <- Tempe %>% filter(Jour <= "2020-03-17") %>% select(-Jour)
Temp_covid <- Tempe %>% filter(Jour >= "2020-03-18" & Jour <= "2021-12-30") %>% select(-Jour)


# Tableau des consommations avant la covid par Catégories clients
conso_avant <- function(categorie){
  conso <- avant %>% filter(Categorie.client == categorie)
  return(cbind(conso,Temp_av_covid))
}


# Tableau des consommations pendant la covid par Catégories clients

vrai_conso_covid <- function(categorie){
  conso <- pendant %>% filter(Categorie.client == categorie)
  return(cbind(conso,Temp_covid))
}

# Modèle de régression linéaire
Estim <- function(table1,table2){
  res = lm(Puissance.moy~Temperature, data=table1)
  prediction <- predict(res,newdata=Temp_covid,
                        interval = "confidence")
  prediction=as.data.frame(prediction) # prediction + IC 
  tab = cbind(table2,prediction)
  tab_f = tab %>%
    mutate(difference = (Puissance.moy - tab$fit))
  return(tab_f)
}

vect_categorie <- unique(avant$Categorie.client)

complete <- c()
for (categorie in vect_categorie){
  estim = Estim(conso_avant(categorie),vrai_conso_covid(categorie))
  complete = rbind(complete,estim)
}


