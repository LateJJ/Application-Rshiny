# Application-Rshiny
Projet : Impact de la crise sanitaire sur les consommations d’électricité.

# Deux fichiers ont été utilisé
Pour télecharger le premier fichier csv en lien avec le projet R-shiny,
il faut aller sur ce site: "https://data.enedis.fr/explore/dataset/bilan-electrique-transpose/export/"

Ensuite pour télécharger le second fichier portant sur les données de températures, 
il faut cliquer sur ce lien : "https://data.enedis.fr/explore/dataset/bilan-electrique-jour/export/?sort=jour"

# Modèle utilisé pour la prédiction
Nous avons utilisé un modèle de régression linéaire pour la prédiction de la puissance moyenne de la comsommation d'électricité
avec variable explicative température car la consommation dépend beaucoup de la température.

En premier nous avons tester un modèle ARIMA mais nous nous sommes rendu compte que la comsommation ne suit pas un ARIMA.

Nous avons de même essayer un modèle de random forest toujours avec variable explicative température, mais sur les données concernant les entreprises,
nous avons obtenu 21 % pour la variance expliquée. 
