library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(forecast)
library(lubridate)
library(dplyr)
#library(fpp2)

# Importation de la table

conso =  read.csv("bilan-electrique-transpose.csv", header = TRUE,sep = ";")
conso_trier = conso %>% arrange(Jour) %>% 
  rename(Puissance_moy = Puissance.moyenne.journalière..W. )

# Conso avant et après covid

conso_train = conso_trier %>% filter(Jour <= "2020-03-17")
vrai_conso = conso_trier %>% filter(Jour >= "2020-03-18" & Jour <= "2021-12-30" )


# Tableau des consommations avant la covid par Catégories clients

conso_entrep =  conso_train %>% filter(Catégorie.client == "Entreprises" )
conso_resid =  conso_train %>% filter(Catégorie.client == "Résidentiels" )
conso_profe =  conso_train %>% filter(Catégorie.client == "Professionnels" )
conso_pme_pmi =  conso_train %>% filter(Catégorie.client == "PME / PMI" )
conso_pmjct =  conso_train %>%
  filter(Catégorie.client == "Puissance moyenne journalière des consommations télérelevées à courbe de charge (W)" )
conso_pmjch =  conso_train %>%
  filter(Catégorie.client == "Puissance moyenne journalière des consommations HTA télérelevées (W)" )


# Tableau des consommations après la covid par Catégories clients
  
vrai_conso_entrep = vrai_conso %>% filter(Catégorie.client =="Entreprises" )
vrai_conso_resid =  vrai_conso %>% filter(Catégorie.client == "Résidentiels" )
vrai_conso_profe =  vrai_conso %>% filter(Catégorie.client == "Professionnels" )
vrai_conso_pme_pmi =  vrai_conso %>% filter(Catégorie.client == "PME / PMI" )
vrai_conso_pmjct =  vrai_conso %>%
  filter(Catégorie.client == "Puissance moyenne journalière des consommations télérelevées à courbe de charge (W)" )
vrai_conso_pmjch =  vrai_conso %>%
  filter(Catégorie.client == "Puissance moyenne journalière des consommations HTA télérelevées (W)" )


# Fonction pour la prédiction avec arima
# Les ordres de arima trouvé à partir de la fonction auto.arima 

Estim = function(table1,table2){
  fit.arima = arima(table1 %>% select(Puissance_moy), order = c(1,0,2),
                    list(order = c(2,1,0), period=7), include.mean = FALSE,
                    method= "CSS-ML")
  fcst = forecast(fit.arima,h=653)
  tab_predit = as.numeric(fcst$mean)
  ICupper <- as.numeric(fcst$upper[,2]) # à 95%
  IClower <- as.numeric(fcst$lower[,2]) # à 95%
  tab = cbind(table2,tab_predit,ICupper,IClower)
  tab_f = tab %>%
    mutate(difference = (Puissance_moy - tab_predit))
  return(tab_f)
}

# Application de la fonction Estim
entreprise = Estim(conso_entrep,vrai_conso_entrep)
professionnel = Estim(conso_profe,vrai_conso_profe)
residentiel = Estim(conso_resid,vrai_conso_resid)
pme_pmi = Estim(conso_pme_pmi,vrai_conso_pme_pmi)
pmjch = Estim(conso_pmjch,vrai_conso_pmjch)
pmjct = Estim(conso_pmjct,vrai_conso_pmjct)

#
complete = rbind(entreprise,professionnel,residentiel,
                 pme_pmi,pmjch,pmjct)

vect_categorie = unique(conso$Catégorie.client)


# Define UI for application 
ui <- fluidPage(
  useShinydashboard(),
  
  # Application title
  titlePanel("Impact de la crise sanitaire sur les consommations d’électricité"),
  
  # Sidebar with a slider input for categories
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "categorie",
                  label = "Choisissez la catégorie",
                  choices = vect_categorie,
                  selected = "Entreprises"),
      
      # Select date range to be plotted
      dateRangeInput("date", strong("Choissisez une période"),
                     start = "2020-03-17", end = "2020-07-30",
                     min = "2020-03-17", max = "2021-12-30"
      ),
      
      # Bouton download
      downloadButton("downloadData", "Download")
      
    ),
    
    mainPanel(
      textOutput({"categorie_aff"}),
      
      dataTableOutput({"mon_df"}),
      
      plotOutput(outputId = "lineplot", height = "300px"),
      textOutput(outputId = "desc"),
      
      valueBoxOutput("vbox1", width = 8),
      valueBoxOutput("vbox2", width = 8),
      valueBoxOutput("vbox3", width = 8)
      
    ),  
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$categorie_aff <- renderText({input$categorie})
  
  ## Datatable
  get_vrai_table <- reactive({
    complete %>%
      filter(complete$Catégorie.client == input$categorie,
             Jour >= as.POSIXct(input$date[1]) & Jour <= as.POSIXct(input$date[2]
             )) %>% select(-Catégorie.client)
  })
  output$mon_df <- renderDataTable(({get_vrai_table()[,-c(4,5)]}))
  
  ## Graphique
  output$lineplot <- renderPlot({
    ggplot(data = get_vrai_table(), aes(x=Jour %>% yday())) +
      geom_line(aes(y = Puissance_moy), color = "black") +
      geom_line(aes(y = tab_predit), color = "red") +
      geom_line(aes(y = ICupper), color="blue", linetype="dashed") +
      geom_line(aes(y = IClower), color="blue", linetype="dashed")
    
  })
  
  ## Valuebox
  output$vbox1 <- shinydashboard::renderValueBox({
    sp <- sum(get_vrai_table()[3])
    shinydashboard::valueBox(sp, "Somme prédite en GWh")
  })

  output$vbox2 <- shinydashboard::renderValueBox({
    sr <- sum(get_vrai_table()[2])
  shinydashboard::valueBox(sr, "Somme réalisée en GWh")
  })

  output$vbox3 <- shinydashboard::renderValueBox({
    Impact <- sum(get_vrai_table()[2]) - sum(get_vrai_table()[3])
  shinydashboard::valueBox(Impact, "Impact en GWh")
  })

  ## Bouton dowload
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(get_vrai_table(), con)
    }
  )

}

# Run the application
shinyApp(ui = ui, server = server)

