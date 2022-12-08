library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(forecast)
library(cowplot)
library(lubridate)
library(dplyr)
library(fpp2)

conso =  read.csv("C:/Users/sguest/Desktop/R av/bilan-electrique-transpose.csv",header = TRUE,sep = ";",encoding = "UTF-8")

conso_trier = conso %>% arrange(Jour)

conso_train = conso_trier %>% filter(Jour <= "2020-03-17")

vrai_conso = conso_trier %>% filter(Jour >= "2020-03-18" & Jour <= "2021-12-30" )

vect_categorie = unique(conso$Catégorie.client)

################################################################################
#  Fonction pour la prédiction

Estimation = function(table,categorie_client){
  ts = ts( table %>% select(Puissance.moyenne.journalière..W.),
           start= c(table$Jour[1] %>% year(),1),frequency = 7 )
  fit.arima = auto.arima(ts,d=0,D=1,stepwise = FALSE,
                         approximation = FALSE,trace = TRUE)
  fcst = forecast(fit.arima,h=653)
  tab_predit = fcst$mean
  return(tab_predit)
}

################################################################################

# Fonction get_table

get_table = function(data1, data2){
  data3 <- cbind(data1,data2)
  data <- cbind(data3,data3[2]-data[3])
  return(data)
}
################################################################################



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
                       start = "2020-03-17", end = "2021-12-30",
                       min = "2020-03-17", max = "2021-12-30"),
        
        downloadButton("downloadData", "Download")
        
      ),
      
     
      mainPanel(
        textOutput({"categorie_aff"}),
    
        dataTableOutput({"mon_df"}),
        
        plotOutput(outputId = "lineplot", height = "300px"),
        textOutput(outputId = "desc"),
        
        #dataTableOutput({"mon_df"}),
        #plotOutput({"repartition"})
        valueBoxOutput("vbox1", width = 2),
        valueBoxOutput("vbox2", width = 2),
        valueBoxOutput("vbox3", width = 2)
        
        
      ),  
      
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$categorie_aff <- renderText({input$categorie})
  
  ####
  get_vrai_table <- reactive({
    
     vrai_conso %>%
      filter(vrai_conso$Catégorie.client == input$categorie, 
             Jour >= as.POSIXct(input$date[1]) & Jour <= as.POSIXct(input$date[2]
             )) %>% 
      select(-Catégorie.client)

  })
  

  
  output$mon_df <- renderDataTable(({get_vrai_table()}))

 
  output$lineplot <- renderPlot({
    #plot(x = get_vrai_table()$Jour %>% yday(), y = get_vrai_table()$Puissance.moyenne.journalière..W.,
     #    xlab = "Date", ylab = "Trend index")
    ggplot(data = get_vrai_table()) +
        aes(y = Puissance.moyenne.journalière..W. ,  x= Jour%>% yday()) +
        geom_line() + xlab("Jour") + ylab("Consommation")
    
  })
  
 
  output$vbox1 <- shinydashboard::renderValueBox({ 
    sp <- 42 # Valeur à modifier par la suite
    shinydashboard::valueBox( sp, "Somme prédite")
  })
  
  output$vbox2 <- shinydashboard::renderValueBox({ 
    sr <- 40 # Valeur à modifier par la suite
    shinydashboard::valueBox( sr, "Somme réalisée")
  })
  
  output$vbox3 <- shinydashboard::renderValueBox({ 
    Impact <- 2 # Valeur à modifier par la suite
    P_Im <- 2/100

    shinydashboard::valueBox( Impact, "Impact")
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(data, con)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

