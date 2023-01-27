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
                     min = "2020-03-17", max = "2021-12-30"
      ),
      
      # Bouton download
      downloadButton("downloadData", "Download")
      
    ),
    
    mainPanel(
      textOutput({"categorie_aff"}),
    
      dataTableOutput({"mon_df"}),
    
      plotlyOutput(outputId = "lineplot", height = "300px"),
      
      valueBoxOutput("vbox1", width = 5),
      valueBoxOutput("vbox2", width = 5),
      valueBoxOutput("vbox3", width = 5),
      valueBoxOutput("vbox4", width = 5)
      
    ),  
  )
)
