
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$categorie_aff <- renderText({input$categorie})
  
  ## Datatable
  get_vrai_table <- reactive({
    complete %>%
      filter(complete$Categorie.client == input$categorie,
             Jour >= as.POSIXct(input$date[1]) & Jour <= as.POSIXct(input$date[2]
             )) %>% select(-Categorie.client)
  })
  output$mon_df <- renderDataTable(({get_vrai_table()[,-c(5,6)]}))
  
  
  output$lineplot <- renderPlotly({
    p <- plot_ly(x = get_vrai_table()$Jour %>% yday(), y = get_vrai_table()$Puissance.moy, type = "scatter", mode = "lines", name = "Valeurs réels") %>% 
      add_trace(x = get_vrai_table()$Jour %>% yday(), y = get_vrai_table()$fit, type = "scatter", mode = "lines", name = "Prédictions") %>% 
      add_trace(x = get_vrai_table()$Jour %>% yday(), y = get_vrai_table()$upr, type = "scatter", mode = "lines", name = "y = IC borne supérieure") %>% 
      add_trace(x = get_vrai_table()$Jour %>% yday(), y = get_vrai_table()$lwr, type = "scatter", mode = "lines", name = "y = IC borne inférieure")
    p<- p %>% layout(title = 'Valeurs observées et valeurs prédites pendant la covid')
    p
  })
  
  ## Valuebox
  output$vbox1 <- shinydashboard::renderValueBox({
     sp <- round(sum(get_vrai_table()["fit"]),2)
    shinydashboard::valueBox(sp, "Somme prédite en GWh", color = "orange")
  })

  output$vbox2 <- shinydashboard::renderValueBox({
    sr <- sum(get_vrai_table()[2])
  shinydashboard::valueBox(sr, "Somme réalisée en GWh",color = "black" )
  })

  output$vbox3 <- shinydashboard::renderValueBox({
    Impact <- round((sum(get_vrai_table()[2]) - sum(get_vrai_table()["fit"]))/sum(get_vrai_table()["fit"]),4)
  shinydashboard::valueBox(Impact, "Impact en GWh", col = "red")
  })

  output$vbox4 <- shinydashboard::renderValueBox({
    Impact <- (sum(get_vrai_table()[2]) - sum(get_vrai_table()["fit"]))/sum(get_vrai_table()["fit"])
    Impact <- scales::percent(Impact)
    shinydashboard::valueBox(Impact, "Impact en pourcentage")
  })
  
  ## Bouton dowload
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(get_vrai_table()[,-c(4,5)], con)
    }
  )

}

# Run the application
shinyApp(ui = ui, server = server)
