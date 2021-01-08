shinyUI(fluidPage(
  
  titlePanel("Projekt PIWD"),
  
  
  sidebarLayout(
    
    
    sidebarPanel(
      
      textInput(inputId = 'poczatek',
                label = "Od"),
      textInput(inputId = 'koniec',
                label = "Do"),
      
      actionButton("getDataFromServer", "Pokaż dane")
      
    ),
    
    mainPanel(
      
      tabPanel("Moja tabela", tableOutput("daneSample"))
    ))
    
  )
)
