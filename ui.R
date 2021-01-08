shinyUI(fluidPage(
  
  titlePanel("Projekt PIWD"),
  
  
  sidebarLayout(
    
    
    sidebarPanel(
      
      textInput(inputId = 'poczatek',
                label = "Od"),
      textInput(inputId = 'koniec',
                label = "Do")
      
    ),
    
    mainPanel(
      
      tabPanel("Moja tabela", tableOutput("daneSample"))
    ))
    
  )
)
