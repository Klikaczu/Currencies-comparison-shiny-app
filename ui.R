library(dplyr)
library(countrycode)
library(googleVis)
library(plotly)
library(lmtest)
library(rmarkdown)
library(knitr)
library(markdown) 

shinyUI(fluidPage(
  
  titlePanel("Projekt PIWD"),
  
  
  sidebarLayout(
    
    
    sidebarPanel(
      
      textInput(inputId = 'poczatek',
                label = "Od"),
      textInput(inputId = 'koniec',
                label = "Do"),
      
      selectizeInput(inputId = 'currency',
                  label = "Wybór 2 walut do analizy",
                  choices = c("THB", "USD", "AUD", "HKD","CAD","NZD",
                              "SGD","EUR","HUF","CHF","GBP","UAH",
                              "JPY","CZK","DKK","ISK","NOK","SEK",
                              "HRK","RON","BGN","TRY","LTL","LVL",
                              "ILS","CLP","PHP","MXN","ZAR","BRL",
                              "MYR","RUB","IDR","INR","KRW","CNY","XDR"),
                  multiple=T, options = list(maxItems = 2)),
      
      downloadButton(outputId= "report", label = "Pobierz raport")

    ),

    
    mainPanel(
      
      # Panel zakladek  
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Tabela", dataTableOutput("daneSample")),

                  tabPanel("Mapa", htmlOutput("view")),
                  
                  tabPanel("W czasie", htmlOutput("szereg")),
                  
                  tabPanel("Scatter", plotlyOutput("scatter")),
                  
                  tabPanel("Róznice kursów", plotlyOutput("histogram")),
                  
                  tabPanel("Regresja", verbatimTextOutput("reg"), plotOutput("wyk"))
                
                
      )
      
      
    )
    
    )
    
  )
)
