library(shiny)
library(dplyr)
library(countrycode)
library(googleVis)
library(plotly)
library(lmtest)
library(rmarkdown)
library(knitr)
library(markdown) 

shinyServer(function(input, output) {
  
  
  ##########################################################################################################################
  ##########################################################################################################################
  ###############################                        TAB 1                          ####################################
  ##########################################################################################################################
  ##########################################################################################################################
  
  
  dataIn <- reactive({
  
  lata <- seq(input$poczatek, input$koniec)
  
 # lata <- seq(2013, 2021)
  lata <- as.data.frame(lata)
  
  
  getNBPData <- function(year){
    
    ret <- data.frame()
    
    if(year>=2013){
      
      # fileName <- paste0(year,"_NBP_data.csv")
      # 
      # try({
      #   if(file.exists(fileName)){
      #     if(as.Date(file.info(fileName)$mtime)==Sys.Date()){
      #       cat(paste("Reading data from local file\n"))
      #       return(read.table(file=fileName,sep=";",dec=",",header=T,stringsAsFactor=F))
      #     }
      #   }
      # })
      # 
      cat(paste("Downloading data\n"))
      
      res <- try({
        
        d <- readLines(paste0("https://www.nbp.pl/kursy/Archiwum/archiwum_tab_a_",year,".csv"))
        d <- d[-2]
        d <- d[-c((length(d)-3):length(d))]
        tmpColnames <- strsplit(d[1],";",useBytes=T)[[1]]
        tmpColnames <- tmpColnames[-c((length(tmpColnames)-1):length(tmpColnames))]
        d <- do.call("rbind",
                     lapply(strsplit(d[-1],";"),
                            function(x){
                              matrix(as.numeric(gsub(",",".",x[-c((length(x)-1):length(x))])),nrow=1)
                            })
        )
        colnames(d) <- tmpColnames
        d <- as.data.frame(d)
        
        d$data <- as.Date(as.character(d$data),format="%Y%m%d")
        ret <- d
        write.table(ret,file=fileName,sep=";",dec=",",row.names=F)
        
      },silent=T)
      
      if(inherits(res,"try-error")){
        cat(paste("An error occurred while downloading data!!!\n")) 
      }
      
      
    
    
    return(ret)
    } else {""}
  
    return(ret)
    
  }
  
  
  data <- apply(lata, 1, getNBPData)
  
  if (length(data)==1){
  daneSample <- try(bind_rows(data[[1]]))
  
  } else if (length(data)==2){
  daneSample <- try(bind_rows(data[[1]],data[[2]]))
  
  } else if (length(data)==3){
  daneSample <- try(bind_rows(data[[1]],data[[2]],data[[3]]))
  
  } else if (length(data)==4){
  daneSample <- try(bind_rows(data[[1]],data[[2]],data[[3]],data[[4]]))
  
  } else if (length(data)==5){
  daneSample <- try(bind_rows(data[[1]],data[[2]],data[[3]],data[[4]],data[[5]]))
  
  } else if (length(data)==6){
  daneSample <- try(bind_rows(data[[1]],data[[2]],data[[3]],data[[4]],data[[5]],data[[6]]))
  
  } else if (length(data)==7){
  daneSample <- try(bind_rows(data[[1]],data[[2]],data[[3]],data[[4]],data[[5]],data[[6]],data[[7]])) 
  
  } else if (length(data)==8){
  daneSample <- try(bind_rows(data[[1]],data[[2]],data[[3]],data[[4]],data[[5]],data[[6]],data[[7]],data[[8]]))
  
  } else if (length(data)==9) {
  daneSample <- try(bind_rows(data[[1]],data[[2]],data[[3]],data[[4]],data[[5]],data[[6]],data[[7]],data[[8]],data[[9]]))
  }
  
  cols <- names(daneSample)
  cols <- str_remove(cols,"X1")
  cols <- str_remove(cols, "0")
  cols <- str_remove(cols, "0")
  cols <- str_remove(cols, "0")
  cols <- str_remove(cols, "0")
  cols <- str_remove(cols, "1")
  colnames(daneSample) <- cols
  daneSample$data <- as.Date(daneSample$data)
  return(daneSample)
  })

  

  # Funkcja do czyszczenia
  library(stringr)
  str_remove <- function(string, pattern) {
    str_replace(string, pattern, "")
  }
  
  # Wypisanie danych

  output$daneSample <- renderDataTable({
    validate(
      need(input$poczatek, ""),
      need(input$koniec, "")
    )
    tmpData <- dataIn()
    return(tmpData)
  }, options = list(lengthMenu = seq(10,100,10), filter='top',rownames=T))
  

  
  ##########################################################################################################################
  ##########################################################################################################################
  ###############################                    TAB 2 - MAPKA                       ###################################
  ##########################################################################################################################
  ##########################################################################################################################
  
   t <- as.data.frame(codelist)
   df <-data.frame(currency=t$iso4217c, country=t$iso2c)
   df <- na.omit(df)
   df$currency <- as.factor(df$currency)

   
  graphdataIn <-reactive({
    
    t <- as.data.frame(codelist)
    df <-data.frame(currency=t$iso4217c, country=t$iso2c)
    df <- na.omit(df)
    df$currency <- as.factor(df$currency)
    
    a <- df[df$currency==input$currency[1],]
    b <- df[df$currency==input$currency[2],]
    map <- rbind(a,b)
    return(map)
  })
  

  output$view <- renderGvis({
    
    gvisGeoChart(graphdataIn(), locationvar="country",colorvar = "currency",  
                 options=list(width=1000, height=900))
    
  })
    
  
  ##########################################################################################################################
  ##########################################################################################################################
  ###############################                   TAB 3 - SZEREGI                      ###################################
  ##########################################################################################################################
  ##########################################################################################################################

  output$szereg <- renderGvis({ 
    gvisLineChart(dataIn(), yvar = c(input$currency[1], input$currency[2]))
  })
  
  daneSzeregi <- reactive({
    d <- dataIn()
    d <- data.frame(d[,'data'], d[,input$currency[1]], d[,input$currency[2]])
  })

  ##########################################################################################################################
  ##########################################################################################################################
  ###############################                   TAB 4 - SCATTER                    #####################################
  ##########################################################################################################################
  ##########################################################################################################################
  
  output$scatter <- renderPlotly({
    plot_ly(dataIn(),x=dataIn()[,input$currency[1]], y=dataIn()[,input$currency[2]], type='scatter')%>%
      layout(xaxis=list(
        title =input$currency[1] )) %>%
      layout(yaxis=list(
        title =input$currency[2]))
  })
  
  
  
  ##########################################################################################################################
  ##########################################################################################################################
  ###############################                 TAB 4 - HISTOGRAM                   ######################################
  ##########################################################################################################################
  ##########################################################################################################################
  
  datahist <- reactive({
    d <- dataIn()
    a <- d[,input$currency[1]] 
    b <- d[,input$currency[2]]
    c <- abs(a-b)
    c <- as.data.frame(c)
    return(c)
  })
  
  output$histogram <- renderPlotly({
    plot_ly(datahist(),x=datahist()[,1],type='histogram')%>%
      layout(xaxis=list(
        title ="Różnica")) %>%
      layout(yaxis=list(
        title = "Freq"))
  })
  
  
  ##########################################################################################################################
  ##########################################################################################################################
  ###############################                  TAB 5 - REGRESJA                   ######################################
  ##########################################################################################################################
  ##########################################################################################################################

  regression <- reactive({

  d <- dataIn()
  r <- lm(d[,input$currency[1]] ~ d[,input$currency[2]])
  print(summary(r))
  print(raintest(r))
  
  })

  output$reg <- renderPrint(regression())
  
  regplotdata <- reactive({
    
    d <- dataIn()
    d <- data.frame(d[,input$currency[1]], d[,input$currency[2]])
    d <- na.omit(d)
  })
    
  output$wyk <- renderPlot({
    
    ggplot(regplotdata(), aes(regplotdata()[,1], regplotdata()[,2])) +
      geom_point() +
      stat_smooth(method=lm) +
      xlab(input$currency[1]) +
      ylab(input$currency[2]) +
      theme_bw()
  })

  
  ##########################################################################################################################
  ##########################################################################################################################
  ###############################                GENEROWANIE RAPORTU                  ######################################
  ##########################################################################################################################
  ##########################################################################################################################
  

    # knit(input='Raport.Rmd', output="tmp.md",envir=new.env())
    # markdownToHTML(file="tmp.md", output="Raport.html")

  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {

      tempReport <- file.path(tempdir(), "Raport.Rmd")
      file.copy("Raport.Rmd", tempReport, overwrite = TRUE)

      params <- list()
      params$szereg = daneSzeregi()
      params$mapa = graphdataIn()
      params$hist = datahist()
      params$reg = regression()

      save(params,file="params")

      knit(input='Raport.Rmd', output="tmp.md",envir=new.env())
      markdownToHTML(file="tmp.md", output="Raport.html")

      unlink("tmp.md")
      unlink("params")

      
      
    }
  )

  
  
}) # KONIEC 

