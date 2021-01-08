shinyServer(function(input, output) {
  
  dataIn <- reactive({
  
  lata <- seq(input$poczatek, input$koniec)
  
# lata <- seq(2013, 2019)
  lata <- as.data.frame(lata)
  
  getNBPData <- function(year){
    
    ret <- data.frame()
    
    if(year>=2013){
      
      fileName <- paste0(year,"_NBP_data.csv")
      
      try({
        if(file.exists(fileName)){
          if(as.Date(file.info(fileName)$mtime)==Sys.Date()){
            cat(paste("Reading data from local file\n"))
            return(read.table(file=fileName,sep=";",dec=",",header=T,stringsAsFactor=F))
          }
        }
      })
      
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
      
      
    }
    
    return(ret)
    
  }

  data <- apply(lata, 1, getNBPData)
  
  if(str_contains(unlist(data),"2013")) {
  data2013 <- data[[1]]
  daneSample <- data2013  
  
  } else if(length(data)==2) {
  data2013 <- data[[1]]
  data2014 <- data[[2]]
  data2014 <- add_column(data2014, X1LVL=NA, .after = "X1LTL")
  daneSample <- rbind(data2013,data2014)
  
  } else if(length(data)==3) {
  data2013 <- data[[1]]
  data2014 <- data[[2]]
  data2014 <- add_column(data2014, X1LVL=NA, .after = "X1LTL")   
  data2015 <- data[[3]]
  data2015 <- add_column(data2015, X1LTL=NA, .after = "X1TRY")
  data2015 <- add_column(data2015, X1LVL=NA, .after = "X1LTL")
  daneSample <- rbind(data2013,data2014,data2015)
  
  } else if(length(data)==4) {
  data2013 <- data[[1]]
  data2014 <- data[[2]]
  data2014 <- add_column(data2014, X1LVL=NA, .after = "X1LTL")   
  data2015 <- data[[3]]
  data2015 <- add_column(data2015, X1LTL=NA, .after = "X1TRY")
  data2015 <- add_column(data2015, X1LVL=NA, .after = "X1LTL")  
  data2016 <- data[[4]]
  data2016 <- add_column(data2016, X1LTL=NA, .after = "X1TRY")
  data2016 <- add_column(data2016, X1LVL=NA, .after = "X1LTL")
  daneSample <- rbind(data2013,data2014,data2015,data2016)
  
  } else if(length(data)==5) {
  data2013 <- data[[1]]
  data2014 <- data[[2]]
  data2014 <- add_column(data2014, X1LVL=NA, .after = "X1LTL")   
  data2015 <- data[[3]]
  data2015 <- add_column(data2015, X1LTL=NA, .after = "X1TRY")
  data2015 <- add_column(data2015, X1LVL=NA, .after = "X1LTL")  
  data2016 <- data[[4]]
  data2016 <- add_column(data2016, X1LTL=NA, .after = "X1TRY")
  data2016 <- add_column(data2016, X1LVL=NA, .after = "X1LTL")  
  data2017 <- data[[5]]
  data2017 <- add_column(data2017, X1LTL=NA, .after = "X1TRY")
  data2017 <- add_column(data2017, X1LVL=NA, .after = "X1LTL")
  daneSample <- rbind(data2013,data2014,data2015,data2016,data2017)
  
  } else if(length(data)==6) {
  data2013 <- data[[1]]
  data2014 <- data[[2]]
  data2014 <- add_column(data2014, X1LVL=NA, .after = "X1LTL")   
  data2015 <- data[[3]]
  data2015 <- add_column(data2015, X1LTL=NA, .after = "X1TRY")
  data2015 <- add_column(data2015, X1LVL=NA, .after = "X1LTL")  
  data2016 <- data[[4]]
  data2016 <- add_column(data2016, X1LTL=NA, .after = "X1TRY")
  data2016 <- add_column(data2016, X1LVL=NA, .after = "X1LTL")  
  data2017 <- data[[5]]
  data2017 <- add_column(data2017, X1LTL=NA, .after = "X1TRY")
  data2017 <- add_column(data2017, X1LVL=NA, .after = "X1LTL")
  data2018 <- data[[6]]
  data2018 <- add_column(data2018, X1LTL=NA, .after = "X1TRY")
  data2018 <- add_column(data2018, X1LVL=NA, .after = "X1LTL")
  daneSample <- rbind(data2013,data2014,data2015,data2016,data2017,data2018)
  
  } else if(length(data)==7) {
  data2013 <- data[[1]]
  data2014 <- data[[2]]
  data2014 <- add_column(data2014, X1LVL=NA, .after = "X1LTL")   
  data2015 <- data[[3]]
  data2015 <- add_column(data2015, X1LTL=NA, .after = "X1TRY")
  data2015 <- add_column(data2015, X1LVL=NA, .after = "X1LTL")  
  data2016 <- data[[4]]
  data2016 <- add_column(data2016, X1LTL=NA, .after = "X1TRY")
  data2016 <- add_column(data2016, X1LVL=NA, .after = "X1LTL")  
  data2017 <- data[[5]]
  data2017 <- add_column(data2017, X1LTL=NA, .after = "X1TRY")
  data2017 <- add_column(data2017, X1LVL=NA, .after = "X1LTL")
  data2018 <- data[[6]]
  data2018 <- add_column(data2018, X1LTL=NA, .after = "X1TRY")
  data2018 <- add_column(data2018, X1LVL=NA, .after = "X1LTL")
  data2019 <- data[[7]]
  data2019 <- add_column(data2019, X1LTL=NA, .after = "X1TRY")
  data2019 <- add_column(data2019, X1LVL=NA, .after = "X1LTL")
  daneSample <- rbind(data2013,data2014,data2015,data2016,data2017,data2018,data2019)
  
  } else if(length(data)==8) {
  data2013 <- data[[1]]
  data2014 <- data[[2]]
  data2014 <- add_column(data2014, X1LVL=NA, .after = "X1LTL")   
  data2015 <- data[[3]]
  data2015 <- add_column(data2015, X1LTL=NA, .after = "X1TRY")
  data2015 <- add_column(data2015, X1LVL=NA, .after = "X1LTL")  
  data2016 <- data[[4]]
  data2016 <- add_column(data2016, X1LTL=NA, .after = "X1TRY")
  data2016 <- add_column(data2016, X1LVL=NA, .after = "X1LTL")  
  data2017 <- data[[5]]
  data2017 <- add_column(data2017, X1LTL=NA, .after = "X1TRY")
  data2017 <- add_column(data2017, X1LVL=NA, .after = "X1LTL")
  data2018 <- data[[6]]
  data2018 <- add_column(data2018, X1LTL=NA, .after = "X1TRY")
  data2018 <- add_column(data2018, X1LVL=NA, .after = "X1LTL")
  data2019 <- data[[7]]
  data2019 <- add_column(data2019, X1LTL=NA, .after = "X1TRY")
  data2019 <- add_column(data2019, X1LVL=NA, .after = "X1LTL")
  data2020 <- data[[8]]
  data2020 <- add_column(data2020, X1LTL=NA, .after = "X1TRY")
  data2020 <- add_column(data2020, X1LVL=NA, .after = "X1LTL")
  daneSample <- rbind(data2013,data2014,data2015,data2016,data2017,data2018,data2019,data2020)
    
  } else {
  data2013 <- data[[1]]
  data2014 <- data[[2]]
  data2014 <- add_column(data2014, X1LVL=NA, .after = "X1LTL")   
  data2015 <- data[[3]]
  data2015 <- add_column(data2015, X1LTL=NA, .after = "X1TRY")
  data2015 <- add_column(data2015, X1LVL=NA, .after = "X1LTL")  
  data2016 <- data[[4]]
  data2016 <- add_column(data2016, X1LTL=NA, .after = "X1TRY")
  data2016 <- add_column(data2016, X1LVL=NA, .after = "X1LTL")  
  data2017 <- data[[5]]
  data2017 <- add_column(data2017, X1LTL=NA, .after = "X1TRY")
  data2017 <- add_column(data2017, X1LVL=NA, .after = "X1LTL")
  data2018 <- data[[6]]
  data2018 <- add_column(data2018, X1LTL=NA, .after = "X1TRY")
  data2018 <- add_column(data2018, X1LVL=NA, .after = "X1LTL")
  data2019 <- data[[7]]
  data2019 <- add_column(data2019, X1LTL=NA, .after = "X1TRY")
  data2019 <- add_column(data2019, X1LVL=NA, .after = "X1LTL")
  data2020 <- data[[8]]
  data2020 <- add_column(data2020, X1LTL=NA, .after = "X1TRY")
  data2020 <- add_column(data2020, X1LVL=NA, .after = "X1LTL")
  data2021 <- data[[9]]
  data2021 <- add_column(data2021, X1LTL=NA, .after = "X1TRY")
  data2021 <- add_column(data2021, X1LVL=NA, .after = "X1LTL")
  daneSample <- rbind(data2013,data2014,data2015,data2016,data2017,data2018,data2019,data2020,data2021)
  }
#  if(input$koniec = 2013){
#  try(daneSample <- rbind(data2013))}
  
#  else if(input$koniec = 2014){
#  try(daneSample <- rbind(data2013,data2014))}
  
#  else if(input$koniec = 2015){
#  try(daneSample <- rbind(data2013,data2014,data2015))}
  
#  else if(input$koniec = 2016){
#  try(daneSample <- rbind(data2013,data2014,data2015,data2016))}
  
#  else if(input$koniec = 2017){
#  try(daneSample <- rbind(data2013,data2014,data2015,data2016,data2017))}
  
#  else if(input$koniec = 2018){
#  try(daneSample <- rbind(data2013,data2014,data2015,data2016,data2017,data2018))}
  
 # else if(input$koniec = 2019){
#  try(daneSample <- rbind(data2013,data2014,data2015,data2016,data2017,data2018,data2019))}
  
#  else if(input$koniec = 2020){
#  try(daneSample <- rbind(data2013,data2014,data2015,data2016,data2017,data2018,data2019,data2020))}
  
#  else (input$koniec = 2021){
#  try(daneSample <- rbind(data2013,data2014,data2015,data2016,data2017,data2018,data2019,data2020,data2021))}
  
  })
  
  
  # Wypisanie danych
  
  output$daneSample <- renderTable({
    tmpData <- dataIn()
    return(tmpData)
  },include.rownames=FALSE)
  
  
  # Zmienna zapisująca stan przycisku
  
  v <- reactiveValues(dataLoadDownload = FALSE)
  
  # Akcja przycisku 
  
  observeEvent(input$getDataFromServer,{
    v$dataLoadDownload <- !v$dataLoadDownload
  })
  
})
