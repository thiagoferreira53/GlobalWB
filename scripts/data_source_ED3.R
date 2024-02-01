
path <- '/Users/thiagoferreira53/Desktop/CIMMYT/global_WB/ExtendedFig3/'
list <- list.files(path, full.names = F)

plots <- function(file){
  fileWTH <- read.csv(file, skip = 4 ,head=T, sep='')
  fileWTH$JulianD <- ifelse(fileWTH$X.DATE > 50000,1900000+fileWTH$X.DATE, fileWTH$X.DATE+2000000)
  fileWTH$date <- as.Date(strftime(as.POSIXct(as.character(fileWTH$JulianD), format="%Y%j")))
  
  filtWTH <- subset(fileWTH, format(as.Date(fileWTH$date),"%Y") >= 2000)
  
  colnames(filtWTH)[11] <- "AgERA5"
  
  meltWTH <- melt(filtWTH, id.vars = 'date', measure.vars = c('RHUT','AgERA5')) 
  
  
  
  if(file=="ENSO_Historical_-14.25_27.25.csv"){
    meltWTH$location <- "Mpika, Zambia"
  }
  if(file=="ENSO_Historical_-23.25_-51.25.csv"){
    meltWTH$location <- "Londrina, Brazil"
  }
  if(file=="ENSO_Historical_23.25_89.25.csv"){
    meltWTH$location <- "Jashore, Bangladesh"
  }
  if(file=="ENSO_Historical_27.25_-109.75.csv"){
    meltWTH$location <- "Obregon, Mexico"
  }
  
  meltWTH
  
}

library(dplyr)
data <- bind_rows(lapply(list, plots))

write.csv(data, "/Users/thiagoferreira53/Desktop/Final_Code_Figures_NCC/source_data/ExtendedData_figure3.csv", row.names = F)
