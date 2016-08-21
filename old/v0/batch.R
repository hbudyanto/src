#########################################
## ========== BATCH
#########################################

# Enable Bellow Packages
require(data.table)
require(plyr)
require(futile.logger)
require(xgboost)
library(caret)
library(lubridate)

isAWS <-(Sys.info()[1]=="Darwin")

if(isAWS){
  #basewd<- "/home/rstudio/Dropbox/Thesis-Data"
  #Figures <- "/home/rstudio/Dropbox/Apps/ShareLaTeX/University College London thesis/Figures"
  Figures <- file.path("/home/rstudio/Dropbox/Apps/ShareLaTeX/University-College-London-thesis/Figures")
  TexTables <- file.path("/home/rstudio/Dropbox/Apps/ShareLaTeX/University-College-London-thesis/Tables")
  functioncode <- file.path(basewd, "SmartMeterThesisCode","Functions")
  Cormats <- "/home/rstudio/Cormats"
  GraphPath<-file.path("/home/rstudio", "graphs")
  options(fftempdir = "/home/rstudio")
} else {
  
  basewd <- "~/PycharmProjects/dissertation"
  #Figures <- file.path("C:/Users/pc1/Dropbox/Apps/ShareLaTeX/University-College-London-thesis/Figures")
  #TexTables <- file.path("C:/Users/pc1/Dropbox/Apps/ShareLaTeX/University-College-London-thesis/Tables")
  file.path("~/PycharmProjects/dissertation")
  functioncode <- "~/PycharmProjects/dissertation/src/v1"
  #Cormats <- "C:/Users/pc1/Dropbox/Thesis-Data/Cormats"
}

SubDataSets <- file.path(basewd, "SubDataSets")
datafile <- file.path(basewd, "TCa1")
daytimeseries <-file.path(basewd,"Cleandata")

# Set Appropriate Path to the link
setwd("~/PycharmProjects/dissertation/raw_data")

# Running the Whole Process
source('~/PycharmProjects/dissertation/src/processing_v0.R') #data cleansing
