
library(dplyr)
library(shiny)
library(binom)
library(xts)
library(ggplot2)
library(grid)
library(gridExtra)
library(googleVis)

###read data
# setwd("~/Desktop") #need to modify
filenames1 <- list.files("cancellation", pattern="20[0-9]+.csv", full.names=TRUE) 
data <- lapply(filenames1,read.csv,header=T)
data<-do.call("rbind", data)

data<-data[c("YEAR","MONTH","DAY_OF_MONTH","UNIQUE_CARRIER","ORIGIN","DEST","CRS_DEP_TIME","CANCELLED","CRS_ELAPSED_TIME","ACTUAL_ELAPSED_TIME","AIR_TIME")]
data$dephour<-floor(data$CRS_DEP_TIME/100)

###prepare data for general trend
data.group_general<-group_by(data,YEAR)
data.group_general<-summarise(data.group_general,count=n(),c_rate=mean(CANCELLED),cancel=sum(CANCELLED))
write.csv(data.group_general,"general.csv")


###prepare data for scatter plot distance
data.group_scat<-group_by(data,UNIQUE_CARRIER,ORIGIN,DEST)
data.group_scat<-summarise(data.group_scat,count=n(),c_rate=mean(CANCELLED),distance=mean(CRS_ELAPSED_TIME))

write.csv(data.group_scat,"scatter.csv")

###prepare data for time series
data.airline<-group_by(data,UNIQUE_CARRIER,YEAR,MONTH,DAY_OF_MONTH)
data.airline_cancel<-summarise(data.airline,count=n(),c_rate=mean(CANCELLED),cancel=sum(CANCELLED))
data.airline_cancel$lower<-binom.confint(data.airline_cancel$cancel,data.airline_cancel$count,conf.level = 0.95, methods = "exact")$lower
data.airline_cancel$upper<-binom.confint(data.airline_cancel$cancel,data.airline_cancel$count,conf.level = 0.95, methods = "exact")$upper
data.airline_cancel$date<-paste(data.airline_cancel$YEAR,data.airline_cancel$MONTH,data.airline_cancel$DAY_OF_MONTH,sep="-")

data.airline_cancel$airline[data.airline_cancel$UNIQUE_CARRIER == "AA"] <- "American"
data.airline_cancel$airline[data.airline_cancel$UNIQUE_CARRIER == "DL"] <- "Delta"
data.airline_cancel$airline[data.airline_cancel$UNIQUE_CARRIER == "UA"] <- "United"
data.airline_cancel$airline[data.airline_cancel$UNIQUE_CARRIER == "US"] <- "US Airways"
data.airline_cancel$airline[data.airline_cancel$UNIQUE_CARRIER == "OO"] <- "Skywest"
data.airline_cancel$airline[data.airline_cancel$UNIQUE_CARRIER == "WN"] <- "Southwest"
data.airline_cancel$airline[data.airline_cancel$UNIQUE_CARRIER == "B6"] <- "JetBlue"
data.airline_cancel$airline[data.airline_cancel$UNIQUE_CARRIER == "EV"] <- "ExpressJet"
data.airline_cancel$airline[data.airline_cancel$UNIQUE_CARRIER == "F9"] <- "Frontier"
data.airline_cancel$airline[data.airline_cancel$UNIQUE_CARRIER == "VX"] <- "Virgin"
data.airline_cancel$airline[data.airline_cancel$UNIQUE_CARRIER == "FL"] <- "airTrans"

##AS (Alaska airlines)
##HA (hawiian airlines), MQ (Envoy air)

write.csv(data.airline_cancel,"airlinedata.csv")

