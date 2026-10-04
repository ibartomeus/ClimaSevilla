#This script fetch Data from AEMET

#install.packages("climaemet")
library(climaemet)
library(dplyr)

## To get API key from AEMET.
#browseURL("https://opendata.aemet.es/centrodedescargas/altaUsuario")

## Set the API key for the current R session.
source("data/secrets.R")
aemet_api_key(sectret)

#Demo
# Plot warming stripes for a weather station.
#library(ggplot2)
# Example data
#temp_data <- climaemet::climaemet_9434_temp
#ggstripes(temp_data, plot_title = "Zaragoza Airport") +
 # labs(subtitle = "(1950-2020)")
#mean_temp <- climatestripes_station(station = "5783", 
 #                                   start = 1950, 
 #                                    end = 2025)                                    

#Get data
# 3. Define parameters
station_id <- "5783"  # San Pablo (Sevilla Airport) #Can also try tablada...
start_year <- 1952
end_year <- 2025

# Generate a sequence of years
years <- start_year:end_year
i <- start_year-1 #to debug

start_date <- paste0(i, "-01-01")
end_date   <- paste0(i, "-12-31")
out <- aemet_daily_clim( #ojo que a lo mejor esto ahorra el loop: sevilla_raw <- aemet_daily_period(station = "5783", start = 1950, end = 2025)
  station = station_id, 
  start = start_date, 
  end = end_date)
out$horatmin <- as.character(out$horatmin)
out$horatmax <- as.character(out$horatmax)
out$horaPresMax <- as.character(out$horaPresMax)
out$horaPresMin <- as.character(out$horaPresMin)
for(i in years){
  start_date <- paste0(i, "-01-01")
  end_date   <- paste0(i, "-12-31")
  temp <- aemet_daily_clim(
    station = station_id, 
    start = start_date, 
    end = end_date)
  if(!is.null(temp$horatmin)){temp$horatmin <- as.character(temp$horatmin)}
  if(!is.null(temp$horatmax)){temp$horatmax <- as.character(temp$horatmax)}
  if(!is.null(temp$horaPresMax)){temp$horaPresMax <- as.character(temp$horaPresMax)}
  if(!is.null(temp$horaPresMin)){temp$horaPresMin <- as.character(temp$horaPresMin)}
  if(!is.null(temp$horaPIntMax)){temp$horaPIntMax <- as.character(temp$horaPIntMax)}
  if(!is.null(temp$horaracha)){temp$horaracha <- as.character(temp$horaracha)}
  if(!is.null(temp$horaHrMax)){temp$horaHrMax <- as.character(temp$horaHrMax)}
  if(!is.null(temp$horaHrMin)){temp$horaHrMin <- as.character(temp$horaHrMin)}
  out <- bind_rows(out, temp)
}

str(temp)
str(out) 
dim(out)

length(years)
27394/365 #75 anys, good!

out <- as.data.frame(out)
str(out)

#Add and clean stuff
out$year <- format(out$fecha,"%Y")
table(out$year) #great!
out$year <- format(out$fecha,"%Y")
out$fecha2 <- as.Date(out$fecha)
out$month <- format(out$fecha2, "%m")

out$prec <- gsub(",", ".", out$prec, fixed = TRUE)
out$prec <- gsub("Ip", "0.05", out$prec, fixed = TRUE)
unique(out$prec)
out[which(is.na(out$prec)),] #4 casos, lo dejo como NA
out$prec <- as.numeric(out$prec)

write.csv(subset(out, select=-c(X, fecha2)), "data/SevillaClimate.csv")
