####################
#Project: Kenitra Shiny 2025

####################

##########################################
#Load some packages
##########################################
#install.packages(“openair”)
#install.packages(“worldmet”)
#install.packages(“dplyr”)
#install.packages(“lubridate”)
#install.packages("shiny")
#install.packages("shinythemes")
#install.packages(leaflet)

library(openair)
library(worldmet)
library(dplyr)
library(lubridate)
library(shiny)
library(shinythemes)

##########################################
#Tell R which folder to work in
#update the file path to match where you saved your data 
##########################################
#setwd("/Users/Flo/Library/CloudStorage/GoogleDrive-fpopescu@greenpeace.org/My Drive/APU 2025 Florin/R Shiny/KenitraShiny")
#setwd("/Users/Flo/Documents/R and Shiny/KenitraDeploy")
#setwd("G:/Shared drives/GPEA-GPI-Clean-Air-Unit/Crew Unit Secondments/Florin Secondment 2025/APU 2025 Florin/R Shiny/KenitraShiny")
#setwd("/Users/afarrow/Library/CloudStorage/GoogleDrive-afarrow@greenpeace.org/Shared\ drives/GPEA-GPI-Clean-Air-Unit/Crew\ Unit\ Secondments/Florin\ Secondment\ 2025/APU\ 2025\ Florin/R\ Shiny/KenitraShiny")
##########################################

#Define the EPA correction function
##########################################
EPAcorrection <- function(rawPM, rawRH){
  # Ensure both rawPM and rawRH are vectors of the same length
  result <- numeric(length(rawPM))  # To store results
  #loop over the data
  for (i in 1:length(rawPM)) {
    #For raw readings below 30 µg/m³:
    #PM2.5 = [0.524 × PAcfatm] - [0.0862 × RHraw] + 5.75
    if(rawPM[i] < 30){
      result[i] <- (0.524 * rawPM[i]) - (0.0862 * rawRH[i]) + 5.75
      #For raw readings between 30 and 50 µg/m³:
      #PM2.5 = [0.786 × (PAcfatm/20 - 3/2) + 0.524 × (1 - (PAcfatm/20 - 3/2))] x PAcfatm - [0.0862 × RHraw] + 5.75
    } else if(rawPM[i] >= 30 & rawPM[i] <= 50){
      result[i] <- (0.786 * (rawPM[i]/20 - 3/2) + 0.524 * (1 - (rawPM[i]/20 - 3/2))) * rawPM[i] - (0.0862 * rawRH[i]) + 5.75
      #For raw readings between 50 and 210 µg/m³:
      #PM2.5 = [0.786 × PAcfatm] - [0.0862 × RHraw] + 5.75
    } else if(rawPM[i] >= 50 & rawPM[i] <= 210){
      result[i] <- (0.786 * rawPM[i]) - (0.0862 * rawRH[i]) + 5.75
      #For raw readings between 210 and 260 µg/m³:
      #PM2.5 = [0.69 × (PAcfatm/50 - 21/5) + 0.786 × (1 - (PAcfatm/50 - 21/5))] x PAcfatm- [0.0862 × RHraw × (1 - (PAcfatm/50 - 21/5))] + [2.966 × (PAcfatm/50 - 21/5)] + [5.75 × (1 - (PAcfatm/50 - 21/5))] + [8.84 × (10-4) × PAcfatm2 × (PAcfatm/50 - 21/5)]
    } else if(rawPM[i] >= 210 & rawPM[i] <= 260){
      result[i] <- (0.69 * (rawPM[i]/50 - 21/5) + 0.786 * (1 - (rawPM[i]/50 - 21/5))) * rawPM[i] - (0.0862 * rawRH[i] * (1 - (rawPM[i]/50 - 21/5))) + (2.966 * (rawPM[i]/50 - 21/5)) + (5.75 * (1 - (rawPM[i]/50 - 21/5))) + (0.000884 * rawPM[i]^2 * (rawPM[i]/50 - 21/5))
      #For raw readings above 260 µg/m³:
      #PM2.5 = 2.966 + [0.69 × PAcfatm] + [8.84 × 10-4 × PAcfatm2 ]
    } else if(rawPM[i] > 260){
      result[i] <- 2.966 + (0.69 * rawPM[i]) + (0.000884 * rawPM[i]^2)
    }
  }
  return(result)
}
##########################################
#Read some data

#Read the particulate matter data
PMdata<-read.csv("new_AirGradientData.csv")
#Read the Met Data
wind<-read.csv("new_MeteorologicalData.csv")

##########################################
#Rename columns in both files
#The date column must be called 'date'
PMdata$date <- PMdata$Local.Date.Time
wind$date <- wind$datetime

#The PM2.5 column name can be shorter
PMdata$PM25_raw<-PMdata$PM2.5..μg.m...raw
#The CO2..ppm. column name can be shorter
PMdata$CO2<-PMdata$CO2..ppm..raw

#The wind speed and direction columns can be "ws" and "wd"
wind$ws<-wind$windspeed
wind$wd<-wind$winddir

##########################################
#Apply the EPA correction function to the PM2.5 data
PMdata$PM2.5<-EPAcorrection(PMdata$PM25_raw,PMdata$Humidity.....raw)

##########################################
#Format the time zones
PMdata$date<-as.POSIXct(strptime(PMdata$date, format='%Y-%m-%d %H:%M',tz='CET'))
wind$date<-as.POSIXct(strptime(wind$date, format='%Y-%m-%dT%H:%M'))

#########################################
# Add the location of the 3 sensors in PMdata:
#for Saknia 1 sensor ID 78160: 34.241828524909074, -6.555493676717981
#for Poste dar chabab sensor ID 84563: 34.259977, -6.561911
#for Industrielle Saknia 1 sensor ID 82925: 34.280936, -6.559629

#find the IDs of the 3 sensors
unique(PMdata[c("Location.ID", "Location.Name")])

#Add a column with the latitude of the 3 sensors identified by ID
PMdata$Location.latitude <- ifelse(PMdata$Location.ID ==78160, 34.241828524909074,
                                   ifelse(PMdata$Location.ID == 84563, 34.259977, 34.280936))

#Add a column with the longitude of the 3 sensors identified by ID
PMdata$Location.longitude <- ifelse(PMdata$Location.ID ==78160, -6.555493676717981,
                                   ifelse(PMdata$Location.ID == 84563, -6.561911, -6.559629))

#double check coordinates, IDs and names of the 3 sensors
unique_loc <- unique(PMdata[c("Location.ID", "Location.Name", "Location.latitude", "Location.longitude")])

#############################################
#Merge the wind and PM data
data<-merge(PMdata,wind,by="date") 




