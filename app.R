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
#setwd("/Users/Flo/KPub")
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

#############################################################################
#############################################################################
#############################################################################

# This is the ui part of the script
# Define UI
ui <- navbarPage(
  title = "Air Quality Visualization",
  theme = shinytheme("cerulean"),
  # Home tab
  tabPanel("Home",
           fluidPage(
             h2("Welcome!"),
             p("The <NAME OF PROJECT HERE> project is a collaboration between <XXX> and <XXX>. It aims to observe, document and analyse air quality in the city
             of Kenitra, Morocco. Low cost air quality sensors were installed in Kenitra during 2024 and 2025. The data they have generated can be viewed 
             and investigated using this web app. Maps and graphics are provided that show sensor locations, pollutant conentrations and wind conditions 
             through time. Simply navigate to different tabs within the app and select the data you wish to visualise. Notes on the use and interpretation 
             of each plot are provided in the 'About' tab.   
             Air quality data presented here is collected using", 
               tags$a("AirGradient", href = "https://www.airgradient.com", target = "_blank"),
               "sensors, it has been combined with modelled wind data produced by",
               tags$a("Visual Crossings", href = "https://www.visualcrossing.com", target = "_blank"),
               "Data analysis is performed using the R OpenAir package (Carslaw and Ropkins, 2012) and is presented using an R shiny application (Chang et al 2025)"),
             tags$div(                                          #insert an image and center it
               tags$img(src = "sky.jpeg", width = "900px"),
               style = "text-align: center;"
             )
           )
  ),
  # Map tab
  tabPanel("Map",
           sidebarLayout(
             sidebarPanel(
               h1("Kenitra Map"),
               p("A map of central Kenitra showing the approximate location of air quality sensors."),
               p("Also shown is Kenitra's fossil fuel fired power plant which is one possible source of particulate
                 and gaseous air pollutants."),
             ),
             mainPanel(
               tags$img(src = "Map.png", width = "900", alt = "map")
             )
           )
  ),
  
  # Wind rose tab
  tabPanel("Wind Rose",
           sidebarLayout(
             sidebarPanel(
               h1("Wind Rose"),
               p("This tab shows a wind rose for your selected location and date range."),
               hr(),
               selectInput("location_rose", "Select Location:",
                           choices = unique(data$Location.Name), 
                           selected = unique(data$Location.Name)[1]),
               selectInput("pollutant_rose", "Select Pollutant:",
                           choices = c("PM2.5", "CO2"), 
                           selected = "PM2.5"),
               dateRangeInput("daterange_rose", "Select date range:",
                              start = min(data$date, na.rm = TRUE), end = max(data$date, na.rm = TRUE), #Florin added the option na.rm=TRUE
                              min = min(data$date, na.rm = TRUE), max = max(data$date, na.rm = TRUE)),
               br(),
               downloadButton("downloadWindRose", "Download Wind Rose"),
               br(),
               hr(),
               p("A wind rose displays a summary of wind speed (m/s) and wind direction.
                The length of bars extending from the centre of the plot show how frequently winds blow from different directions.
                Colour shading indicates the wind speed. e.g. a long bar extending to the top of the plot indicated northerly winds are frequent,
                if most of the bar is shaded red these winds are often fast.")
             ),
             mainPanel(
               plotOutput("windRose")
             )
           )
  ),
  
  # Polar Plot tab
  tabPanel("Polar Plot",
           sidebarLayout(
             sidebarPanel(
               h1("Polar Plot"),
               p("This tab shows a polar plot for your selected location, pollutant and date range."),
               hr(),
               selectInput("location_polar", "Select Location:",
                           choices = unique(data$Location.Name), 
                           selected = unique(data$Location.Name)[1]),
               selectInput("pollutant_polar", "Select Pollutant:",
                           choices = c("PM2.5", "CO2"), 
                           selected = "PM2.5"),
               dateRangeInput("daterange_polar", "Select date range:",
                              start = min(data$date, na.rm = TRUE), end = max(data$date, na.rm = TRUE), #Florin added the option na.rm=TRUE
                              min = min(data$date, na.rm = TRUE), max = max(data$date, na.rm = TRUE)),
               br(),
               downloadButton("downloadPolarPlot", "Download Polar Plot"),
               br(),
               hr(),
               p("A polar plot displays pollutant concentrations in relation to wind speed (m/s) and wind direction. 
                 Colour shading indicates the pollutant concentration associated with different wind directions
                 and speeds. The centre of the plot indicates pollutant concentrations when wind speed is slow.
                 The outer area of the plot indicates pollutant concentration when wind speed
                 is fastest. e.g. shading at the top of the plot relates to faster northerly winds.")
             ),
             mainPanel(
               plotOutput("polarPlot")
             )
           )
  ),
  
  # Time Variation tab
  tabPanel("Time Variation",
           sidebarLayout(
             sidebarPanel(
               h1("Time Variation"),
               p("The tab shows a time variation plot for your selected location, pollutant and date range."),
               hr(),
               selectInput("location_time", "Select Location:",
                           choices = unique(data$Location.Name),     
                           selected = unique(data$Location.Name)[1]),
               selectInput("pollutant_time", "Select Pollutant:",
                           choices = c("PM2.5", "CO2"), 
                           selected = "PM2.5"),
               dateRangeInput("daterange_time", "Select date range:",
                              start = min(data$date, na.rm = TRUE), end = max(data$date, na.rm = TRUE),
                              min = min(data$date, na.rm = TRUE), max = max(data$date, na.rm = TRUE)),
               br(),
               downloadButton("downloadTimeVariation", "Download Time Variation Plot"),
               br(),
               hr(),
               p("This plot displays pollutant concentration changes with time. 
                 There are four figures, day of the week variation, mean hour of day variation 
                 and a combined hour of day – day of week plot and a monthly plot. 
                 The plots also display a 95 % confidence interval in the mean determined by bootstrap sampling
                 of the data.")
             ),
             mainPanel(
               plotOutput("timeVariation")
             )
           )
  ),
  
  # Polar Contribution Tab
  tabPanel("Polar Contribution",
           sidebarLayout(
             sidebarPanel(
               h1("Polar Contribution Plot"),
               p("This tab shows a polar contribution plot for your selected location, pollutant and date range."),
               hr(),
               selectInput("location_freq", "Select Location:",
                           choices = unique(data$Location.Name), 
                           selected = unique(data$Location.Name)[1]),
               selectInput("pollutant_freq", "Select Pollutant (optional):",
                           choices = c("PM2.5", "CO2"), 
                           selected = "PM2.5"),
               dateRangeInput("daterange_freq", "Select date range:",
                              start = min(data$date, na.rm = TRUE), end = max(data$date, na.rm = TRUE),
                              min = min(data$date, na.rm = TRUE), max = max(data$date, na.rm = TRUE)),
               br(),
               downloadButton("downloadPolarFreq", "Download Polar Frequency Plot"),
               br(),
               hr(),
               p("A polar contribution plot displays the weighted mean of pollutant concentrations
                 for all wind directions. Colour shading indicates how much of the total pollution burden
                 is contributed by each wind direction.")
             ),
             mainPanel(
               plotOutput("polarFreqPlot")
             )
           )
  ),
  # Calendar Plot Tab
  tabPanel("Calendar Plot",
           sidebarLayout(
             sidebarPanel(
               h1("Calendar Plot"),
               p("This tab shows a daily average pollutant concentrations in the form of a calendar for your selected location, pollutant and date range."),
               hr(),
               selectInput("location_calendar", "Select Location:",
                           choices = unique(data$Location.Name), 
                           selected = unique(data$Location.Name)[1]),
               selectInput("pollutant_calendar", "Select Pollutant:",
                           choices = c("PM2.5", "CO2"), 
                           selected = "PM2.5"),
               dateRangeInput("daterange_calendar", "Select date range:",
                              start = min(data$date, na.rm = TRUE), end = max(data$date, na.rm = TRUE),
                              min = min(data$date, na.rm = TRUE), max = max(data$date, na.rm = TRUE)),
               br(),
               downloadButton("downloadCalendarPlot", "Download Calendar Plot"),
             ),
             mainPanel(
               plotOutput("CalendarPlot")
             )
           )
  ),
  tabPanel("About",
           h1("About this App"),
           p("This app has been designed to aid air pollution investigations using measured data from low-cost sensors. Maps and graphics are provided that show sensor locations, pollutant conentrations and wind conditions through time."),
           p("Air quality data is collected from", 
             tags$a("AirGradient", href = "https://www.airgradient.com", target = "_blank"),
             "sensors, and modelled wind data has been downloaded from",
             tags$a("Visual Crossings", href = "https://www.visualcrossing.com", target = "_blank"), "Data analysis is performed using the R OpenAir package (Carslaw and Ropkins, 2012) and is presented using an R shiny application (Chang et al 2025). The app provides interactive graphics which are described below. Further explanation of the plots provided in this tool can be found in the", tags$a("OpenAir Manual", href = "https://davidcarslaw.com/files/openairmanual.pdf", target = "_blank"),"."),
           h4("Windrose"),
           p("A wind rose displays a summary of wind speeds and wind directions. This is useful for air pollution investigations because it allows us to understand which wind conditions happen most often and which are less common. The most common winds could be responsible for bringing the most air pollution to a location. It’s also important to look at the least common winds, for example if there is a major pollution source nearby, but winds rarely bring that pollution to the monitoring location it could be missed. In the wind-rose, the data are summarised into wind direction sectors and wind speed categories. Bars starting from the centre of the plt show the frequency that the wind is from each direction. A colour code is used to show the wind speed on each bar. This makes it easy to see which wind directions are most common and what proportion of those winds are fast or slow."),
           h4("Polar plot"),
           p("Polar Plots display a summary of pollutant concentrations, wind speeds and wind directions on one figure. This helps to identify patterns and sources of air pollution. Concentration observations are linked to the corresponding wind speed and wind direction data. Concentration observations are shown using colour shading. Concentrations that occurred when wind speeds where low are shown at the centre of the plot. Concentrations that occurred when wind speeds where faster are shown progressively further from the centre. So the concentration associated with fast winds from the north is shown at the top of the plot, the concentration associated with moderate winds from the west is shown towards the left of the plot. The polar plot is useful for quickly identifying the location of potential sources of air pollution. When higher concentrations are associated with a  particular wind direction this can indicate the direction to the source. If there is more than one air pollution monitor operating locally, multiple polar plots can be used to triangulate source locations."),
           h4("Time variation"),
           p("The way air pollutant concentrations change with time can help identify the likely sources of the pollution. Road traffic emissions often follow daily and weekly cycles linked to the busiest times for travel. Major industrial sources can follow patterns linked to the industrial process, or whether or not a facility such as a power plant is operating or not. Natural sources like desert dust or sea salt aerosols, may have daily or seasonal patterns linked to weather conditions for example. This plot displays pollutant concentration changes with time. There are four figures, day of the week variation, mean hour of day variation and a combined hour of day – day of week plot and a monthly plot. The plots also display a 95 % confidence interval in the mean determined by bootstrap sampling of the data."),
           h4("Polar Contribution"),
           p("This polar contribution plot identifies which wind direction makes the biggest contribution to pollution loading at the monitoring site. This is useful for identifying sources that, if tackled, have the greatest potential to reduce exposure. The plot displays the weighted mean of pollutant concentrations for all wind directions. Colour shading indicates how much of the total pollution burden is contributed by each wind direction."),
           h4("Calendar plot"),
           p("The Calendar plot provides an intuitive visualisation of pollution concentrations. The familiar calendar layout can be useful for communication and engagement with the public or stakeholders and to relate measured data to personal experience."),
           h4("References"),
           p("Carslaw, D. C. and K. Ropkins, (2012) Openair --- an R package for air quality data analysis.Environmental Modelling & Software. Volume 27-28, 52-61."),
           p("Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J, Dipert A, Borges B (2025). shiny: Web Application Framework for R. R package version 1.10.0.9001, https://shiny.posit.co/."),
  ),
)


###############################
###############################
###############################

####################
########### Define server logic
server <- function(input, output, session) {
  
  #### server logic for Wind Rose tab
  # Reactive: Filtered data for Wind Rose tab
  filtered_data_rose <- reactive({
    req(input$daterange_rose)
    subset(data,
           Location.Name == input$location_rose &
             date >= input$daterange_rose[1] &
             date <= input$daterange_rose[2])
  })
  
  # Render Wind Rose
  output$windRose <- renderPlot({
    windRose(filtered_data_rose(), pollutant = input$pollutant_rose,
             main = paste0("Wind Rose ", input$location_rose, " ", input$pollutant_rose),
             type = "default")
  })
  
  # Download Wind Rose
  output$downloadWindRose <- downloadHandler(
    filename = function() {
      paste0("Wind Rose_", input$location_rose, "_", input$pollutant_rose, ".png")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      polarPlot(filtered_data_rose(), pollutant = input$pollutant_rose, type = "default")
      dev.off()
    }
  )
  
  #### server logic for Polar Plot tab
  # Reactive: Filtered data for Polar Plot tab
  filtered_data_polar <- reactive({
    req(input$daterange_polar)
    subset(data,
           Location.Name == input$location_polar &
             date >= input$daterange_polar[1] &
             date <= input$daterange_polar[2])
  })
  
  # Render Polar Plot
  output$polarPlot <- renderPlot({
    polarPlot(filtered_data_polar(), pollutant = input$pollutant_polar,
              main = paste0("Polar Plot ", input$location_polar, " ", input$pollutant_polar),
              type = "default")
  })
  
  # Download Polar Plot
  output$downloadPolarPlot <- downloadHandler(
    filename = function() {
      paste0("Polar_Plot_", input$location_polar, "_", input$pollutant_polar, ".png")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      polarPlot(filtered_data_polar(), pollutant = input$pollutant_polar, type = "default")
      dev.off()
    }
  )
  
  #### server logic for Time Variation tab 
  # Reactive: Filtered data for Time Variation tab
  filtered_data_time <- reactive({
    req(input$daterange_time)
    subset(data,
           Location.Name == input$location_time &
             date >= input$daterange_time[1] &
             date <= input$daterange_time[2])
  })
  
  # Render Time Variation Plot
  output$timeVariation <- renderPlot({
    timeVariation(filtered_data_time(), pollutant = input$pollutant_time,
                  main = paste0("Time Variation ", input$location_time, " ", input$pollutant_time),) 
  })
  
  # Download Time Variation Plot
  output$downloadTimeVariation <- downloadHandler(
    filename = function() {
      paste0("Time_Variation_", input$location_time, "_", input$pollutant_time, ".png")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      timeVariation(filtered_data_time(), pollutant = input$pollutant_time)
      dev.off()
    }
  )
  
  #### server logic for Polar Frequency tab
  # reactive - filtered data for Polar Frequency tab
  filtered_data_freq <- reactive({
    req(input$daterange_freq)
    subset(data,
           Location.Name == input$location_freq &
             date >= input$daterange_freq[1] &
             date <= input$daterange_freq[2] & #Aidan added next line
             ws > 0) #Aidan removed all data with zero wind speed
  })
  
  # Render Polar Freq Plot
  output$polarFreqPlot <- renderPlot({
    polarFreq(filtered_data_freq(), pollutant = input$pollutant_freq,
              main = paste0("Polar Contribution Plot ", input$location_freq, " ", input$pollutant_freq),
              statistic = "weighted.mean", ws.int = 100,offset = 100, trans = FALSE, col = "heat")
  }) #Aidan edited the plot so that it only looks at wind direction
  
  # Download Polar Freq Plot
  output$downloadPolarFreq <- downloadHandler(
    filename = function() {
      paste0("Polar_Contribution_", input$location_freq, "_", input$pollutant_freq, ".png")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      if (input$pollutant_freq == "None") {
        polarFreq(filtered_data_freq())
      } else {
        polarFreq(filtered_data_freq(), pollutant = input$pollutant_freq)
      }
      dev.off()
    }
  )
  
  #### server logic for Calendar Plot tab 
  # Reactive: Filtered data for Calendar Plot tab
  filtered_data_calendar <- reactive({
    req(input$daterange_calendar)
    subset(data,
           Location.Name == input$location_calendar &
             date >= input$daterange_calendar[1] &
             date <= input$daterange_calendar[2])
  })
  
  # Render Calendar Plot
  output$CalendarPlot <- renderPlot({
    calendarPlot(filtered_data_calendar(), pollutant = input$pollutant_calendar,
                 main = paste0("Calendar Plot ", input$location_calendar, " ", input$pollutant_calendar))
  })
  
  # Download Calendar Plot
  output$downloadCalendarPlot <- downloadHandler(
    filename = function() {
      paste0("Calendar_", input$location_calendar, "_", input$pollutant_calendar, ".png")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      calendarPlot(filtered_data_calendar(), pollutant = input$pollutant_calendar)
      dev.off()
    }
  )
}

######################################
######################################

# Run the application
shinyApp(ui = ui, server = server)



