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

  
