library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(shiny)
library(DT)
library(rsconnect)
library(leaflet)
rm(list = ls())

# setwd("~/DATA-332/uber_project")

### GRAPHS
trips_x_hour <- read.csv('trips_x_hour.csv')
trips_x_hour_x_month <- read.csv('trips_x_hour_x_month.csv')
trips_x_day <- read.csv('trips_x_day.csv')
trips_x_day_x_month <- read.csv('trips_x_day_x_month.csv')
trips_x_base_x_month <- read.csv('trips_x_base_x_month.csv')

### HEATMAPS
trips_x_hour_x_day <- read.csv('trips_x_hour_x_day.csv')
trips_x_weekday_x_hour <- read.csv('trips_x_weekday_x_hour.csv')
trips_x_month_x_week <- read.csv('trips_x_month_x_week.csv')
trips_x_base_x_weekday <- read.csv('trips_x_base_x_weekday.csv')

### LEAFLET
trips_x_coordinates <- read.csv('trips_x_coordinates.csv')

### PREDICTION MODEL
model <- lm(count ~ weekday + hour, data = trips_x_weekday_x_hour)
summary(model)

model_resid <- resid(model)

resid_df <- data.frame(x = trips_x_weekday_x_hour$weekday, resid = model_resid)

### DEFINE UI
ui <- navbarPage(
  "Uber Rides Data",
  
  tabPanel("Trips by the hour", plotOutput('plot1'), HTML("<p>This graph is useful because it 
                                                          shows how many uber trips are taken 
                                                          during each hour of the day.</p>")),
  tabPanel("Trips by hour by the month", plotOutput('plot2'), HTML("<p>This graph shows the amount of uber 
                                                                   trips taken during each hour 
                                                                   depending on the month. This is 
                                                                   useful because you can see how 
                                                                   the times people uber change as 
                                                                   the weather changes from month 
                                                                   to month.</p>")),
  tabPanel("Trips by the day", DT::dataTableOutput('table1'), HTML("<p>This data table shows the amount of
                                                                   uber trips taken during each day of each 
                                                                   month as an aggregate. This data table 
                                                                   isn't as useful to actually use because
                                                                   it is not specific enough in my opinion.</p>")),
  tabPanel("Trips by day by the month", plotOutput('plot3'), HTML("<p>This graph shows the amount of trips taken
                                                                  on each day of each month. This graph can be very
                                                                  useful because it can show daily trends month by 
                                                                  month.</p>")),
  tabPanel("Trips by base by the month", plotOutput('plot4'), HTML("<p>This graph shows the amount of trips in 
                                                                   each base in each month. This could be useful for
                                                                   internal use to make management decisions about 
                                                                   each base and its productivity.</p>")),
  tabPanel("Trips by hour by the day", plotOutput('heatmap1'), HTML("<p>This heatmap shows the busiest times for uber
                                                                    trips. It shows the busiest times by the hour and day. 
                                                                    This graph could be useful for external or internal use
                                                                    at uber.</p>")),
  tabPanel("Trips by weekday by the hour", plotOutput('heatmap2'), HTML("<p>This heatmap shows the busiest times for uber 
                                                                        trips during the week. This graph is very useful 
                                                                        for customers to see when the best/worst times 
                                                                        to take an uber trip are.</p>")),
  tabPanel("Trips by month by the week", plotOutput('heatmap3'), HTML("<p>This heatmap shows the busiest weeks in each month. 
                                                                      This is useful to see when the busiest times for uber are.</p>")),
  tabPanel("Trips by base by the weekday", plotOutput('heatmap4'), HTML("<p>This heatmap shows the busiest weekdays for each 
                                                                        base. This could be useful for internal use or decision
                                                                        making regarding the bases.</p>")),
  tabPanel("Leaflet Map", leafletOutput('map1'), HTML("<p>This leaflet map shows where each uber trip was taken. This is useful
                                                      to see the busiest areas where these uber trips occurred.</p>")),
  tabPanel("Prediction Model Residual Plot", plotOutput('plot5'), HTML("<p>This graph shows the residuals of the prediction 
                                                                       model I created. You can read the distribution of the 
                                                                       residuals for possible skews in the data.</p>"))
)

### DEFINE SERVER
server<-function(input,output){
  
  output$plot1 <- renderPlot({
    ggplot(trips_x_hour, aes(x = hour, y = count, fill = 'blue')) +
      geom_bar(stat = 'identity') +
      scale_fill_manual(values = c("blue"))
  })
  
  output$plot2 <- renderPlot({
    ggplot(trips_x_hour_x_month, aes(x = hour, y = count, fill = month)) +
      geom_bar(stat = 'identity') +
      scale_fill_gradient(low = "yellow", high = "blue")
  })
  
  output$table1 <- DT::renderDataTable(trips_x_day[,c('day', 'count')], options = list(pageLength = 4))
  
  output$plot3 <- renderPlot({
    ggplot(trips_x_day_x_month, aes(x = day, y = count, fill = month)) +
      geom_bar(stat = 'identity') +
      scale_fill_gradient(low = "yellow", high = "blue")
  })
  
  output$plot4 <- renderPlot({
    ggplot(trips_x_base_x_month, aes(x = base, y = count, fill = month)) +
      geom_bar(stat = 'identity') +
      scale_fill_gradient(low = "yellow", high = "blue")
  })
  
  output$heatmap1 <- renderPlot({
    ggplot(trips_x_hour_x_day, aes(x = day, y = hour)) +
      geom_tile(aes(fill = count)) + 
      scale_fill_gradient(low = "green", high = "red")
  })
  
  output$heatmap2 <- renderPlot({
    ggplot(trips_x_weekday_x_hour, aes(x = weekday, y = hour)) +
      geom_tile(aes(fill = count)) + 
      scale_fill_gradient(low = "green", high = "red")
  })
  
  output$heatmap3 <- renderPlot({
    ggplot(trips_x_month_x_week, aes(x = month, y = week)) +
      geom_tile(aes(fill = count)) + 
      scale_fill_gradient(low = "green", high = "red")
  })
  
  output$heatmap4 <- renderPlot({
    ggplot(trips_x_base_x_weekday, aes(x = weekday, y = base)) +
      geom_tile(aes(fill = count)) + 
      scale_fill_gradient(low = "green", high = "red")
  })
  
  output$map1 <- renderLeaflet({
    leaflet_map <- leaflet(trips_x_coordinates) %>%
      addTiles() %>%
      addMarkers(~lon, ~lat, popup = ~as.character(count))
  })
  
  output$plot5 <- renderPlot({
    ggplot(resid_df, aes(resid)) +
      geom_histogram(color = "white", fill = "black") +
      labs(x = "Residual", y = "Count")
  })
  
}

### RUN SHINY APP
shinyApp(ui = ui, server = server)

