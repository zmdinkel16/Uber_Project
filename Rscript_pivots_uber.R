library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
rm(list = ls())

setwd("~/DATA-332/uber_project")

df1 <- read.csv('uber-raw-data-apr14.csv')
df2 <- read.csv('uber-raw-data-may14.csv')
df3 <- read.csv('uber-raw-data-jun14.csv')
df4 <- read.csv('uber-raw-data-jul14.csv')
df5 <- read.csv('uber-raw-data-aug14.csv')
df6 <- read.csv('uber-raw-data-sep14.csv')

df_base <- rbind(df1, df2, df3, df4, df5, df6)

names(df_base) <- tolower(names(df_base))

### Filter to make quicker
# df_base <- head(df_base, n=200000)

### DATE FORMATTING
df_base$date.time <- as.POSIXct(df_base$date.time, format='%m/%d/%Y %H:%M')

df_base <- df_base %>%
  mutate(date = format(date.time, "%m/%d/%Y"),
         time = format(date.time, "%H:%M:%S"),
         month = format(date.time, "%m"),
         day = format(date.time, "%d"),
         year = format(date.time, "%Y"),
         hour = format(date.time, "%H"),
         minute = format(date.time, "%M"),
         week = format(date.time, "%W")) %>%
  mutate(weekday = weekdays(date.time))

df_base$month <- as.numeric(df_base$month)
df_base$day <- as.numeric(df_base$day)
df_base$year <- as.numeric(df_base$year)
df_base$hour <- as.numeric(df_base$hour)
df_base$minute <- as.numeric(df_base$minute)
df_base$week <- as.numeric(df_base$week)

df_base$week <- df_base$week -((df_base$month - 1) *4)
df_base$lat <- round(df_base$lat, digits = 2)
df_base$lon <- round(df_base$lon, digits = 2)

### PIVOTS FOR GRAPHS
trips_x_hour <- df_base %>%
  select(hour) %>%
  group_by(hour) %>%
  summarise(count = n())
write.csv(trips_x_hour, "trips_x_hour.csv", row.names = FALSE)

  
trips_x_hour_x_month <- df_base %>%
  select(hour, month) %>%
  group_by(hour, month) %>%
  summarise(count = n())
write.csv(trips_x_hour_x_month, "trips_x_hour_x_month.csv", row.names = FALSE)


trips_x_day <- df_base %>%
  select(day) %>%
  group_by(day) %>%
  summarise(count = n())
write.csv(trips_x_day, "trips_x_day.csv", row.names = FALSE)


trips_x_day_x_month <- df_base %>%
  select(day, month) %>%
  group_by(day, month) %>%
  summarise(count = n())
write.csv(trips_x_day_x_month, "trips_x_day_x_month.csv", row.names = FALSE)


trips_x_base_x_month <- df_base %>%
  select(base, month) %>%
  group_by(base, month) %>%
  summarise(count = n())
write.csv(trips_x_base_x_month, "trips_x_base_x_month.csv", row.names = FALSE)


### PIVOTS FOR HEATMAPS
trips_x_hour_x_day <- df_base %>%
  select(hour, day) %>%
  group_by(hour, day) %>%
  summarise(count = n())
write.csv(trips_x_hour_x_day, "trips_x_hour_x_day.csv", row.names = FALSE)


trips_x_weekday_x_hour <- df_base %>%
  select(hour, weekday) %>%
  group_by(weekday, hour) %>%
  summarise(count = n())
write.csv(trips_x_weekday_x_hour, "trips_x_weekday_x_hour.csv", row.names = FALSE)


trips_x_month_x_week <- df_base %>%
  select(month, week) %>%
  group_by(month, week) %>%
  summarise(count = n())
write.csv(trips_x_month_x_week, "trips_x_month_x_week.csv", row.names = FALSE)


trips_x_base_x_weekday <- df_base %>%
  select(base, weekday) %>%
  group_by(base, weekday) %>%
  summarise(count = n())
write.csv(trips_x_base_x_weekday, "trips_x_base_x_weekday.csv", row.names = FALSE)


### PIVOT FOR LEAFLET
trips_x_coordinates <- df_base %>%
  select(lat, lon) %>%
  group_by(lat, lon) %>%
  summarise(count = n())
trips_x_coordinates <- trips_x_coordinates[order(trips_x_coordinates$count, decreasing = TRUE), ]
trips_x_coordinates <- head(trips_x_coordinates, n=150)
write.csv(trips_x_coordinates, "trips_x_coordinates.csv", row.names = FALSE)

