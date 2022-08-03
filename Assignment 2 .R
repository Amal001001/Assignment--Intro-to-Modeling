#----- GROUP Assignments-------
## Assignment 3: Intro to Modeling using ggplot.

# 
# Authors: NOUF ALJOHANI, Amal Almutairi, Salha Nasser, Rawan Alsudias, & Rahaf Alzaharani
# Last Update : Tue, 26 Jul, 2022, 
#------------------------------------------------------------------------------------------------
install.packages("tidyverse")
install.packages("lubridate")

library(ggplot2) #for data visualisation.
library(dplyr)  #for data manipulation.
library(tidyr)  #for data tidying.
library(readr) #for data import.
library(purrr) #for functional programming.
library(tibble) #for tibbles, a modern re-imagining of data frames.
library(stringr) #for strings.
library(forcats) #for factors.
library(lubridate) #for dates and datetime


 

trip_data_orginal<-read_csv("C:/Users/RA/Desktop/202112-divvy-tripdata.csv")

                                   
View(trip_data_orginal)
summary(trip_data_orginal)
#remove all duplicated values 
trip_data <- trip_data_orginal[!duplicated(trip_data_orginal$ride_id), ]

#filtering the data
#--------------------
#remove unneeded columns
trip_data <- select(trip_data, -c(ride_id))


View(trip_data)
glimpse(trip_data)
#add and change  columns types
trip_data$date <- as.Date(trip_data$started_at) #date col
trip_data$year <- format(as.Date(trip_data$date), "%Y")  #year col
trip_data$month <- format(as.Date(trip_data$started_at), "%b")   #month col
trip_data$day <- format(as.Date(trip_data$date), "%d")  #day col
trip_data$weekday <- format(as.Date(trip_data$date), "%A")  #weekday col
trip_data$start_time <- format(trip_data$started_at, format = "%H:%M")  #time started col
trip_data$end_time <- format(trip_data$ended_at, format = "%H:%M")  #time end col
trip_data$ride_length <- (as.double(difftime(trip_data$ended_at, trip_data$started_at)))/60  #length/min
View(trip_data)
#check if there is any na variables 
any(is.na(trip_data))

#remove records with na values
trip_data <- na.omit(trip_data)

View(trip_data)
#===============================================================================================================================

#Graphs
#-------------------------------- Graph-1 (Users per Type Distribution)  ------------------------

#How much data for members and casuals?

count_members<-trip_data %>% 
  group_by(member_casual) %>% 
  summarise(count = length(started_at))


# Create table 
casuals_Members <- data.frame(
  RiderType=c('Casual ','Member'),
  value=c(count_members[[2]])
)
casuals_Members


# Casuals vs members pie chart
  df <- trip_data %>% 
    group_by(member_casual) %>% # Variable to be transformed
    count() %>% 
    ungroup() %>% 
    mutate(perc = `n` / sum(`n`)) %>% 
    arrange(perc) %>%
    mutate(labels = scales::percent(perc))
  
  View(df)


  
  ggplot(df, aes(x = "", y = perc, fill = member_casual)) +
    geom_col(color = "white") +
    geom_label(aes(label = labels), color = c(1, "black"),
               position = position_stack(vjust = 0.5),
               show.legend = FALSE) +
    labs(title="Users per Type Distribution")+
    guides(fill = guide_legend(title = "User Type")) +
    coord_polar(theta = "y") + 
    theme_void()
  #--------------------------- Insights ---------------------------
  # 1- As we can see in December 202 1,members have a larger proportion of the dataset, composing 74%,48% bigger than the count of casual riders.
  
  
  
  
  #-------------------------------- Graph-2 (Number of Rides by User Type During the Week))  ------------------------
  
#weekday 


              trip_data %>% 
                mutate(weekday = wday(started_at, label = TRUE)) %>% 
                group_by(member_casual, weekday) %>% 
                summarise(number_of_rides = n()
                          ,average_duration = mean(ride_length),.groups = 'drop') %>% 
                arrange(member_casual, weekday)  %>% 
                ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
                geom_col(position = "stack") +
                labs(title = "Number of Rides by User Type During the Week",x="Days of the week",y="Number of rides", fill="User type") +
                theme(legend.position="top")     
  #--------------------------- Insights ---------------------------
  # 1-There is a significant decrease in the number of rider in both type during weekdays, especially on Sunday.
  #2-Wednesday, Thursday and Friday are the highest in the number of rider from the rest of the days.
  
  
  
  
  #--------------------------------------------------------------------------------------------------------------------------------
  #-------------------------------- Graph-3 (Average Ride Time per User Type per Weekday (min))  ------------------------
  
  trip_data %>%
    group_by(rideable_type) %>% 
    summarise(count = length(started_at),
              '%' = (length(started_at) / nrow(trip_data)) * 100,
              'members_p' = (sum(member_casual == "member") / length(started_at)) * 100,
              'casual_p' = (sum(member_casual == "casual") / length(started_at)) * 100,
              'member_casual_perc_difer' = members_p - casual_p) - casual_p
  
  #Distribution of types of bikes chart
  ggplot(trip_data, aes(rideable_type, fill=member_casual)) +
    guides(fill = guide_legend(title = "User Type"))+
    labs(x="Bike Type",y="Number of Rides", title= "Number of Rides by Bikes Types per User Type") +
    geom_bar() +
    coord_flip()
  
  
  
  #--------------------------- Insights ---------------------------
  
  #In this graph, we can see the Number of Rides based on Bike Type and User Type,
  #classic bikes have the higher number of rides, the majority of it is coming from members.
  #also, we can see that member users favored the classic type more than the electric bike,
  #whereas the casual users didn't prefer one over the other.
  #we also can see some casual users picked docked bike, this is not the case for the member users.
  
  #---------------------------------------------------------------------------------------------------------------------------------
  
#-------------------------------- Graph-4 (Average Ride Time per User Type per Weekday (min))  ------------------------
  trip_data %>%  
    group_by(member_casual, weekday) %>% 
    summarise(average_ride_length = mean(ride_length), .groups="drop") %>%
    ggplot(aes(x = weekday, y = average_ride_length, fill = member_casual)) +
    guides(fill = guide_legend(title = "User Type"))+
    geom_col(width=0.5, position = position_dodge(width=0.5)) + 
    labs(y="Average Ride Time (min)",title ="Average Ride Time per User Type per Weekday (min)")
  
  
  #--------------------------- Insights ---------------------------
  # 1- The rid time by casual customers is higher than that of customers with membership.  

  # 2- The average ride time is higher during the weekends for both customer types, especially for casual customers.

  
