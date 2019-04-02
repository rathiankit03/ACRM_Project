### Importing data

setwd("F:/ACRM/Project/Dataset")

#2017

Data2017Q1 <- read.csv("Divvy_Trips_2017_Q1.csv")
Data2017Q2 <- read.csv("Divvy_Trips_2017_Q2.csv")
Data2017Q3 <- read.csv("Divvy_Trips_2017_Q3.csv")
Data2017Q4 <- read.csv("Divvy_Trips_2017_Q4.csv")

DataStation2017Q1Q1 <- read.csv("Divvy_Stations_2017_Q1Q2.csv")
colnames(DataStation2017Q1Q1)[1]<- "from_station_id"
str(DataStation2017Q1Q1)

DataStation2017Q3Q4 <- read.csv("Divvy_Stations_2017_Q3Q4.csv")
colnames(DataStation2017Q3Q4)[1]<- "from_station_id"
str(DataStation2017Q3Q4)

#2018
Data2018 <- read.csv("Divvy_Trips_2018_Q1.csv")



### Merging the data

Data17Q1Merge <- merge(Data2017Q1, DataStation2017Q1Q1, by = "from_station_id", all = TRUE )
Data17Q2Merge <- merge(Data2017Q2, DataStation2017Q1Q1, by = "from_station_id", all = TRUE )
Data17Q3Merge <- merge(Data2017Q3, DataStation2017Q3Q4, by = "from_station_id", all = TRUE )
Data17Q4Merge <- merge(Data2017Q4, DataStation2017Q3Q4, by = "from_station_id", all = TRUE )

str(Data17Q1Merge)
str(Data17Q4Merge)

Data17Q3Merge$X <- NULL
Data17Q4Merge <- NULL

### Concanating the data

Data <- rbind(Data17Q1Merge,Data17Q2Merge,Data17Q3Merge,Data17Q4Merge)

######################### Cleaning the data ###############################

#Removing unnecessary column
Data$birthyear <- NULL
Data$name <- NULL
Data$city <- NULL
Data$online_date <- NULL

#Checking the null Values in dataframe

nacount <- sapply(Data, function(y) sum(length(which(is.na(y)))))
nacount

#Removing the column with null value

Data <- Data[complete.cases(Data),]

nacount <- sapply(Data, function(y) sum(length(which(is.na(y)))))
nacount


#Extracting time information into year, month,week, day

library(lubridate)
library(dplyr)

Data <- Data %>% mutate(hours = hour(strptime(start_time, '%m/%d/%Y %H:%M')) %>% as.factor())
str(Data)

Data <- Data %>% mutate(years = year(strptime(start_time, '%m/%d/%Y %H:%M')) %>% as.factor())
str(Data)

Data <- Data %>% mutate(months = month(strptime(start_time, '%m/%d/%Y %H:%M')) %>% as.factor())
str(Data)

Data <- Data %>% mutate(weeks = week(strptime(start_time, '%m/%d/%Y %H:%M')) %>% as.factor())
str(Data)


#### Arranging the Data

Data$start_time <- NULL
Data$end_time <- NULL

write.csv(Data, file = "CleanedData.csv", row.names = F)
