setwd("F:/ACRM/Dataset/Bike Sharing/Dataset")

load("HolidayDivvy.Rda")

#write.csv(Divvy, file = "Divvy.csv", row.names = F)

dataset <- Divvy

Divvy$starttime <- NULL
Divvy$stoptime <- NULL
Divvy$events <- NULL

str(Divvy)
summary(Divvy)


################# Trip Duration ################## 


#Box plot - Trip Duration per day


calcmed <- function(x) {
  return(c(y = 8, label = round(median(x), 1)))
  # modify 8 to suit your needs
}


library(ggplot2)

ggplot(dataset, aes(x = day, y = tripduration)) + geom_boxplot() + 
  theme(axis.text.x=element_text(size=10, angle=0,hjust=0.95,vjust=0.2), 
        plot.title = element_text(hjust = 0.5)) + 
  stat_summary(fun.data = calcmed, geom = "text")+ 
    ggtitle("When customer do long bike ride?") +
  xlab('Day') + ylab('Trip Duration') +
  scale_x_discrete(breaks=c("0", "1", "2", "3", "4", "5", "6"),
                   labels=c("Monday", "Tuesday", "Wednesday",
                            "Thursday","Friday", "Saturday", "Sunday"))
  
  
#Box plot - Trip Duration by Holiday

Weekend = dataset[dataset$day == "5" | dataset$day == "6", ]
WorkingDf <- dataset[dataset$day == "1" | dataset$day == "2" |
                       dataset$day == "3" | dataset$day == "4" |
                       dataset$day == "0",]
Holiday <- dataset[dataset$Holiday == "1",]


ggplot(WorkingDf, aes(x = Holiday, y = tripduration)) + geom_boxplot() + 
  theme(axis.text.x=element_text(size=10, angle=0,hjust=0.95,vjust=0.2), 
        plot.title = element_text(hjust = 0.5)) + 
  stat_summary(fun.data = calcmed, geom = "text") + 
  ggtitle("When customer do long bike ride? - Holiday or Workding day ") +
  xlab('Day') + ylab('Trip Duration') +
  scale_x_discrete(breaks=c("0", "1"),
                   labels=c("Working day", "Holiday"))

 
head(Divvy)

#Table
library(dplyr)

WorkingDf %>% 
  group_by(as.factor(Holiday_Desc)) %>%
  summarise(Min = min(tripduration),
            Max = max(tripduration),
            Median = median(tripduration),
            IQRange = IQR(tripduration))


### Trip Duration According to per Month

dataset$month <- factor(dataset$month, levels = c("1","2","3","4","5","6","7","8",
                                                                              "9","10","11","12"))

ggplot(dataset, aes(x = month, y = tripduration)) +
  geom_boxplot()  + 
  stat_summary(fun.data = calcmed, geom = "text")

### Trip Duration According to year

ggplot(dataset, aes(x = year, y = tripduration)) +
  geom_boxplot()  + 
  stat_summary(fun.data = calcmed, geom = "text")

### Trip Duration According to hour

dataset$hour <- factor(dataset$hour, levels = c("1","2","3","4","5","6","7","8",
                                                  "9","10","11","12","13","14","15",
                                                  "16","17","18","19","20","21","22","23"))

ggplot(dataset, aes(x = hour, y = tripduration)) +
  geom_boxplot()  + 
  stat_summary(fun.data = calcmed, geom = "text")

str(dataset)

### Trip Duration According to usertype

ggplot(dataset, aes(x = usertype, y = tripduration)) +
  geom_boxplot()  +  theme(plot.title = element_text(hjust = 0.5)) + 
  stat_summary(fun.data = calcmed, geom = "text") + 
  ggtitle("Which type of user ride a bike for long time?")


ggplot(Weekend, aes(x = usertype, y = tripduration)) +
  geom_boxplot()  +  theme(plot.title = element_text(hjust = 0.5)) + 
  stat_summary(fun.data = calcmed, geom = "text") + 
  ggtitle("Which type of user ride a bike for long time during weekend?")


ggplot(WorkingDf, aes(x = usertype, y = tripduration)) +
  geom_boxplot()  +  theme(plot.title = element_text(hjust = 0.5)) + 
  stat_summary(fun.data = calcmed, geom = "text") + 
  ggtitle("Which type of user ride a bike for long time during working day?")


### Trip Duration According to gender

ggplot(dataset, aes(x = gender, y = tripduration)) +
  geom_boxplot()  + theme(plot.title = element_text(hjust = 0.5)) + 
  stat_summary(fun.data = calcmed, geom = "text") + 
  ggtitle("Trip duration : Men Vs Women")



#####################Ride Transaction ################################

### Number of transaction per year

library(dplyr)
TransactionPerYear <- dataset %>% group_by(year) %>% tally()
TransactionPerYear


ggplot(TransactionPerYear, aes(x = year, y = n)) +
  geom_bar(stat = "identity")

### Average transaction per day in 2017

Data2017 <- dataset[dataset$year == "2017", ]
AvgTrip2017 <- Data2017 %>% group_by(year,month,week,day) %>% tally()
mean(AvgTrip2017$n)

### Average transaction per day in 2018

Data2018 <- dataset[dataset$year == "2018", ]
AgTrip2018 <- Data2018 %>% group_by(year,month,week,day) %>% tally()
mean(AgTrip2018$n)

### Number of transaction per month

str(dataset)

dataset2017 <- dataset[dataset$year == "2017", ]
dataset2018 <- dataset[dataset$year == "2018",]

levels(dataset2017$month)

dataset2017Q12 <- dataset2017[dataset2017$month == c("1","2","3","4","5","6"),]
dataset2017Q3Q4 <- dataset2017[dataset2017$month == c("8","9","10","11","12","7"),]

#2017

TransactionPerMonth <- dataset2017 %>% group_by(month) %>% tally()
TransactionPerMonth
str(TransactionPerMonth)

#Sorting Ascending - 
TransactionPerMonth$month <- factor(TransactionPerMonth$month, levels = c("1","2","3","4","5","6","7","8",
                                                                          "9","10","11","12"))
ggplot(TransactionPerMonth, aes(x = month, y = n)) +
  geom_bar(stat = "identity")

#2018

TransactionPerMonth18 <- dataset2018 %>% group_by(month) %>% tally()
TransactionPerMonth18
TransactionPerMonth18$month <- factor(TransactionPerMonth18$month, levels = c("1","2","3","4","5","6","7","8",
                                                                              "9","10","11","12"))

ggplot(TransactionPerMonth18, aes(x = month, y = n)) +
  geom_bar(stat = "identity")


### Transaction per week 

#Count of transaction according to day - 

dataset %>% group_by(day) %>% summarise(no_rows = length(day)) #No of counts for weekday is more than weekend

TransactionPerDay <- dataset %>% group_by(day) %>% tally()
TransactionPerDay


ggplot(TransactionPerDay, aes(x = day, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Total Number of trips from Monday to Sunday in the year 2017 and 2018")+
  xlab('Day') + ylab('Number of trips') +
  scale_x_discrete(breaks=c("0", "1","2","3","4","5","6"),
                   labels=c("Monday", "Tuesday","Wednesday","Thursday","Friday",
                            "Saturday","Sunday"))


# Average transaction in weekend

NoSat <- 52+53
NoSun <- 52+53

AvgTransSat <- 584845/NoSat
AvgTransSun <- 552317/NoSun

### Transaction per Holiday

TransactionPerHoliday <- dataset %>% group_by(Holiday_Desc) %>% tally()
TransactionPerHoliday <- TransactionPerHoliday[c(1,2,3,4,5,6,7,9,10,11),] #excluding No Holiday 
TransactionPerHoliday


ggplot(TransactionPerHoliday, aes(x = Holiday_Desc, y = n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2))

#Average transation on Holiday

TransactionPerHoliday$n <- TransactionPerHoliday$n / 2
TransactionPerHoliday

##### Compairing Average transaction at holiday and weekend

AvgTrasaction <- TransactionPerHoliday

Holiday_Desc <- c("Saturday","Sunday")
n <- c(5570,5260)

Weekdf <- data.frame(Holiday_Desc, n)
Weekdf

AvgTrasaction <- rbind(AvgTrasaction,Weekdf)
AvgTrasaction

library(ggplot2)

ggplot(AvgTrasaction, aes(x = Holiday_Desc, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2))+
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Total Number of trips in Holidays and Weekend")+
  xlab('Holiday') + ylab('Number of trips') 



####2017

TransactionPerHoliday <- dataset2017 %>% group_by(Holiday_Desc) %>% tally()
TransactionPerHoliday <- TransactionPerHoliday[c(1,2,3,4,5,6,7,9,10,11),] #excluding No Holiday 
TransactionPerHoliday

ggplot(TransactionPerHoliday, aes(x = Holiday_Desc, y = n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2))


#2018

TransactionPerHoliday <- dataset2018 %>% group_by(Holiday_Desc) %>% tally()
TransactionPerHoliday <- TransactionPerHoliday[c(1,2,3,4,5,6,7,9,10,11),] #excluding No Holiday 
TransactionPerHoliday

ggplot(TransactionPerHoliday, aes(x = Holiday_Desc, y = n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2))

str(dataset)


## Number of transation per usertype

TransactionUserType <- dataset %>% group_by(usertype) %>% tally()
TransactionUserType

ggplot(TransactionUserType, aes(x = usertype, y = n)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = n, x = usertype , y = n), vjust = -0.6)

## Number of transation per Gender

TransactionperGender <- dataset %>% group_by(gender) %>% tally()
TransactionperGender

ggplot(TransactionperGender, aes(x = gender, y = n)) +
  geom_bar(stat = "identity", fill = "blue")+
  geom_text(aes(label = n, x = gender , y = n), vjust = -0.6)+
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Total Number of trips by Gender")+
  xlab('Gender') + ylab('Number of trips') 

#write.csv(dataset, file = "FullData.csv", row.names = F)


## Relation Between Number of Transaction and dp cpacity

#Dp capacity Start 

DpCapacityStart <- dataset %>% group_by(dpcapacity_start, from_station_id) %>% tally()
str(DpCapacityStart)
tail(DpCapacityStart)
DpCapacityStart$dpcapacity_start <- as.factor(DpCapacityStart$dpcapacity_start)

DpCapacityStart$from_station_id <- NULL
str(DpCapacityStart)
tail(DpCapacityStart)

DpCapacityStart <- aggregate( n ~ dpcapacity_start, DpCapacityStart, mean )
DpCapacityStart <- DpCapacityStart[c(2:17),]

ggplot(DpCapacityStart, aes(x = dpcapacity_start, y = n)) +
  geom_bar(stat = "identity", fill = "blue")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Number of trips and Number of docks available at pick up station") +
  xlab("Number of docks")+ ylab("Number of transaction")



#Dp Capacity End

str(dataset)

DpCapacityEnd <- dataset %>% group_by(dpcapacity_end, to_station_id) %>% tally()

DpCapacityEnd$dpcapacity_end <- as.factor(DpCapacityEnd$dpcapacity_end)

DpCapacityEnd$from_station_id <- NULL
str(DpCapacityEnd)
tail(DpCapacityEnd)

DpCapacityEnd <- aggregate( n ~ dpcapacity_end, DpCapacityEnd, mean )
DpCapacityEnd <- DpCapacityEnd[c(2:17),]

ggplot(DpCapacityEnd, aes(x = dpcapacity_end, y = n)) +
  geom_bar(stat = "identity")

############################## Peak Time #############

PeakTime <- dataset %>% group_by(hour, day) %>% tally()


PeakTime$hour <- as.character(PeakTime$hour)
PeakTime$hour <- as.numeric(PeakTime$hour)

ggplot(data = PeakTime, aes(x = hour , y = n, group = day)) +
  geom_line(aes(colour = day)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Peak time") +
  xlab("Hour")+ ylab("Number of Trips")
  
str(dataset)


## Holiday

PeakTimeHoliday <- Holiday %>% group_by(hour, Holiday_Desc) %>% tally()
PeakTimeHoliday

PeakTimeHoliday$hour <- as.character(PeakTimeHoliday$hour)
PeakTimeHoliday$hour <- as.numeric(PeakTimeHoliday$hour)

ggplot(data = PeakTimeHoliday, aes(x = hour , y = n, group = Holiday_Desc)) +
  geom_line(aes(colour = Holiday_Desc)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Peak time") +
  xlab("Hour")+ ylab("Number of Trips")

str(dataset)



############### Behaviour of customer ride more than 30 mins, What are there station combination #########

opp <- dataset[dataset$tripduration > 30, ]
str(opp)

library(dplyr)
Station <- opp %>% group_by(from_station_id, to_station_id) %>% tally()

rownames(Station) <- do.call(paste, c(Station[c("from_station_id", "to_station_id")], sep = "-"))

library(data.table)
setDT(Station, keep.rownames = TRUE)[]
colnames(Station)[1] <- "Station_Combination"

Station <- arrange(Station, desc(n))

Top10Station<- Station[1:10,]
str(Top10Station)
Top10Station$Station_Combination <- as.factor(Top10Station$Station_Combination)

library(ggplot2)
ggplot(Top10Station, aes(x = Station_Combination, y = n)) +
  geom_bar(stat = "identity", fill = "blue")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Top Station combination when customer ride more than 30 mins") +
  xlab("Station combination")+ ylab("Average Number of Trips")


#################### In week days, Which station comnination was popular ##########

library(dplyr)
WeekStation <- WorkingDf %>% group_by(from_station_id, to_station_id) %>% tally()

rownames(WeekStation) <- do.call(paste, c(WeekStation[c("from_station_id", "to_station_id")], sep = "-"))

WeekStation
library(data.table)
setDT(WeekStation, keep.rownames = TRUE)[]
colnames(WeekStation)[1] <- "Station_Combination"

WeekStation <- arrange(WeekStation, desc(n))

Top10StationWeek <- WeekStation[1:10,]
str(Top10StationWeek)
Top10StationWeek$Station_Combination <- as.factor(Top10StationWeek$Station_Combination)

library(ggplot2)
ggplot(Top10StationWeek, aes(x = Station_Combination, y = n)) +
  geom_bar(stat = "identity", fill = "blue" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Top Station combination in Week days") +
  xlab("Station combination")+ ylab("Average Number of Trips")



######Weekend 

library(dplyr)
WeekendStation <- Weekend %>% group_by(from_station_id, to_station_id) %>% tally()

rownames(WeekendStation) <- do.call(paste, c(WeekendStation[c("from_station_id", "to_station_id")], sep = "-"))

WeekendStation
library(data.table)
setDT(WeekendStation, keep.rownames = TRUE)[]
colnames(WeekendStation)[1] <- "Station_Combination"

WeekendStation <- arrange(WeekendStation, desc(n))

Top10StationWeekend <- WeekendStation[1:10,]
str(Top10StationWeekend)
Top10StationWeekend$Station_Combination <- as.factor(Top10StationWeekend$Station_Combination)

library(ggplot2)
ggplot(Top10StationWeekend, aes(x = Station_Combination, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Top Station combination in Weekend ") +
  xlab("Station combination")+ ylab("Average Number of Trips")

##### Holiday

library(dplyr)
HolidayStation <- Holiday %>% group_by(from_station_id, to_station_id) %>% tally()

rownames(HolidayStation) <- do.call(paste, c(HolidayStation[c("from_station_id", "to_station_id")], sep = "-"))

HolidayStation
library(data.table)
setDT(HolidayStation, keep.rownames = TRUE)[]
colnames(HolidayStation)[1] <- "Station_Combination"

HolidayStation <- arrange(HolidayStation, desc(n))

Top10StationHoliday <- HolidayStation[1:10,]
str(Top10StationHoliday)
Top10StationHoliday$Station_Combination <- as.factor(Top10StationHoliday$Station_Combination)

library(ggplot2)
ggplot(Top10StationHoliday, aes(x = Station_Combination, y = n)) +
  geom_bar(stat = "identity", fill = "blue") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Top Station combination in holidays ") +
  xlab("Station combination")+ ylab("Average Number of Trips")


###### What type of the users are ?

## Number of transation per usertype

oppUser <- opp %>% group_by(usertype) %>% tally()


ggplot(oppUser, aes(x = usertype, y = n)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = n, x = usertype , y = n), vjust = -0.6)

## Number of transation per Gender

oppGender <-opp %>% group_by(gender) %>% tally()

ggplot(oppGender, aes(x = gender, y = n)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = n, x = gender , y = n), vjust = -0.6)


#Subseeting Data for top 10 station combination

str(opp)

opp$Station_Combination <- do.call(paste, c(opp[c("from_station_id", "to_station_id")], sep = "-"))
opp$Station_Combination <- as.factor(opp$Station_Combination)


FinalData <- opp[opp$Station_Combination == "247-4" |
           opp$Station_Combination == "177-177" |
           opp$Station_Combination == "157-157" |
           opp$Station_Combination == "177-35" |
           opp$Station_Combination == "35-177" |
           opp$Station_Combination == "35-35" |
           opp$Station_Combination == "4-247" |
           opp$Station_Combination == "4-4" |
           opp$Station_Combination == "459-177" |
           opp$Station_Combination == "76-177",]

str(FinalData)

holiday <- FinalData %>% group_by(Holiday) %>% tally()
holiday #56 transaction out of 1738 

mean(FinalData$tripduration) #36.96
mean(FinalData[FinalData$Holiday == 0, "tripduration"]) #Normal Day - 36.98
mean(FinalData[FinalData$Holiday == 1, "tripduration"]) #Bank Holiday - 36.31

mean(FinalData[FinalData$day == 0, "tripduration"]) #Monday - 36.49
mean(FinalData[FinalData$day == 1, "tripduration"])#Tuesday -38.04
mean(FinalData[FinalData$day == 2, "tripduration"]) #Wednesday - 36.42
mean(FinalData[FinalData$day == 3, "tripduration"]) #Thursday - 36.21
mean(FinalData[FinalData$day == 4, "tripduration"]) #Friday - 36.41
mean(FinalData[FinalData$day == 5, "tripduration"]) #Satuday - 36.84
mean(FinalData[FinalData$day == 6, "tripduration"]) #Sunday - 37.5


CustomerType <- FinalData %>% group_by(usertype) %>% tally()
CustomerType #Subscriber

str(FinalData)


############ Weekend 2018 ###################################


#In weekend which station are popular ?

Weekend = dataset2018[dataset2018$day == "5" | dataset2018$day == "6", ]
  
write.csv(Weekend, file = "weekend.csv")

sum(is.na(Weekend$usertype))
sum(is.na(dataset$usertype))
sum(is.na(dataset2018$usertype))

#In holiday which staion are popular

Holiday <- dataset2018[dataset2018$Holiday == "1",]

write.csv(Holiday, file= "Holiday.csv")


## Number of transation per usertype in weekend

WeekendUser <- Weekend %>% group_by(usertype) %>% tally()
WeekendUser
AvgWeekendUser <- 246/106 


ggplot(WeekendUser, aes(x = usertype, y = n)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = n, x = usertype , y = n), vjust = -0.6)

#Numnber of transaction per user type in working day

WorkingDf <- dataset2018[dataset2018$day == c("1","2","3","4"),]

WorkingUser <- WorkingDf%>% group_by(usertype) %>% tally()
WorkingUser

AvgWorkingUser <- 117/259


#Number of transaction per user type in Holiday

Holidaydf <- dataset2018[dataset2018$Holiday == "1",]

HolidayUser <- Holidaydf%>% group_by(usertype) %>% tally()
HolidayUser

AvgHolidayUser <- 29/11

### Compairing the customer rate according to day

Day <- c("Working day", "Weekend", "Holiday")
CustomerUserate <- c(0.45,2.32,2.63)

CustomerRate <- data.frame(Day,CustomerUserate)

ggplot(CustomerRate, aes(x = Day, y = CustomerUserate)) +
  geom_bar(stat = "identity", fill = "blue")+
  geom_text(aes(label = CustomerUserate, x = Day , y = CustomerUserate), vjust = -0.6)+
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("When customer buy single ride pass? ") +
  ylab("Average Number of trip per hour")


## Number of transation per Gender in weekend

TransactionperGender <- dataset %>% group_by(gender) %>% tally()
TransactionperGender


ggplot(TransactionperGender, aes(x = gender, y = n)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = n, x = gender , y = n), vjust = -0.6)

####################### Pollution ###########



Pollution <- read.csv("2018Data.csv")
sum(is.na(Pollution$PM.2.5))

mean(Pollution$n)

#Replacing the missing value of PM2.5 by Moving average of window 7 days

library(imputeTS)

Pollution$PM.2.5 <- imputeTS::na.ma(Pollution$PM.2.5, k = 7, weighting = "simple")
sum(is.na(Pollution$PM.2.5))

#Correlation

cor(Pollution$n , Pollution$PM.2.5, method = "pearson") #0.048

#Scatter plot

plot(Pollution$PM.2.5, Pollution$n, main="Scatterplot Example", 
     xlab="Pm 2.5 ", ylab="Transaction ", pch=19)
abline(lm(Pollution$n~Pollution$PM.2.5), col="red")




