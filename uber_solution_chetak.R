## Install and load stringr, & ggplot2 packages

install.packages("stringr")	
library(stringr)	

install.packages("ggplot2")	
library(ggplot2)

## Data Cleaning and Preparation

#Importing the Uber Request Data.csv file as a dataframe in R. 
#Making sure that strings are not treated as factors and blanks are also treated as NA values.

uber <- read.csv ("Uber Request Data.csv", header = TRUE, fill = TRUE, stringsAsFactors = FALSE, na.strings = c("", "NA"))

#Replacing '/' with '-' to make date format consistent.

uber$Request.timestamp <- str_replace_all(uber$Request.timestamp, "/", "-")

uber$Drop.timestamp <- str_replace_all(uber$Drop.timestamp, "/", "-")


#Converting timestamp into standard format YYYY-MM-DD HH:MM

uber$Request.timestamp <- as.POSIXlt(uber$Request.timestamp, format = "%d-%m-%Y %H:%M")

uber$Drop.timestamp <- as.POSIXlt(uber$Drop.timestamp, format = "%d-%m-%Y %H:%M")


#Extracting the Request hour in which trip request was made

uber$Request.hour <- as.numeric(format(uber$Request.timestamp, "%H"))

#Extracting the Drop hour in which trip was completed

uber$Drop.hour <- as.numeric(format(uber$Drop.timestamp, "%H"))


#Plotting the hourly demand at Airport and at City. 
#This will help in identifying specific hours, that can be aggregarted in time slots

ggplot(uber, aes(factor(Request.hour), fill = Pickup.point)) + geom_bar() + facet_wrap(~Pickup.point) + labs(x="Request Hour", y="Demand")


#Plotting the hourly request status at Airport & City. 
#This will help in identifying specific hours, that can be aggregarted in time slots

ggplot(uber, aes(factor(Request.hour), fill = Pickup.point)) + geom_bar() + facet_wrap(~Pickup.point~Status) + labs(x="Request Hour")


##Based on the above two plots, following timeslots are defined for further analysis

#early_morning  : 00:00:00 to 03:59:59
#morning        : 04:00:00 to 10:00 
#daytime        : 10:00:01 to 16:59:59
#evening        : 17:00:00 to 22:00:00
#night          : 22:00:01 to 23:59:59

#segregating the demand at Airport & at City into different time slots 

uber$Timeslot<-ifelse(uber$Request.hour >= 0 & uber$Request.hour <4 ,'early_morning',
              ifelse(uber$Request.hour >=4 & uber$Request.hour <=10, 'morning',
              ifelse(uber$Request.hour >10 & uber$Request.hour <17, 'daytime',
              ifelse(uber$Request.hour >=17 & uber$Request.hour <=22, 'evening', 'night'))))

#plotting the demand at Airport & at City during various time slots 

ggplot(uber, aes(Timeslot, fill = Pickup.point)) + geom_bar(position = "dodge") + facet_wrap(~Pickup.point) + labs(x="Time slot", y="Demand")


## 1. Visually identify the most pressing problems for Uber. 

#making a sub-set only for those requests that are not completed.

Uber_demand_not_met<- uber[which(uber$Status != "Trip Completed"),]

#plotting requests that got 'cancelled' or show 'no cars available' across the time slots.

ggplot(Uber_demand_not_met, aes(Timeslot, fill = Status)) + geom_bar() + facet_wrap(~Pickup.point) + geom_text(stat = "count", position = "stack", aes(label=..count..),vjust=.75, size=3)



##2. Find out the gap between supply and demand and show the same using plots.

#identifying Demand-Supply gap

# Total demand = All requests (Trip Completed + No Cars Available + Cancelled)
# Total supply = Requests Honoured (Trip Completed)
# Demand-Supply Gap = Requests Not Honoured (No Cars Available + Cancelled)

uber$Demand_Supply_Gap <-ifelse(uber$Status == "Trip Completed",'Request honoured','Request not honoured')


#plotting the Demand-Supply gap to identify the time slots when the highest gap exists
# i.e maximum count for Request not honoured

ggplot(uber, aes(Timeslot, fill = Demand_Supply_Gap)) + geom_bar() + geom_text(stat = "count", position = 'stack', aes(label=..count..),vjust=0.75, size=3)


#Making subset only for 'morning' & 'evening' time slots for requests that are not hnoured 
# i.e these requests are demand-supply gap

Uber_mor_eve <- uber[which((uber$Timeslot=='evening' | uber$Timeslot=='morning') & uber$Demand_Supply_Gap=='Request not honoured'),]


#plotting the Demand-Supply gap for 'morning' & 'evening' time slots

ggplot(Uber_mor_eve, aes(Timeslot, fill = Demand_Supply_Gap)) + geom_bar() + geom_text(stat = "count", position = 'stack', aes(label=..count..),vjust=0.75, size=3) + facet_wrap(~Pickup.point)
