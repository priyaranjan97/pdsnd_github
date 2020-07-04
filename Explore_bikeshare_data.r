
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)     #First 5 entries of the new york city dataset

head(wash)   #First 5 entries of the washington dataset

head(chi)    

library(ggplot2)

class(ny$Start.Time)    # as the datatype is factor we have to convert it into date to fetch the month from the column.

ny$New.Start.Time=strptime(ny$Start.Time,format = '%Y-%m-%d %H:%M:%S')          

class(ny$New.Start.Time)

library(lubridate)     #this is the library to fetch date & time from a factor datatype

table(month(ny$New.Start.Time))

#we have 6 months data so we are scaled our x axis to 6 units

qplot(x=month(New.Start.Time),data=ny,bins=6,binwidth=0.5,
     xlab='Month of travel',
     ylab='Number of travels',
     main='Number of travels per month')+
scale_x_continuous(breaks=seq(1,6,1))

qplot(x=wday(New.Start.Time,label=TRUE),data=ny,
     xlab='Day of week',
     ylab='Number of travels',
     main='Number of travels per day of week')

table(wday(ny$New.Start.Time,label=TRUE))

qplot(x=hour(New.Start.Time),data=ny,bins=1,binwidth=0.5,
     xlab='Hour of travel',
     ylab='Number of travels',
     main='Number of travels every hour')+
scale_x_continuous(breaks=seq(0,23,1))

table(hour(ny$New.Start.Time))

station_start=data.frame(table(ny$Start.Station))                   #creating a new data frame for start.Stations and its frequencies

station_start=station_start[with(station_start,order(-Freq)),]    # descending the dataset using freq column

head(station_start)

dim(station_start)

station_start2=head(station_start,10)    #it is not possible to plot all the entries because there are 636 entries,so i am using top 10 entries here

station_start2

dim(station_start2)

barplot(station_start2$Freq,
     xlab='Start Station name',
     ylab='Number of travels',
     main='Number of travels from Start Stations',
       names.arg=station_start2$Var1,las=2)

qplot(x=Var1,y=Freq,data=station_start2,
     xlab='Start Station names',
     ylab='Number of travels',
     main='Number of travels from Start Stations')+
theme(axis.text.x = element_text(angle = 90) )

station_end=data.frame(table(ny$End.Station))    #creating a new data frame for End Stations and its frequencies

station_end=station_end[with(station_end,order(-Freq)),]  # descending the dataset using freq column

dim(station_end)

station_end2=head(station_end,10)
station_end2                         #it is not possible to plot all the entries because there are 638 entries,so i am using top 10 entries here

qplot(x=Var1,y=Freq,data=station_end2,
     xlab='End Station names',
     ylab='Number of travels',
     main='Number of travels from End Stations')+
theme(axis.text.x = element_text(angle = 90) )

a=data.frame(table(ny$Start.Station,ny$End.Station))  
a=a[with(a,order(-Freq)),]                          #creating a new data frame from combination of start.Station and end station used in a trip and its frequencies
head(a)

a$stations=paste(a$Var1,' & ',a$Var2)   # making a new column in the data frame by combining start station and end station

dim(a)

a2=head(a)      #it is not possible to plot all the entries because there are 405768 of entries,so i am using top 10 entries here

qplot(x=stations,y=Freq,data=a2,
     xlab='Stations names used for travelling in a trip',
     ylab='Number of travels',
     main='Number of travels from combination of start station and end station')+
theme(axis.text.x = element_text(angle = 90) )

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

wash$Country='Washington'
ny$Country='NewYorkCity'
chi$Country='Chicago'

new=rbind(ny,chi)      #concating two datasets i.e ny and chi

dim(new)

wash$Gender=NA
wash$Birth.Year=NA

new=rbind(new,wash)      #concating wash dataset with already combined datasets ny and chi

dim(new)

library(dplyr)          #installing this library to perform group by function

table1=summarise_at(group_by(new,Country),vars(Trip.Duration),funs(sum(.,na.rm=TRUE)))     #performing group by
table1

barplot(table1$Trip.Duration,
     xlab='Country name',
     ylab='Total travel time by users',
     main='Total travel time by users in particular city',
       names.arg=table1$Country,las=1)

table1=summarise_at(group_by(new,Country),vars(Trip.Duration),funs(mean(.,na.rm=TRUE)))   #performing group by
table1

barplot(table1$Trip.Duration,
     xlab='Country name',
     ylab='Avg travel time by users',
     main='Avg travel time by users in particular city',
       names.arg=table1$Country,las=1)

system('python -m nbconvert Explore_bikeshare_data.ipynb')
