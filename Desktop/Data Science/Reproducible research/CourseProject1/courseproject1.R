##Rep Research - Course Project 1

##Load the data  
activity<-read.csv("activity.csv")

##Checking
str(activity)
head(activity)

##Split activity 
by_date <- aggregate(steps ~ date, data=activity,sum)

##Plot using ggplot2
library(ggplot2)
ggplot(by_date, aes(steps))+
geom_histogram(fill = "steelblue2",colour="darkgreen",breaks=c(0,5000,10000,15000,20000,25000),
               position="dodge")+
        labs(y=expression("frequency"))+
        labs(x=expression('number of steps per day'))+
        labs(title=expression
             ('Histogram of the total number of steps by day'))
        
        

##Get the mean total number of steps taken per day
mean1<-mean(by_day$steps)
mean1

##Get the median ...
median1<-median(by_day$steps)
median1

##Average by 5mn-interval
by_interval<-aggregate(steps~interval,data=activity,
                       FUN=function(x){mean(x,na.rm=TRUE)})

##Time series plot by_interval~interval
ggplot(by_interval, aes(interval, steps))+geom_line()

##Obtain the 5-min interval which contains the maximum number of steps
by_interval$interval[which.max(by_interval$steps)]
##Maximum number of steps is :
range(by_interval$steps)[2]

##Count the total number of NA in the df
sum(is.na(activity))


##Fill the missing values with the 5-min interval mean
for(i in 1:length(activity$steps)){
        if(is.na(activity[i,1])){
              steps_average<-subset(by_interval,
                                    by_interval$interval==
                                            as.numeric(activity[i,3]))$steps
              activity[i,1]<-steps_average
        }
        else{
                activity[i,1]<-activity[i,1]
        }
        activity
}

##Histogram of the total number of steps taken each day

by_date <- aggregate(steps ~ date, data=activity,sum)

library(ggplot2)
ggplot(by_date, aes(steps))+
        geom_histogram(fill = "white",colour="darkgreen")+
        labs(y=expression("frequency"))+
        labs(x=expression('number of steps per day'))+
        labs(title=expression
             ('Histogram of the total number of steps by day'))

##Get the mean total number of steps taken per day
mean2<-mean(by_date$steps)

##Get the median ...
median2<-median(by_date$steps)

##Differences between 2 data's
(mean1-mean2)/mean1
(median1-median2)/median1
##Estimates values from PART1 don't differ

##Convert date to POSIXct class
activity$date<-strptime(activity$date,"%Y-%m-%d")

library(dplyr)

##Create a new column with days of the week
activity<-mutate(activity,day=weekdays(date))

##Create a factor variable with 2 levels
##levels= weekday and weekend

for(i in 1:length(activity$day)){
        if(activity[i,4]=="samedi"){
                activity[i,4]<-"weekend"
                
        }
        else{
                activity[i,4]<-"weekday"
                
        }
}
activity$day<-as.factor(activity$day)


##Time-series plot 

summary<-aggregate(activity$steps,list(interval=activity$interval,
                                       day = activity$day),mean)
names(summary)<-c("interval","day","steps")

ggplot(summary, aes(interval,steps))+geom_line(color="chartreuse4")+
        facet_wrap(~day, ncol=1)


