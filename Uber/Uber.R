#Remove all objects from environment
remove(list = ls())

library(lubridate)
library(ggplot2)
library(plyr)
library(dplyr)

uber.main<-read.csv("Uber Request Data.csv",stringsAsFactors = FALSE)
uber<-uber.main

dim(uber)
str(uber)

#Assuptions
# Data is specifically between city and airport pickup point.
# All requests have one of these 3 statuses : Trip Completed, Cancelled, No Cars Available
# No drop data, driver data available when  status = “No Cars Available”
# 24 hour date format is used



#reordering the columns
uber<-uber[,c(1,3,2,4,5,6)]
#See any duplicates are there or not
sum(duplicated(uber$Request.id))
# No duplicates

#functions to check na,blank values
count_na<-function(x){return(sum(is.na(x)))}
count_blank<-function(x){return(length(which(str_trim(x,side="both")=="")))}

#Check if there is any Na or Blank values in any columns 
sapply(uber,count_na, simplify = "array")
sapply(uber,count_blank, simplify = "array")
#No blanks in any observations found

#There are  2650 Na in Driver.id ND 3914 NA IN Drop.timestamp
#Status and pickup points where there are no driverid there
table(uber[which(is.na(uber$Driver.id)),c(3,4)])
#                 Status
# Pickup.point No Cars Available
# Airport              1713
# City                  937

#Status and pickup points where there are no Drop.timestamp there
table(uber[which(is.na(uber$Drop.timestamp)),c(3,4)])
#                         Status
# Pickup.point Cancelled No Cars Available
# Airport       198              1713
# City         1066               937
table(uber$Status,uber$Pickup.point)
#                   Airport City
# Cancelled             198 1066
# No Cars Available    1713  937
# Trip Completed       1327 1504 

# Deleting Na from Driver.id and Drop.timestamp will result in data loss
# Keeping NAs as it shows different status of the requests.

#Creating variable to know weather trip is completed or not
uber<-mutate(uber,Trip.completed=ifelse(is.na(uber$Drop.timestamp),"No","Yes"))
table(uber$Trip.completed)
# No  Yes 
# 3914 2831 


#Time of Request and Drop off time, changing them into date format
uber$Request.timestamp<-parse_date_time(x = as.character(uber$Request.timestamp),
                                        orders = c("d/m/y H:M", "d-m-Y H:M:S"))
uber$Drop.timestamp<-parse_date_time(x = as.character(uber$Drop.timestamp),
                                        orders = c("d/m/y H:M", "d-m-Y H:M:S"))

uber$Request.date<-as.Date(uber$Request.timestamp)
uber$Request.hour<-format(uber$Request.timestamp,format = "%H")

uber$Drop.date<-as.Date(uber$Drop.timestamp)
uber$Drop.hour<-format(uber$Drop.timestamp,format = "%H")

#To see the frequency of requests
plot_request_freq_hour<-ggplot(uber,aes(Request.hour)) +
        geom_bar(stat = "count",fill="#CC6666")+
        geom_text(stat="count",aes(label=..count..),vjust=-0.5,size=3)

plot_request_freq_hour
#from 5-10, 17-21 hours there is sudden increase in requests 
#---calling these time as rush hours 
# "Morning Rush" 5am-10am
# "Evening Rush" 5pm-10pm"
#---other time slots will be 
# "Late night"   12am-4am
# "Afternoon"    11am-4pm
# "Night"        10pm-12am

#How many requests in Rush hours?
Rush_hour_request<-nrow(uber[which(as.numeric(uber$Request.hour)>=5 & as.numeric(uber$Request.hour)<=10),])
Rush_hour_request <-Rush_hour_request + nrow(uber[which(as.numeric(uber$Request.hour)>=17 & as.numeric(uber$Request.hour)<=21),])
#How many percentage of request uber gets in Rush hours
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
paste0("Total requests in rush hours: ", Rush_hour_request)
paste0("% requests in rush hours: ", round(Rush_hour_request/nrow(uber)*100,2),"%")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Creating ordinal category day part or time slots keeping rush hours.
day_part<-function(time){
        time<-as.numeric(as.character(time))
        return(case_when(
                between(time,0,4) ~"Late Night 12am-4am",
                between(time,5,10) ~"Morning Rush 5am-10am",
                between(time,11,16) ~"Afternoon 11am-4pm",
                between(time,17,21) ~"Evening Rush 5pm-10pm",
                between(time,22,23) ~"Night 11pm-12am",
                TRUE ~ "NA"))
}
uber<- mutate(uber,Request.day_part = day_part(Request.hour))
#This new variable is also ordinal categorical variable , so while converting 
#it from char to factor, level is specified to keep the order of day_part
uber$Request.day_part <- factor(uber$Request.day_part,levels =c("Late Night 12am-4am",
                                                                "Morning Rush 5am-10am",
                                                                "Afternoon 11am-4pm",
                                                                "Evening Rush 5pm-10pm",
                                                                "Night 11pm-12am","NA"))


uber<- mutate(uber,Drop.day_part = day_part(Drop.hour))
uber$Drop.day_part<- factor(uber$Drop.day_part,levels=c("Late Night 12am-4am",
                                                        "Morning Rush 5am-10am",
                                                        "Afternoon 11am-4pm",
                                                        "Evening Rush 5pm-10pm",
                                                        "Night 11pm-12am","NA"))
table(uber$Request.day_part)
summary(uber$Drop.day_part)



#----------------------
str(uber)
# Converting categorical variables as factors. 
# Levels will show result data in order they are specified here
uber$Pickup.point<- factor(uber$Pickup.point,levels=c("Airport","City"))
uber$Status<-factor(uber$Status,levels=c("Trip Completed","Cancelled","No Cars Available"))
uber$Trip.completed<-factor(uber$Trip.completed, levels=c("No","Yes"))

#lets see Hour, request frequency
#Here request.hour is ordinal categorycal variable, and we want to see frequency
#Bar plot is used to show descrete values and it's frequency 
#here using aesthetics for to show  other categorical variable as different color
#Facet has been used to show plot for each category in categorical variable
plot_request_freq_hour1<-ggplot(uber,aes(Request.hour,fill=Status)) +
        geom_bar(stat = "count")+ #to show frequency
        geom_text(stat="count",aes(label=..count..),vjust=-0.5,size=3)+ #to label each bar with count
        facet_grid(Status~.)+ # will show each status wise hourly request count
        scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99")) #specifying colors manually
plot_request_freq_hour1


plot_request_freq_hour2<-ggplot(uber,aes(Request.hour,fill=Pickup.point)) +
        geom_bar(stat = "count")+
        geom_text(stat="count",aes(label=..count..),vjust=-0.5,size=3)+
        facet_grid(Pickup.point~.)+# will show each Pick up point wise hourly request count
        scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"))
plot_request_freq_hour2

#Pickup point vise requests
#Airport -> sudden hike in requests in evening rush hours
#City-> sudden hike in requests in morning rush hours

plot_request_freq_hour3<-ggplot(uber,aes(Request.hour,fill=Pickup.point,color=Status)) +
        geom_bar(stat = "count")+
        geom_text(stat="count",aes(label=..count..),vjust=-0.5,size=3)+
        facet_grid(Status+Pickup.point~.)+
        scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"))
plot_request_freq_hour3

# Completed trips
# Both airports and city has same trend hourly. 
# Cancelled Trips
# Airport: very less cancellation happens from driver
# City: sudden pick in cancellation by drivers in morning hours
# No Cars Available
# Airport: evening rush hours has sudden increase in requests and there are no cars available
# City: in morning rush hours no car availability is moderate compare to airport
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Lets explore more 
# calculation is done before ploting to show % values in label
uber_freq_not_completed <- uber[which(uber$Trip.completed=="No"),]%>%
        group_by(Request.day_part,Trip.completed,Pickup.point)%>%
        dplyr::summarise(count=n(),
                         Perc_in_total=n()/nrow(uber[which(uber$Trip.completed=="No"),])*100)
head(uber_freq_not_completed)

plot_uber_freq_not_completed<-
        ggplot(uber_freq_not_completed,aes(x=Request.day_part,y=count,fill=Pickup.point)) +
        geom_bar(stat="identity",position = "dodge") +
        geom_text(stat= "identity",aes(label=count)
                  ,position = position_dodge(width=0.7),hjust=0.5,vjust=2)+ # label count
        geom_text(stat= "identity",aes(label=paste(round(Perc_in_total,2),"%"))
                  ,position = position_dodge(width=0.7),hjust=0.5,vjust=1)+ #label %
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+# to show xaxis label 45% angle
        labs(x="Time Slot",y="Count",
             title="Day time slot Vs count and percentage of Requests not completed")+
        scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"))

plot_uber_freq_not_completed
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Above plot shos how not completed trip requests are distributed among day time 
# slot and pickup points
# Trip is not completed from city in morning rush hours(5am-10am) 33.5% of total non completed trips
# Trip is not completed from Airport in Evening rush hours(5pm-10pm) 36.5 of total non completed trips
# which together contribute  70%  of total non completed trips.


#lets see Status frequency-------------------------------------------------------
table(uber$Status) 

#To see the frequency of requests in count and in pecent of total requests
plot_Status_freq<-ggplot(uber,aes(Status)) + geom_bar(stat = "count",fill="#CC6666") + 
        geom_text(stat="count",aes(label=..count..,vjust=0))+
        geom_text(stat="count",aes(label=paste(round(..count../sum(..count..)*100,2),"%"),vjust=1),color="white")
        # % is calculated in geom_text 
plot_Status_freq
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Status = Cancelled or No cars avilable result in Trip incompletion
paste("Pecentage request not completed: ", round(nrow(uber[which(uber$Trip.completed=="No"),])/nrow(uber)*100,2))
# From the plot we can see only 42% Requests gets completed 58% requests either 
# cancelled or No cars are available
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#from which location the requests are not completed?
table(uber$Pickup.point) 
# Airport    City 
# 3238    3507
table(uber$Pickup.point,uber$Trip.completed)
#         FALSE TRUE
# Airport  1911 1327
# City     2003 1504

ggplot(uber,aes(Pickup.point),fill="#CC6666") + 
        geom_bar(stat = "count") + 
        geom_text(stat="count",aes(label=..count..,vjust=2))+
        geom_text(stat="count",aes(label=paste(round(..count../sum(..count..)*100,2),"%"),vjust=1),
                  color="white")

#Above chart shows requests from city and airport has around 4% less requests (demand)

#Trip is not completed vs Pickup point (Status as asthetics)
plot_PP_freq1<-uber[which(uber$Trip.completed=="No"),]%>%
        ggplot(aes(Pickup.point,fill=Status)) + 
        geom_bar(stat = "count",position = "stack") + 
        geom_text(stat="count",aes(label=..count..,vjust=2),position = position_stack())+
        geom_text(stat="count",aes(label=paste(round(..count../sum(..count..)*100,2),"%"),vjust=1),
                  color="white",position = position_stack())+
        labs(x="Pickup point ",y="Count",title="Pickup point Vs Trip Not Completed", size=2)+
        scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"))
plot_PP_freq1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot_PP_freq1 :
# From Not completed trips only 5% gets cancelled at airport
# 44% time request made, it is from airport and "no cars available"
# 51% time request made and not fulfilled its from city.
# Almost more than half of the time requests get cancelled from driver in city(27%)
# other times no cars available in city when trip is not completed (24%)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Demand = Requests made by customer
#Supply = Cars available for ride
#Status="Trip Completed" or "Cancelled" by driver, car is available ==> Supply
#Demand=Total no of requests

## let see demand and supply in Airport and city 

#if demand is 100 supply = 
Total_demand<-nrow(uber)
Airport_demand<-nrow(uber[which(uber$Pickup.point=="Airport"),])
Airport_supply<-nrow(uber[which(uber$Pickup.point=="Airport" & uber$Status%in%c("Trip Completed","Cancelled")),])
City_demand<-nrow(uber[which(uber$Pickup.point=="City"),])
City_supply<-nrow(uber[which(uber$Pickup.point=="City" & uber$Status%in%c("Trip Completed","Cancelled")),])
paste("If demand is 100$ supply = " ,round(nrow(uber[which(uber$Trip.completed=="Yes"),])/Total_demand*100,2),"%")
paste("Total demand at Airport = ", round(Airport_demand/Total_demand*100,2),"%")
paste("Total supply at Airport = ", round(Airport_supply/Total_demand*100,2),"%")
paste("Total demand at City = ", round(City_demand/Total_demand*100,2),"%")
paste("Total supply at City = ", round(City_supply/Total_demand*100,2),"%")

#Create a new variable to refelect demand and supply in population
uber<-mutate(uber,Trip.DemandSupply = ifelse(Status%in%c("Trip Completed","Cancelled"),"Supply","Demand Supply Gap"))

# Above plot shows location wise demand and supply
plot_PP_freq2<-ggplot(uber,aes(Pickup.point,fill=Trip.DemandSupply)) + 
        geom_bar(stat = "count",position = "stack") + 
        geom_text(stat="count",aes(label=..count..,vjust=2),position = position_stack())+
        geom_text(stat="count",aes(label=paste(round(..count../sum(..count..)*100,2),"%"),vjust=1),
                  color="white",position = position_stack())+
        labs(x="Pickup point ",y="Count",title="Pickup point Vs All Requests", size=2)+
        scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"))+ 
        geom_text(aes(x="Airport",y=3400,label=paste(round(Airport_demand/Total_demand*100,2),"%")))+
        geom_text(aes(x="City",y=3600,label=paste(round(City_demand/Total_demand*100,2),"%")))
        #showing total % on top of each bar using geom_text
plot_PP_freq2

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_PP_freq<-ggplot(uber,aes(Pickup.point,fill=Trip.completed)) + 
        geom_bar(stat = "count",position = "stack") + 
        facet_grid(Trip.DemandSupply~.)+
        geom_text(stat="count",aes(label=..count..,vjust=2),position = position_stack())+
        geom_text(stat="count",aes(label=paste(round(..count../sum(..count..)*100,2),"%"),vjust=1),
                  color="white",position = position_stack())+
        labs(x="Pickup point ",y="Count",title="Pickup point Vs All Requests", size=2)+
        scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"))

plot_PP_freq
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# above plots show most pressing problem, which is 
# 1) 25.4% supply gap at airport and 14% supply gap at city = total 39% requests
# 2) 16% cancellation from driver despite of supply 
#    (even after supply business is being lost) 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


avg_supply_pp<-data.frame(Pickup.point=c("Airport","City"),
                          avg_supply=c(round(Airport_supply/Airport_demand,2),
                                       round(City_supply/City_demand,2)))


Date_demand_supply<-ggplot(uber,aes(Request.date,fill=Trip.DemandSupply)) + 
        geom_area(stat = "count",position = position_fill()) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"),
                          name="No of Requests",labels=c("Demand","Supply"))+
        labs(x="Hour",y="No of Requests",
             title="Supply and demand by date")+
        #scale_fill_discrete(name="No of Requests",labels=c("Demand","Supply"))+
        facet_wrap(Pickup.point~.,ncol=1,scales = "free")+
        theme(legend.direction = "horizontal",legend.position = "top")

Date_demand_supply
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Demand supply do not vary based on date. 
# City has more supply than Airport on almost all days
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

avg_supply<-round(nrow(uber[which(uber$Trip.DemandSupply=="Supply"),])/Total_demand,2)        
hour_demand_supply<-ggplot(uber,aes(Request.hour,group=Trip.DemandSupply)) + 
        geom_bar(stat = "count",aes(fill=factor(Trip.DemandSupply)),position = position_fill()) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        labs(x="Hour",y="No of Requests",
             title="Supply and demand by hour of the day")+
        scale_fill_discrete(name="No of Requests",labels=c("Demand","Supply"))+
        facet_wrap(Pickup.point~.,ncol=1,scales = "free")+
        theme(legend.direction = "horizontal",legend.position = "top")+
        geom_hline(aes(yintercept = avg_supply))+
        geom_text(aes(0,avg_supply,label=avg_supply,vjust=-1,hjust=-2))
hour_demand_supply

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# At night from 12am to 4 am, and in evening rush 5pm to10pm  supply 
# is less then average supply.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 #rush hours and not rush hours
 uber<-mutate(uber,Request.rush_hour = ifelse(Request.day_part%in%c("Morning Rush 5am-10am","Evening Rush 5pm-10pm"),"Yes","No"))
 uber<-mutate(uber,Drop.rush_hour = ifelse(Drop.day_part%in%c("Morning Rush 5am-10am","Evening Rush 5pm-10pm"),
                                           "Yes",
                                           ifelse(Drop.day_part=="NA","Trip Failed","No")))
 
 uber$Status<-factor(uber$Status,levels=c("No Cars Available","Cancelled","Trip Completed"))
 plot_request_rush_hour<-ggplot(uber,aes(Request.rush_hour,group=Status)) + 
         geom_bar(stat = "count",aes(fill=Status),position = "fill")+
         geom_text(stat="count",aes(label=paste(round(..count../sum(..count..)*100,2),"%"),vjust=1),
                   hjust=0.5,position = position_fill(),size=2)+
         labs(x="During Rush Hours?",y="Hour",
              title="Requests in rush hours Vs Status")+
         scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"))
 plot_request_rush_hour
 
 # Observations:
 # Less availability of cars as well driver's cancellation is high in 
 # rush hours (5am-10am, 5pm-10pm)

 
 uber<-mutate(uber,Trip.duration=difftime(Drop.timestamp,Request.timestamp, unit = "min"))
# Average time for trip and standard deviation
 # Low sd means duration time is close average duration
 # High sd means duration time is spred out from average duration 
 
 # mean duration across all trip completed
 m_duration<-round(as.numeric(mean(uber$Trip.duration,na.rm=T)),2) 
 #calculation hourly mean and sd for trip duration before creating plot
 #because hour is ordinal category , converting it to numerical to show 
 # crossbar and line plot
 Plot_hourly_trip_duration<-
       uber[which(uber$Trip.completed=="Yes"),]%>%
                group_by(Request.hour,Pickup.point)%>%
                dplyr::summarise(mean_duration=as.numeric(mean(Trip.duration)),
                                sd_duration= as.numeric(sd(Trip.duration)))%>%
         ggplot(aes(as.numeric(Request.hour),mean_duration,
                    ymin=mean_duration-sd_duration,ymax=mean_duration+sd_duration,
                    color=Pickup.point)) + 
         geom_crossbar()+
         geom_line(color="red")+
         facet_wrap(Pickup.point~.,ncol=1)+
         geom_hline(aes(yintercept = m_duration))+ # horizontal bar 
         geom_text(aes(0,m_duration,label=paste("Avg:",m_duration," min"),vjust=-1,hjust=-2),color="blue")+
         labs(x="Hour",y="Average Trip Duration",
               title="Hourly average trip duration in minutes")
 Plot_hourly_trip_duration
 # Above plot shows 
 # mean trip time per hour and standard deviation per hour, horizontal line 
 # shows average time for all completed trips.
 # in sight : Traffic or trip duration is not the reason behind trip cancellation
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Hypothesis: 
#       Cancellation 
#       * Its almost an hour long ride from city to airport
#       * When driver goes from city to airport in morning rush hours(5am to 10am) 
#         because of less request from airport to city in same time slot make them wait 
#         till they get ride from airport to city. Which happens after 5pm. This results 
#         in to driver cancellation. 
#       * Less ride requests from airport to city in morning and afternoon hours discourages 
#               drivers to go to airport.
#       No Cars Available
#       * In evening rush hours and night to early morning, supply is less than 
#         average at Airport. At city its less at night and early morning hours.
#       * Less supply of drivers during evening and night.
#       Recommendation:
#        * Demand can be predicted based on flight data and drivers can be incentivized 
#          based upon supply gap.
#        * Give drivers incentive for working in Rush hours, specially from 5pm-4am.

#see final structure of data frame.
# str(uber)
# write.csv(uber,"uber.csv",row.names = FALSE)