## R code to manipulate and plot climate data relating to the APSU Farm ##
## Written by Joe Joe Endris ##

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(multcomp)
library(multcompView)
library(lubridate)
library(readxl)
library(gridExtra)
library(MuMIn)

##################################
### Data entry and preparation ###
##################################

#Load NOAA Climate Data Online data (not used right now)
TN<-read.csv("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/5- Climate/Tennessee.csv")

#Load PRISM data
#TN_PRISM<-read.csv("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/5- Climate/Tennessee_PRISM.csv")

#create column for year
TN <- mutate(TN, year=year(TN$Date))

## create column for julian date##
TN$julian_date <- yday(TN$DATE)

#omit NA in temperature recordings 
TN<-TN[complete.cases(TN[,5]),]

###########################
### Last freeze by year ###
###########################

#calculate last day below freezing for each year
last_freeze <- TN%>%
  filter(TMIN< -2)%>%
  filter(year(DATE)>1979)%>%
  filter(julian_date<180)%>%
  group_by(year(DATE))%>%
  filter(row_number()==n())

#calculate mean last freeze for TN since 1980
mean(as.numeric(last_freeze$julian_date))

#################################
### Absolute Low Temp by Year ###
#################################

#Determine absolute coldest day by year
as.Date(TN_PRISM$DATE)

yearly_TMIN <- TN_PRISM %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(temp = min(TMIN))

###############################
### Mean Low Temps by Month ###
###############################

#calculate mean monthly low
TN_monthly_low <- 

## create graph for temps by month of year ##
TN_TMIN_plot <-
  ggplot(TN_PRISM, aes(x = month, y = TMIN)) +
  geom_line() +
  labs(title = "Annual Lowest Temperatures",
       y= "Temperature (Celcius)",
       x= "Year") + 
  theme_bw(base_size = 15)

TN_TMIN_plot

#Number of Days Below -2
TN_freeze <- TN %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(n=sum(TMIN < -2))

#plot Number of Days Below zero
TN_freeze %>%
  filter(as.integer(year)>1980)%>%
  filter(n>0)%>%
  ggplot(aes(x = year, y = n)) +
  geom_point(color = "grey") +
  geom_smooth(method="lm")+
  labs(title = "Number of Days Below Zero",
       subtitle = "Tuscaloosa, AL",
       y= "Number of Days",
       x= "Year") + theme_bw(base_size = 15)


TN_freeze %>%
  filter(n>0)%>%
  filter(year>1980)

#Calculate mean temperature by Julian date
TN_mean <- TN %>%
  group_by(julian_date) %>%
  summarise(mean_low = mean(TMIN))

TN_mean$julian_date = as.numeric(as.character(TN_mean$julian_date))

#plot mean temperature by julian date
TN_mean_plot <- ggplot(TN_mean, aes(x= julian_date, y=mean_low))+
  xlim(1,135)+
  geom_point(color = "grey") +
  geom_smooth(method="lm")+
  labs(title = "Mean Low Temperture by Julian Date",
       subtitle = "Tuscaloosa, AL",
       y= "Temperature (C)",
       x= "Julian Date") + theme_bw(base_size = 15)

TN_mean_plot


## plot monthly mean low temps for Jan-May ##

TN_month_mean <- TN %>%
  group_by(month=lubridate::floor_date(DATE, "month")) %>%
  summarise(mean_low = mean(TMIN))

TN_month_mean$month2<-month(TN_month_mean$month)
sub<-TN_month_mean%>%
  filter(month2%in%c(1,2,3,4,5))

ggplot(sub,aes(x=month,y=mean_low))+
  geom_point()+
  facet_wrap(~month2,scales="free")+
  geom_smooth(method="lm")

TN_monthly_mean_plot <- ggplot(TN_month_mean, aes(x= month, y=mean_low))+
  geom_point(color = "grey") +
  geom_smooth(method="lm")+
  labs(title = "Monthly Mean Low Temperture",
       subtitle = "Tuscaloosa, AL",
       y= "Temperature (C)",
       x= "Month") + theme_bw(base_size = 15)

TN_monthly_mean_plot
