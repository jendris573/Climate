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

#Load NOAA Climate Data Online data
TN<-read.csv("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/5- Climate/Tennessee.csv")

#Load PRISM data
#TN_PRISM<-read.csv("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/5- Climate/Tennessee_PRISM.csv")

#omit NA in temperature recordings 
TN<-TN[complete.cases(TN[,5]),]

#create column for year
TN <- mutate(TN, year=year(TN$DATE))

#create column for month
TN <- mutate(TN, month=month(TN$DATE))

## create column for julian date##
TN$julian_date <- yday(TN$DATE)

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

#calculate last day below freezing for 2022
last_freeze_2022 <- TN%>%
  filter(TMIN< -2)%>%
  filter(year(DATE)==2022)%>%
  filter(julian_date<180)%>%
  group_by(year(DATE))%>%
  filter(row_number()==n())

#calculate last day below freezing for 2023
last_freeze_2023 <- TN%>%
  filter(TMIN< -2)%>%
  filter(year(DATE)==2023)%>%
  filter(julian_date<180)%>%
  group_by(year(DATE))%>%
  filter(row_number()==n())

#################################
### Absolute Low Temp by Year ###
#################################

#Determine absolute coldest day by year
as.Date(TN$DATE)

yearly_TMIN <- TN %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(temp = min(TMIN))

###############################
### Mean Low Temps by Month ###
###############################

#calculate mean monthly low
TN_monthly_low <- TN %>%
  group_by(month) %>%
  summarise(temp=mean(TMIN))

## create graph for mean low temps by month of year ##
TN_TMIN_plot <-
  ggplot(TN_monthly_low, aes(x = month, y = TMIN)) +
  geom_line() +
  labs(title = "Annual Lowest Temperatures",
       y= "Temperature (Celcius)",
       x= "Year") + 
  theme_bw(base_size = 15)+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

TN_TMIN_plot

#############################################
### Determine the number of days below -2 ###
#############################################

#Number of Days Below -2
TN_freeze <- TN %>%
  group_by(year) %>%
  summarise(total_days=sum(TMIN < -2))

#plot Number of Days Below -2 since 1980
TN_freeze_plot <- TN_freeze %>%
  filter(year > 1980)%>%
  ggplot(aes(x = year, y = total_days)) +
  geom_point(color="black") +
  geom_smooth(method="loess")+
  labs(title = "Number of Days Below -2(C)",
       subtitle = "Clarksville, TN",
       y= "Number of Days",
       x= "Year") + 
  theme_bw(base_size = 15)+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

TN_freeze_plot

############################################
### Plot mean temperature by julian date ###
############################################

#Calculate mean temperature by Julian date
TN_mean <- TN %>%
  group_by(julian_date) %>%
  summarise(mean_low = mean(TN$TMIN))

TN_mean$julian_date = as.numeric(as.character(TN_mean$julian_date))

#plot mean temperature by julian date
TN_mean_plot <- ggplot(TN_mean, aes(x= julian_date, y=mean_low))+
  xlim(1,135)+
  geom_point(color = "grey") +
  geom_smooth(method="lm")+
  labs(title = "Mean Low Temperture by Julian Date",
       subtitle = "Clarksville, TN",
       y= "Temperature (C)",
       x= "Julian Date") + theme_bw(base_size = 15)

TN_mean_plot

###############################################
### plot monthly mean low temps for Jan-May ###
###############################################

as.Date(TN$DATE)

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
       subtitle = "Clarksville, TN",
       y= "Temperature (C)",
       x= "Month") + theme_bw(base_size = 15)

TN_monthly_mean_plot
