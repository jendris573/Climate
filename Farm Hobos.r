#Code to plot and examine temperatures from the woodlot on the APSU Farm
#written by Joe Endris

library(lubridate)
library(tidyr)
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(multcomp)
library(multcompView)
library(gridExtra)
library(MuMIn)

##################################
### Data entry and preparation ###
##################################

#Load canopy temperature data
canopy_temps <- read.csv("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/5- Climate/canopy.csv")

#convert the data_time column to a date class
canopy_temps$date_time <- as.numeric(canopy_temps$date_time)
canopy_temps$date_time <- as.Date(canopy_temps$date_time)
class(canopy_temps$date_time)

#create column for year
canopy_temps <- mutate(canopy_temps, year=year(canopy_temps$date_time))

#create column for month
canopy_temps <- mutate(canopy_temps, month=month(canopy_temps$date_time))

## create column for julian date##
canopy_temps$julian_date <- yday(canopy_temps$date_time)

#load ground temperature data
ground_temps <- read.csv("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/5- Climate/ground.csv")

#convert the data_time column to a date class
ground_temps$date_time <- as.numeric(ground_temps$date_time)
ground_temps$date_time <- as.Date(ground_temps$date_time)
class(ground_temps$date_time)

#create column for year
ground_temps <- mutate(ground_temps, year=year(ground_temps$date_time))

#create column for month
ground_temps <- mutate(ground_temps, month=month(ground_temps$date_time))

## create column for julian date##
ground_temps$julian_date <- yday(ground_temps$date_time)


#####################################
### Plots for canopy temperatures ###
#####################################

ggplot(canopy_temps, aes(x = julian_date, y = temperature, color=year, group=year)) +
  geom_line() +
  xlab("Date") +
  ylab("Temperature (°C)") +
  theme_bw()+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
