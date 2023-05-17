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
library(raster)

#Load WorldClim data for mean temperatures
WC5<-raster("~/Users/Joe/Documents/College/04- Climate data/wc2.1_30s_tavg/wc2.1_30s_tavg_01.tif")

#create column for year
WC5 <- mutate(WC5, year=year(TN$DATE))

#create column for month
WC5 <- mutate(WC5, month=month(TN$DATE))

## create column for julian date##
WC5$julian_date <- yday(WC5$DATE)