#Code to plot and examine temperatures from the APSU Farm
#written by Joe Endris

library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(readxl)
library(writexl)

setwd("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/5- Climate/")

canopy_temps <- read_excel("Canopy.xlsx")

ggplot(canopy_temps, aes(x = date_time, y = temperature)) +
  geom_point(size = 0.1) +
  geom_smooth(stat="smooth")+
  xlab("Date") +
  ylab("Temperature (°C)") +
  theme_bw()
