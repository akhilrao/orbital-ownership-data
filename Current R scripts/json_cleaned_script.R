#####
#API Queries by apogee used to obtain json files
# <800: https://www.space-track.org/basicspacedata/query/class/satcat/OBJECT_TYPE/PAYLOAD/APOGEE/%3C800/orderby/INTLDES%20asc/emptyresult/show
#  800-1999: https://www.space-track.org/basicspacedata/query/class/satcat/OBJECT_TYPE/PAYLOAD/APOGEE/800,800--2000/orderby/INTLDES%20asc/emptyresult/show
# 2000-33999: https://www.space-track.org/basicspacedata/query/class/satcat/OBJECT_TYPE/PAYLOAD/APOGEE/2000,2000--34000/orderby/INTLDES%20asc/emptyresult/show
# >33999: https://www.space-track.org/basicspacedata/query/class/satcat/APOGEE/>33999/OBJECT_TYPE/PAYLOAD/orderby/INTLDES asc/emptyresult/show
#####

rm(list=ls())

library(jsonlite)
library(lubridate)
library(tidyverse)
library(dplyr)
library(readr)

setwd("../Space-Track data/Payload Data")

### Reads JSON files
apogee_lessthan_800 <- read_json("payload_lessthan_800.json", simplifyVector = TRUE)
apogee_800_1999 <- read_json("payload_800_1999.json", simplifyVector=TRUE)
apogee_2000_33999 <- read_json("payload_2000_33999.json", simplifyVector=TRUE)
apogee_greaterthan_33999 <- read_json("payload_greater_33999.json", simplifyVector=TRUE)

### Merges JSON files, changes norad variable name
all_apogees <- rbind_pages(list(apogee_lessthan_800, apogee_800_1999, apogee_2000_33999, apogee_greaterthan_33999))
all_apogees <- rename(all_apogees, c("NORAD_number" = "NORAD_CAT_ID"), c("Entity_Country" = "COUNTRY"),
                      c("Date_of_First_Launch" = "LAUNCH"))

### Lists if there are duplicate entries, there should be none
duplicates <- duplicated(all_apogees$NORAD_number)

### Changes launch date and decay date from chr to date format
all_apogees$Date_of_First_Launch <- as.Date(all_apogees$Date_of_First_Launch)
all_apogees$DECAY <- as.Date(all_apogees$DECAY)
  #make sure to change this to the same format as the UCS data, i.e. "%m/%d/%Y"

### Changes multiple variables from chr to their proper formats
all_apogees$NORAD_number <- as.numeric(all_apogees$NORAD_number)
all_apogees$APOGEE <- as.numeric(all_apogees$APOGEE)
all_apogees$PERIGEE <- as.numeric(all_apogees$PERIGEE)
all_apogees$PERIOD <- as.numeric(all_apogees$PERIOD)
all_apogees$INCLINATION <- as.numeric(all_apogees$INCLINATION)

### Sorts the data alphabetically by NORAD number
all_apogees <- arrange(all_apogees, NORAD_number)


### Sets the working directory, loads the UCS combined .csv file
getwd()
setwd("../../UCS data/Output")

#read in combined UCS data
all_UCS <- read_csv("UCS_Combined_Data.csv")

#merge UCS data with JSON data, using NORAD as the key
JSpOC_UCS_data <- merge(all_apogees, all_UCS, by=c("NORAD_number"))

#Keeping decay information (Ethan)
#This keeps all NORAD entries but we only want the ones we have UCS data for
#alt_JSpOC_UCS_data <- merge(all_apogees, all_UCS, by=c("NORAD_number"), all.y=TRUE, sort = TRUE)

#renaming variables that occur in both data sets

JSpOC_UCS_data <- rename(JSpOC_UCS_data, c("SATNAME_JSpOC" = "SATNAME.x"), c("SATNAME_UCS" = "SATNAME.y"), 
                         c("Date_Of_First_Launch_JSpOC" = "Date_of_First_Launch.x"), 
                         c("Date_Of_First_Launch_UCS" = "Date_of_First_Launch.y"),
                         c("Launch_site_UCS" = "Launch_site"),c("Launch_site_JSpOC" = "SITE"))


#Up until here, the DECAY variable is recorded correctly. This information is lost once we 


### Sorts the data frame alphabetically by entity name
#JSpOC_UCS_data <- arrange(JSpOC_UCS_data, Entity_Name)

#set output directory
setwd("../../Space-Track data")
#setwd("/Users/ethanberner/Dropbox/Orbit-use economics data assembly/Space-Track data")
#write output csv
#write_csv(JSpOC_UCS_data, "JSpOC_UCS_data.csv", na="")

write.csv(JSpOC_UCS_data, "JSpOC_UCS_data.csv") #, na="")

#Sanity Check - Decay Entries
#Initial NA's by apogee interval
NA_1 <- length(which(is.na(apogee_lessthan_800$DECAY)))
NA_2 <- length(which(is.na(apogee_800_1999$DECAY)))
NA_3 <- length(which(is.na(apogee_2000_33999$DECAY)))
NA_4 <- length(which(is.na(apogee_greaterthan_33999$DECAY)))
NA_total <- sum(NA_1, NA_2, NA_3, NA_4)  #5472
NA_all_apogees <- sum(is.na(all_apogees$DECAY)) #5472 = no data loss through binding operations, this includes all NORADS (even the ones not int he UCS data)
NA_all_apogees <- sum(is.na(all_apogees$DECAY)&all_apogees$NORAD_number>=3029) #5281, not all of which are in the UCS data

#NA's relevant for UCS data
dups<- duplicated(JSpOC_UCS_data[,c("NORAD_number")]) #Boolean vector
ipt_unique <- JSpOC_UCS_data[!dups,] #Vector with non-duplicates
sum(is.na(ipt_unique$DECAY)) #number of satellites with NA as decay date, 3199



