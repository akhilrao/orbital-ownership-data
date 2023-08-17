##################################################
#Script to clean UCS .csv files
##################################################
#Overview: Loads the .csv files, renames columns, creates new variables, exports to .csv

##################################################
######
# Block 1: clear workspace, load packages
######

### Clears the Global Environment so the script can be tested, w/out deloading packages
rm(list = ls())

### Loads the relevant packages
library(lubridate)
library(tidyverse)
library(dplyr)
library(readr)
library(jsonlite)
library(data.table)

#################################################
######
# Block 2: Load data, rename/create variables in neat dataframes
######

### Gets and sets working directory - assume working from Current R Scripts folder
getwd()
setwd("../UCS data/csv files/")

### Creates an list of all .csv files in the UCS folder, as well as an empty list to store the data frames later
files <- list.files(path = "./",
                    pattern = "[.]csv", full.names = TRUE, recursive = FALSE)
file_name <- basename(files)

csv_list <- list();

### Creates a vector with only the dates of the UCS releases, i.e. deletes all the text in the file names
file_date <- gsub(x = file_name, pattern = "UCS_Satellite_Database_", replacement = "", fixed = T)
file_date <- gsub(x = file_date, pattern = ".csv", replacement = "", fixed = T)
file_date <- mdy(file_date)

#################################################
######
# Block 3: Run a loop to clean, rename, and merge the dataframes, and write the data frames as new, clean .csv files
######

#list_of_files <- list()

### Creates a for loop to run the code on all of the .csv files in the UCS folder
for (i in 1:length(files)){

	### Opens up the data set, drops the empty columns, and disregards empty rows
	clean_data <- read_csv(files[i])
	clean_data <- select(clean_data, -starts_with("X"))
	clean_data <- clean_data[rowSums(is.na(clean_data))<ncol(clean_data),]

	### Renames the clean_data columns to the names we want them to have, with underscores
	clean_data <- rename(clean_data, 
		c("Entity_Name" = "Operator/Owner"), 
		c("Entity_Country" = "Country of Operator/Owner"),
		c("Entity_Type" = "Users"), 
		c("Date_of_First_Launch" = "Date of Launch"), 
		c("Expected_Lifetime_years" = "Expected Lifetime"), 
		c("NORAD_number" = "NORAD Number"), 
		c("SATNAME" = "Name of Satellite, Alternate Names"), 
		c("Launch_Mass_kg" = "Launch Mass (kg.)"),
		c("Country_of_Contractor" = "Country of Contractor"), 
		c("Launch_vehicle" = "Launch Vehicle"),
		c("Launch_site" = "Launch Site"), 
		c("COSPAR_number" = "COSPAR Number"), 
		c("Class_of_Orbit" = "Type of Orbit")
	)
	
	### Keeps the relevant columns
	clean_data <- clean_data[, c("Entity_Name", "Entity_Type", "Contractor", "SATNAME", "Purpose", "Date_of_First_Launch",
	                             "Launch_vehicle", "Launch_site", "COSPAR_number", "NORAD_number", "Class_of_Orbit",
	                             "Expected_Lifetime_years")]

	### Sorts the data alphabetically by operator
	clean_data <- arrange(clean_data, Entity_Name)
	
	###
	clean_data$Expected_Lifetime_years <- as.numeric(sub(" yrs.| yr.| +",  "", clean_data$Expected_Lifetime_years))

	###Remove non-numeric characters
	#clean_data$Expected_Lifetime_years <- sub(" yrs.| yr.| +",  "", clean_data$Expected_Lifetime_years)
	
	###Computes averages of lifetime ranges and converts them to numeric
	#clean_data$Expected_Lifetime_years <-sapply(strsplit(clean_data$Expected_Lifetime_years, "-"), function(x) mean(as.numeric(x)))
	
	### Changes the NORAD number column from a chr to a num
	clean_data$NORAD_number <- as.numeric(clean_data$NORAD_number)

	### Changes the data type for Date_of_First_Launch from chr to Date in "%m/%d/%Y" format
	clean_data$Date_of_First_Launch <- mdy(clean_data$Date_of_First_Launch)
	
	### Creates a variable for today's date
	current_date <- Sys.Date()

	### Converts the expected lifetime of sats from years to months, and adds it as a column to the data frame
	clean_data <- mutate(clean_data, Expected_Lifetime_months = Expected_Lifetime_years * 12)

	### Rounds the months off to an even number
	clean_data$Expected_Lifetime_months <- round(clean_data$Expected_Lifetime_months)

	### Creates a variable for the expected date of deorbit for a satellite
	clean_data <- mutate(clean_data, Expected_Deorbit_Date = Date_of_First_Launch + months(Expected_Lifetime_months))
	
	### Creates a data frame that counts how often an entity name occurs (i.e. how many satellites are in each release)
	### Essentially is a count of all active sats (since only active sats are included in the UCS .csv releases)
	count <- count(clean_data, vars = Entity_Name)
	count <- rename(count, c("Entity_Name" = "vars"))

	### Merges the count data frame with the clean_data data frame
	clean_data <- merge(clean_data, count, by = "Entity_Name")

	### Renames n to Sat_Count_entity
	clean_data <- rename(clean_data, c("Sat_Count_entity" = "n"))
	
	### Creates a new data frame, grouping the data for an entity's first sat launch, merges new data frame with clean_data
	grouped <- clean_data %>% group_by(Entity_Name) %>% summarise(Entity_First_Launch = min(Date_of_First_Launch))
	clean_data <- merge(clean_data, grouped, by = "Entity_Name")

	### Puts clean data into list_of_files (pseudo code??)
	#list_of_files[[i]] <- clean_data
	
	#### Variable that records which year of the ucs data the .csv is from -  will be used to compute UCS vintage (min)
	release_year <- file_name[i]
	release_year <- gsub(x=release_year,pattern = "UCS_Satellite_Database_", replacement = "",fixed = F)
	release_year <- mdy(gsub(x=release_year,pattern = ".csv", replacement = "",fixed = T))
  
	clean_data$UCS_entry_year = release_year


	### Creates a list containing all of the individual cleaned data frames as they are ran by the loop
	csv_list[[i]] <- clean_data

	
	### Changes the working directory to the Output folder
	#change to regular output folder E
	# output_directory <- paste0("/Users/ethanberner/Dropbox/Orbit-use economics data assembly/UCS data/Output/", file_name[i])
	
	#output_directory <- paste0("/Users/gordonlewis/Dropbox/Orbit-use economics data assembly/UCS data/Output/", file_name[i])
	#uncomment below E
	output_directory <- paste0("../Output/", file_name[i])
  
	### Writes data into .csv file, na="" included so empty cells are saved as blanks
	write_csv(clean_data, output_directory, na="")
}


#################################################
######
# Block 4: Evaluate meta data frame

### Binds the list of individual cleaned data frames into a combined data frame
combined_csv <- rbindlist(csv_list)

### Creates a column for the vintage of UCS year for each satellite
combined_csv <- combined_csv %>% group_by(SATNAME) %>% mutate(vintage_of_UCS_year = min(UCS_entry_year))

### Creates a column that finds the first launch for the entity name all time
combined_csv <- combined_csv %>% group_by(Entity_Name) %>% mutate(Alltime_Entity_First_Launch = min(Entity_First_Launch))

### Creates a new data frame that counts the number of unique satellites by entity name all time, merges with combined data frame
alltime_count <- combined_csv %>% group_by(Entity_Name) %>% distinct(SATNAME) %>% mutate(Entity_sat_count_alltime = n())
combined_csv <- merge(combined_csv, alltime_count, by = c("Entity_Name","SATNAME"))

### Creates a column that shows the expected deorbit date for the last satellite by entity name
combined_csv <- combined_csv %>% group_by(Entity_Name) %>% mutate(Last_Deorbit_expected = max(Expected_Deorbit_Date, na.rm = TRUE))

# A variable for launch cohort year
combined_csv$entity_launch_cohort_year <- lubridate::year(combined_csv$Alltime_Entity_First_Launch)
#levels(as.factor(combined_csv$launch_cohort_year)) # anything funky going on here?

#Testing unique entities before sourcing, 992 (8/20), 1040 (1/21)
unique_entities_before <- length(levels(as.factor(combined_csv$Entity_Name)))

### Sources a text cleaning script to fix misspellings and other problems with the text before exporting to .csv
setwd("../../Current R scripts/")
source("UCS_text_cleaner.R")

#Testing unique entities after sourcing
unique_entities_after <- levels(as.factor(combined_csv$Entity_Name))
unique_entities_after_length <- length(levels(as.factor(combined_csv$Entity_Name)))

### Exports the combined data frame to a .csv in the output folder

setwd("../UCS data/Output")
write_csv(combined_csv, "UCS_Combined_Data.csv", na="")

###### Make some pictures!

(launch_cohorts_dist <- ggplot(combined_csv, aes(x=entity_launch_cohort_year)) + 
	geom_histogram() + 
	facet_wrap(~ Entity_Type) + 
	xlab("") + theme_bw())

(ownership_by_type <- ggplot(combined_csv, aes(x=Entity_sat_count_alltime)) +
	geom_histogram() + 
	facet_wrap(~ Entity_Type) + 
	xlab("") + theme_bw())

(ownership_by_orbit <- ggplot(combined_csv, aes(x=Entity_sat_count_alltime)) +
	geom_histogram() + 
	facet_wrap(~ Class_of_Orbit) + 
	xlab("") + theme_bw())

png("../../Output_figures/launch_cohorts_dist.png", width=1000, height=800)
launch_cohorts_dist
dev.off()

png("../../Output_figures/ownership_by_type.png", width=1000, height=800)
ownership_by_type
dev.off()

png("../../Output_figures/ownership_by_orbit.png", width=1000, height=800)
ownership_by_orbit
dev.off()
