##################################################
# Script to construct satellite-operator-year panel
##################################################

library(tidyverse)
library(lubridate)
library(data.table)
library(cowplot)
library(patchwork)
library(viridis)

input <- read.csv("../Space-Track data/JSpOC_UCS_data.csv")
dim(input)

#####
# Block 1: remove duplicates, generally clean up dataframe so that each row is a unique sat-operator-year entry from the latest UCS release in which it appears
#####

dups_idx <- duplicated(input[,c("COSPAR_number")]) # duplicated marks the row with the smallest row index as the original, and the subsequent ones as duplicates. This is a good quick-and-dirty approach, but it discards more recent information in favor of older info. Maybe if it was sorted first on the UCS vintage it would be better?

input_unique <- input[!dups_idx,]
#dim(input_unique)

#####
# Block 2: Generate a sequence of year-observations for each satellite. This gets us the satellite-year level; since operators are already in there, it should get us to satellite-operator-year as well.
#####

### One approach: 
# 1. give everything observations from 1967-2020 as "Year", 
# 2. calculate the "expected life left" in orbit as the difference between "Year" and "Expected_Deorbit_Date". similarly, calculate whether or not an object is in orbit yet by comparing LAUNCH_YEAR with Year
# 3. drop all observations with a negative expected time left
# PROBLEM: This approach doesn't use information that's in the JSpOC data, i.e. actual decay dates. A row *should* only get dropped if decay year is earlier than the value of Year. One way to incorporate this given the decay year variable from JSpOC: calculate another variable "Real_Time_Left" as the difference between "Year" and the decay year. Then drop every row where "Real_Life_Left" is negative.

# Step 1
soy_panel_base <- setDT(input_unique)[, .(
				Year = seq(1967,2022,by=1)
				), by = COSPAR_number]
# The above line does two things. First, setDT() makes input_unique a data.table. This allows it to be manipulated with _copy-in-place_. This (1) allows the use of [] to reference/add columns without re-specifying input_unique$ by putting them inside the .(); (2) introduces a by operator which can be used to to a calculation; and (3) speeds things up by avoiding unnecessary copying.
# The .() creates a new column, Year, which is a sequence from 1967-2020 in increments of 1 (the "by=1" here is not the as the one on the next line). This is the raw material for the satellite-level time series.
# by = COSPAR_number specifies that the new column creation specified earlier should be done once for each unique COSPAR_number. This makes sure each object has a copy of the entries of the new column.

dim(soy_panel_base)

soy_panel <- inner_join(soy_panel_base, input_unique, by = "COSPAR_number")
dim(soy_panel)

# Step 2
soy_panel <- soy_panel %>% mutate(launched_yet = sign(Year - LAUNCH_YEAR))
soy_panel <- soy_panel[soy_panel$launched_yet>=0,]
dim(soy_panel)

### Removes the day and month part of the DECAY date
soy_panel$DECAY <- as.character(soy_panel$DECAY) 
soy_panel$DECAY <- substr(soy_panel$DECAY,1,nchar(soy_panel$DECAY)-6)

### Changes DECAY column from chr to num
soy_panel$DECAY <- as.numeric(soy_panel$DECAY)

### Creates a column to record whether a DECAY entry was NA
soy_panel <- soy_panel %>% mutate(decay_na = is.na(DECAY))

### Creates column to calculate if DECAY has occurred already. If DECAY has occurred, decayed_yet == 1
soy_panel <- soy_panel %>% mutate(decayed_yet = sign(Year - DECAY))

### Assign NA values of decayed_yet with -1. This should just be those with decay_na == TRUE
soy_panel[is.na(soy_panel$decayed_yet),"decayed_yet"] <- -1

### Keeps all rows with nonnegative decayed_yet (i.e. active satellites)
soy_panel <- soy_panel[soy_panel$decayed_yet != 1,]
  #Gets rid of all positives, keeping rows with 0 and -1

write.csv(soy_panel, file="../Output_data/soy_panel_base.csv")

### Calculate mean altitude for each satellite
soy_panel <- soy_panel %>% mutate(mean_altitude = (APOGEE+PERIGEE)/2)

### Create orbit shell fixed effects
soy_panel <- soy_panel %>% mutate(ind = cut(mean_altitude, breaks = c(-Inf, c(0, seq(100, 2000,by = 50 )))), 
                                    shell_index = ind, n = 1) %>%pivot_wider(names_from = ind, values_from = n, values_fill= list(n = 0))  

soy_panel[setdiff(levels(soy_panel$shell_index), soy_panel$shell_index)] <- 0

### Create indicator variable for if an observation is in LEO
soy_panel <- soy_panel %>% mutate(LEO_indicator = mean_altitude <= 2000)

### Filter all observations that aren't in LEO
soy_panel_leo <- soy_panel %>% subset(LEO_indicator == TRUE)


soy_panel_leo_out <- soy_panel_leo %>% arrange(mean_altitude) %>% 
	select(Entity_Name, Year, COSPAR_number, NORAD_number, mean_altitude, INTLDES, DECAY, PERIOD, INCLINATION, APOGEE, PERIGEE, LAUNCH_YEAR, LAUNCH_NUM, OBJECT_NAME, Entity_sat_count_alltime, launched_yet, decayed_yet, decay_na)

write.csv(soy_panel_leo_out, file="../Output_data/soy_panel_leo.csv")


### Calculate time path of satellite stock by entity type
sat_stock_series_type <- soy_panel_leo %>% group_by(Entity_Type, Year) %>%
					summarise(sats = sum(n()) ) %>%
					ungroup(Entity_Type)
sat_stock_series_aggregate <-  soy_panel_leo %>% group_by(Year) %>%
					summarise(sats = sum(n()) ) %>%
					mutate(type = "All")
sat_stock_series_shell <-  soy_panel_leo %>% group_by(shell_index, Year) %>%
					summarise(sats = sum(n()) ) %>%
					mutate(type = "Shell")

ggplot(sat_stock_series_type) + geom_line(aes(x=Year,y=sats, group=Entity_Type, color=Entity_Type)) + theme_bw()
ggplot(sat_stock_series_aggregate) + geom_line(aes(x=Year,y=sats)) + theme_bw()

write.csv(sat_stock_series_type, file="../Output_data/sat_stock_series_type.csv")
write.csv(sat_stock_series_aggregate, file="../Output_data/sat_stock_series_aggregate.csv")
write.csv(sat_stock_series_shell, file="../Output_data/sat_stock_series_shell.csv")


### Calculating counts by entity
soy_panel_leo <- soy_panel_leo %>% group_by(Entity_Name, Year) %>% 
								mutate(Sat_Count_entity = n()) %>% 
							ungroup(Entity_Name, Year) %>% 
							group_by(Year) %>%
								mutate(mean_count_entity = mean(Sat_Count_entity),
									sd_count_entity = sd(Sat_Count_entity)) %>% 
							ungroup(Year) %>%
							group_by(Entity_Type, Year) %>%
								mutate(mean_count_entitytype = mean(Sat_Count_entity)) %>% 
							ungroup(Entity_Type,Year)

### HHI calculations
soy_panel_HHI <- soy_panel_leo %>% group_by(shell_index, Year) %>%
								mutate(count_orbit = n()) %>%
							ungroup(shell_index,Year) %>% group_by(shell_index, Year, Entity_Name) %>%
							  mutate(shares = n()/count_orbit,
							  		 squared_shares = shares^2) %>%
							  summarise(shares = mean(shares),
							  			squared_shares = mean(squared_shares),
							  			total_sats = mean(count_orbit)) %>%
							  summarise(HHI = sum(squared_shares),
							  			invHHI = 1/HHI,
							  			total_sats = mean(total_sats),
							  			shannon_entropy = -sum(shares*log(shares))) %>%
							  ungroup(shell_index,Year) %>% group_by(Year) %>%
							  mutate(mean_HHI = mean(HHI),
							  		sd_HHI = sd(HHI)) %>%
							  mutate(HHI_category = "High concentration")
soy_panel_HHI

soy_panel_HHI$HHI_category[soy_panel_HHI$HHI < 0.25] <- "Moderate concentration"
soy_panel_HHI$HHI_category[soy_panel_HHI$HHI < 0.15] <- "Low concentration"
summary(as.factor(soy_panel_HHI$HHI_category))

soy_panel_HHI <- soy_panel_HHI %>% mutate(HHI_category = factor(HHI_category, levels = c("Low concentration", "Moderate concentration", "High concentration")))
summary(as.factor(soy_panel_HHI$HHI_category))
#undo scientific notation
soy_panel_HHI <- soy_panel_HHI %>% mutate(shell_index_2 = shell_index)

soy_panel_HHI$shell_index_2 <- recode(soy_panel_HHI$shell_index_2, 
	"(950,1e+03]" = "(950,1000]",
	"(1e+03,1.05e+03]" = "(1000,1050]",
	"(1.05e+03,1.1e+03]" = "(1050,1100]",
	"(1.1e+03,1.15e+03]" = "(1100,1150]",
	"(1.15e+03,1.2e+03]" = "(1150,1200]",
	"(1.2e+03,1.25e+03]" = "(1200,1250]",
	"(1.25e+03,1.3e+03]" = "(1250,1300]",
	"(1.3e+03,1.35e+03]" = "(1300,1350]",
	"(1.35e+03,1.4e+03]" = "(1350,1400]",
	"(1.4e+03,1.45e+03]" = "(1400,1450]",
	"(1.45e+03,1.5e+03]" = "(1450,1500]",
	"(1.5e+03,1.55e+03]" = "(1500,1550]",
	"(1.55e+03,1.6e+03]" = "(1550,1600]",
	"(1.6e+03,1.65e+03]" = "(1600,1650]",
	"(1.65e+03,1.7e+03]" = "(1650,1700]",
	"(1.7e+03,1.75e+03]" = "(1700,1750]",
	"(1.75e+03,1.8e+03]" = "(1750,1800]",
	"(1.8e+03,1.85e+03]" = "(1800,1850]",
	"(1.85e+03,1.9e+03]" = "(1850,1900]",
	"(1.9e+03,1.95e+03]" = "(1900,1950]",
	"(1.95e+03,2e+03]" = "(1950,2000]"
	)
summary(soy_panel_HHI)
(soy_panel_HHI)
write.csv(soy_panel_HHI, file="../Output_data/sat_HHI_series_shell.csv")

soy_panel_big <- left_join(soy_panel_leo, soy_panel_HHI, by=c("shell_index","Year"))

write.csv(soy_panel_big, file="../Output_data/UCS-JSpOC-soy-panel-22.csv")

#####
# Block 3: Some plots
#####

(avg_fleet_size <- ggplot(data = soy_panel_leo, aes(x=Year)) +
					geom_ribbon(aes(ymin=pmax(mean_count_entity-sd_count_entity,0),ymax=mean_count_entity+sd_count_entity), alpha = 0.15) + 
					geom_line(aes(y=mean_count_entity), size = 1.1) + 
					geom_line(aes(y=mean_count_entitytype, group = Entity_Type, color = Entity_Type), size = 0.7) +
					scale_color_viridis(discrete=TRUE, option="D") +
					theme_bw() + labs(x="Year",y="Satellites",title="Average fleet size (black = across all types)", color="Entity type")) + guides( color = FALSE)

(orbit_pops <- ggplot(data = soy_panel_HHI, aes(x=Year)) + 
					geom_line(aes(y=total_sats, group = shell_index, color = shell_index), size = 1.1) + 
					scale_color_viridis(discrete=TRUE, option="C") +
					theme_bw() + labs(x="Year",y="Satellites",title="Orbital populations", color="Orbit class"))

(orbit_HHI <- ggplot(data = soy_panel_HHI, aes(x=Year)) +
					geom_line(aes(y=HHI, group = shell_index, color = shell_index), size = 1.1) + 
					geom_line(aes(y=mean_HHI), size = 1.25) +
					geom_ribbon(aes(ymin=pmax(mean_HHI-sd_HHI,0),ymax=pmin(mean_HHI+sd_HHI,1)), alpha = 0.15) + 
					scale_color_viridis(discrete=TRUE, option="C") +
					theme_bw() + labs(x="Year",y="HHI",title="Orbital concentrations", color="Orbit class"))

(orbit_HHI_2 <- ggplot(data = soy_panel_HHI) + 
					geom_tile(aes(x=Year, y=shell_index_2, z=HHI, fill=HHI)) +  
					scale_fill_viridis(option="C") + 
					labs(x = "Year", y = "Altitude shell", fill = "Orbital\nownership HHI") +theme_bw())

(orbit_HHI_scaled <- ggplot(data = soy_panel_HHI) + 
					geom_tile(aes(x=Year, y=shell_index_2, fill=total_sats*HHI)) +  
					scale_fill_viridis(option="C") + 
					labs(x = "Year", y = "Altitude shell", fill = "HHI", title = "Activity-scaled orbital-use HHI") +theme_bw())

(orbit_shannon <- ggplot(data = soy_panel_HHI) + 
					geom_tile(aes(x=Year, y=shell_index_2, z=shannon_entropy, fill=shannon_entropy)) +  
					scale_fill_viridis(option="C") + 
					labs(x = "Year", y = "Altitude shell", fill = "Nats", title = "Orbital-use Shannon entropy") +theme_bw())


(orbit_invHHI <- ggplot(data = soy_panel_HHI) + 
					geom_tile(aes(x=Year, y=shell_index_2, fill=invHHI)) +  
					scale_fill_viridis(option="C") + 
					labs(x = "Year", y = "Altitude shell", fill = "Operators", title = "Effective number of operators (inverse HHI)") +theme_bw())

(orbit_invHHI_sats <- ggplot(data = soy_panel_HHI) + 
					geom_tile(aes(x=Year, y=shell_index_2, fill=total_sats/invHHI)) +  
					scale_fill_viridis(option="C") + 
					labs(x = "Year", y = "Altitude shell", fill = "Satellites", title = "Satellites per effective operator") +theme_bw())


(orbit_HHI_cat <- ggplot(data = soy_panel_HHI) + 
					geom_tile(aes(x=Year, y=shell_index_2, z=HHI_category, fill=HHI_category)) + 
					scale_fill_viridis(discrete=TRUE, option="C") + 
					labs(x = "Year", y = "Altitude shell", fill = "Degree of\nownership concentration") +
					theme_bw())

(HHI_sats <- ggplot(data = soy_panel_HHI, aes(x=total_sats)) +
					geom_point(aes(y=HHI), size = 1.1) + 
					theme_bw() + labs(x="Satellites",y="HHI",title="Orbital concentrations and satellite counts", color="Year"))


HHI_plot_panel <- (orbit_HHI_cat | orbit_HHI_2) / ( orbit_invHHI | orbit_invHHI_sats) + plot_annotation(
  title = 'How diverse is satellite ownership?',
  caption = 'Data from the Union of Concerned Scientists and Space-Track.org.'
)

HHI_plot_panel

soy_panel_big_2019 <- soy_panel_big %>% filter(Year==2019)

sat_inclination_period_HHI <- ggplot(data = soy_panel_big_2019) +
							geom_point(aes(x = INCLINATION, y = PERIOD, color = HHI), size = 0.75) + 
							scale_y_continuous(limits = c(85,130)) +
							scale_x_continuous(limits = c(0,130)) +
							theme_bw()

sat_inclination_period_HHI_polar <- ggplot(data = soy_panel_big_2019) +
							geom_point(aes(x = INCLINATION, y = PERIOD, color = HHI), size = 0.75) + 
							scale_y_continuous(limits = c(85,130)) +
							scale_x_continuous(limits = c(0,180)) +
						 	coord_polar(theta = "x", start = -90*pi/180, direction = -1) + theme_bw()

summary_plot1 <- (avg_fleet_size | orbit_pops) / (orbit_HHI | HHI_sats) / (sat_inclination_period_HHI | sat_inclination_period_HHI_polar) + plot_layout(guides = "collect", widths = c(1,1,2,1,2,1), heights = c(1,1,2))

png(file="../Output_figures/hhi_over_abstract_space.png", width = 1500, height = 900)
HHI_sats
dev.off()

png(file="../Output_figures/hhi_over_orbits.png", width = 1500, height = 900)
orbit_HHI
dev.off()

png(file="../Output_figures/hhi_over_orbits2.png", width = 1500, height = 900)
orbit_HHI_2
dev.off()

png(file="../Output_figures/hhi_plot_panel.png", width = 1500, height = 900)
HHI_plot_panel
dev.off()


png(file="../Output_figures/summary_over_time.png", width = 1500, height = 900)
summary_plot1
dev.off()

png(file="../Output_figures/summary_over_time.png", width = 900, height = 900)
sat_inclination_period_HHI_polar
dev.off()
