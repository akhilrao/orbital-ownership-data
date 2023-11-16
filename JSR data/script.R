##### R script to calculate the number of unique entities launching to orbit each year. In the end, as a bonus, calculate the number of unique operators in each 50 km shell in 2023.

library(tidyverse)
library(lubridate)
library(patchwork)

launches <- read_tsv("launchlog.tsv")
satcat <- read_tsv("currentcat.tsv")

# The launch users calculation

launches_small <- launches %>%
    mutate(year = as.numeric(substr(Launch_Date, start=1, stop=4))) %>%
    group_by(year,SatOwner) %>%
    mutate(number_of_sats = n()) %>% #select(year, SatOwner, number_of_sats) %>% print(n=Inf)
    ungroup() %>% group_by(year) %>%
    summarise(
        unique_launchers = n_distinct(SatOwner),
        median_sats = median(unique(number_of_sats))
        )

launch_entities <- ggplot(launches_small, aes(x=year, y=unique_launchers)) +
    geom_line() +
    labs(title = "Unique Entities Launching per Year",
         x = "Year",
         y = "Number of Unique Entities") +
    theme_bw()

number_of_sats <- ggplot(launches_small, aes(x=year, y=median_sats)) +
    geom_line() +
    labs(title = "Median Number of Satellites Launched per Entity per Year",
         x = "Year",
         y = "Number of Satellites") +
    theme_bw()

launch_entities_panel <- (launch_entities | number_of_sats) + plot_annotation(tag_levels = "A")

ggsave("launch_entities.png", launch_entities_panel, width = 14, height = 7, dpi = 300)

# The number of operators by orbital shell calculation. Restrict to observations in 2023.

satcat_small <- satcat %>%
    mutate(year = as.numeric(substr(ODate, start=1, stop=4))) %>%
    filter(year == 2023) %>%
    filter(Apogee!="-", Perigee!="-") %>%
    mutate(
        apogee = as.numeric(Apogee),
        perigee = as.numeric(Perigee),
        mean_altitude = (apogee + perigee)/2
        ) %>%
    filter(mean_altitude <= 2000) %>%
    mutate(altitude_bin = cut(mean_altitude, breaks = seq(0, 2000, by = 50))) %>%
    group_by(altitude_bin, Owner) %>%
    mutate(
        number_of_sats = n()
    ) %>%
    ungroup() %>% group_by(altitude_bin) %>%
    mutate(
        unique_operators = n_distinct(Owner)
        ) %>%
    select(altitude_bin, mean_altitude, Owner, unique_operators, number_of_sats) %>%
    distinct() %>% ungroup() %>% group_by(altitude_bin) %>%
    summarise(
        mean_altitude = mean(mean_altitude),
        unique_operators = mean(unique_operators),
        max_sats = max(number_of_sats),
        mean_sats = mean(number_of_sats),
        median_sats = median(number_of_sats)
        ) %>% ungroup()


# Plot density of operators, max sats, mean sats, and median sats by altitude bin.

# Converting bins from scientific to decimal notation bins.
satcat_small$altitude_bin <- as.character(satcat_small$altitude_bin)
satcat_small$altitude_bin <- gsub("1e+03", "1000", satcat_small$altitude_bin, fixed = TRUE)
satcat_small$altitude_bin <- gsub("1.05e+03", "1050", satcat_small$altitude_bin, fixed = TRUE)
satcat_small$altitude_bin <- gsub("1.1e+03", "1100", satcat_small$altitude_bin, fixed = TRUE)
satcat_small$altitude_bin <- gsub("1.15e+03", "1150", satcat_small$altitude_bin, fixed = TRUE)
satcat_small$altitude_bin <- gsub("1.2e+03", "1200", satcat_small$altitude_bin, fixed = TRUE)
satcat_small$altitude_bin <- gsub("1.25e+03", "1250", satcat_small$altitude_bin, fixed = TRUE)
satcat_small$altitude_bin <- gsub("1.3e+03", "1300", satcat_small$altitude_bin, fixed = TRUE)
satcat_small$altitude_bin <- gsub("1.35e+03", "1350", satcat_small$altitude_bin, fixed = TRUE)
satcat_small$altitude_bin <- gsub("1.4e+03", "1400", satcat_small$altitude_bin, fixed = TRUE)
satcat_small$altitude_bin <- gsub("1.45e+03", "1450", satcat_small$altitude_bin, fixed = TRUE)
satcat_small$altitude_bin <- gsub("1.5e+03", "1500", satcat_small$altitude_bin, fixed = TRUE)
satcat_small$altitude_bin <- gsub("1.55e+03", "1550", satcat_small$altitude_bin, fixed = TRUE)
satcat_small$altitude_bin <- gsub("1.6e+03", "1600", satcat_small$altitude_bin, fixed = TRUE)
satcat_small$altitude_bin <- gsub("1.65e+03", "1650", satcat_small$altitude_bin, fixed = TRUE)
satcat_small$altitude_bin <- gsub("1.7e+03", "1700", satcat_small$altitude_bin, fixed = TRUE)
satcat_small$altitude_bin <- gsub("1.75e+03", "1750", satcat_small$altitude_bin, fixed = TRUE)
satcat_small$altitude_bin <- gsub("1.8e+03", "1800", satcat_small$altitude_bin, fixed = TRUE)
satcat_small$altitude_bin <- gsub("1.85e+03", "1850", satcat_small$altitude_bin, fixed = TRUE)
satcat_small$altitude_bin <- gsub("1.9e+03", "1900", satcat_small$altitude_bin, fixed = TRUE)
satcat_small$altitude_bin <- gsub("1.95e+03", "1950", satcat_small$altitude_bin, fixed = TRUE)
satcat_small$altitude_bin <- gsub("2e+03", "2000", satcat_small$altitude_bin, fixed = TRUE)
satcat_small$altitude_bin <- gsub("1.1000", "1100", satcat_small$altitude_bin, fixed = TRUE)

satcat_small$altitude_bin <- factor(satcat_small$altitude_bin, levels = unique(satcat_small$altitude_bin), ordered = TRUE)

# Plots
operator_density_plot <- ggplot(satcat_small, aes(x=altitude_bin, y=unique_operators)) +
    geom_point() +
    labs(title = "Unique Operators by Altitude Bin",
         x = "Altitude Bin (km)",
         y = "Count") +
    theme_bw() +
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size=20)
        )

max_sats_plot <- ggplot(satcat_small, aes(x=altitude_bin, y=max_sats)) +
    geom_point() +
    labs(title = "Maximum Number of Satellites by Altitude Bin",
         x = "Altitude Bin (km)",
         y = "Count") +
    theme_bw() +
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size=20)
        )

mean_sats_plot <- ggplot(satcat_small, aes(x=altitude_bin, y=mean_sats)) +
    geom_point() +
    labs(title = "Mean Number of Satellites by Altitude Bin",
         x = "Altitude Bin (km)",
         y = "Count") +
    theme_bw() +
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size=20)
        )

median_sats_plot <- ggplot(satcat_small, aes(x=altitude_bin, y=median_sats)) +
    geom_point() +
    labs(title = "Median Number of Satellites by Altitude Bin",
         x = "Altitude Bin (km)",
         y = "Count") +
    theme_bw() +
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size=20)
        )

satcat_panel <- (operator_density_plot | max_sats_plot) / (mean_sats_plot | median_sats_plot) + plot_annotation(tag_levels = "A")

ggsave("satcat_panel.png", satcat_panel, width = 28, height = 14, dpi = 300)

ggsave("operator_density_plot.png", operator_density_plot, width = 14, height = 7, dpi = 300)
