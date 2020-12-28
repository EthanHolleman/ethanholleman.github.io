
# Resources
# =============================

# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html

# Ran into error installing "sf" package used solution here
# https://stackoverflow.com/questions/12141422/error-gdal-config-not-found-while-installing-r-dependent-packages-whereas-gdal

# Download data from nCoV2019 github repo, return dataframe

library(dplyr)
library(sf)
library(ggplot2)
library(rgeos)
library(tidyverse)
library(ggpubr)
library(ggmap)
library(rnaturalearth)
library(rnaturalearthdata)

read_data <- function(dest="latestdata.tar.gz"){
  untar.name <- "latestdata.csv"
  link <- "https://github.com/beoutbreakprepared/nCoV2019/raw/master/latest_data/latestdata.tar.gz"
  download.file(link, dest)
  message("Opening archive")
  untar(dest)
  message("Reading data from csv")
  data <- read.csv(untar.name)
  data %>% mutate(
     date_admission_hospital=convert_date(date_admission_hospital),
     date_onset_symptoms=convert_date(date_onset_symptoms),
     date_death_or_discharge=convert_date(date_death_or_discharge)
     )
}

script_theme <- function(){
  theme_minimal() + theme(text=element_text(size=15, face="bold"))
}

convert_date <- function(d){
  d1 <- as.character(d)
  as.Date(d1, format="%d.%m.%Y")
}

hospitalization_cords <- function(df){
  df_hospital <- df[complete.cases(df$date_admission_hospital), ]
  pre_sf <- as.data.frame(cbind(df_hospital$latitude, df_hospital$longitude))
  pre_sf <- pre_sf[complete.cases(pre_sf), ]
  col.names <- c('latitude', "longitude")
  colnames(pre_sf) <- col.names
  pre_sf
}

world_map <- function(){
  world <- ne_countries(scale = "medium", returnclass = "sf")
}

point_world_map <- function(df, height=1000, color="#f23535"){
  width <- height*1.25
  world <- world_map()
  cords <- hospitalization_cords(df)
  ggplot(data=world) + geom_sf() + 
    geom_point(data=cords, aes(x=longitude, y=latitude), 
               alpha=0.5, size=1, fill=color, color=color) +
    theme_bw() + labs(x="", y="", main="Global Hospitilizations")
}

hospital_addmissions <- function(df){
  ggplot(df[complete.cases(df$date_admission_hospital), ], 
         aes(x=date_admission_hospital)) +
    geom_density(stat = "count", fill="#f23535", alpha=0.5) +
    theme(legend.position = "None") +
    script_theme() + labs(x="", y="Individuals Admitted")
}

time_till_boxplots <- function(df){
  df.hospital <- df[complete.cases(df$date_admission_hospital), ]
  df.hospital.till <- cbind(
    df.hospital$date_onset_symptoms - df.hospital$date_admission_hospital,
    df.hospital$date_death_or_discharge - df.hospital$date_admission_hospital
  )
  colnames(df.hospital.till) <- c("admit_delay", "length_stay")
  df.hospital.till.complete <- df.hospital.till[complete.cases(df.hospital.till), ]
  g <- gather(as.data.frame(df.hospital.till.complete), "type", "time", admit_delay:length_stay, factor_key = TRUE)
  ggplot(g, aes(x=type, y=time), fill="#f23535", color="#f23535") + 
    geom_boxplot(fill="#f23535", alpha=0.5) + labs(x='', y="Days") + script_theme() +
    scale_x_discrete(labels=c("Symptoms to Admission Delay", "Admission Length"))
  
}

age_ranges <- function(){
  ages <- c()
  lower <- seq(0, 120, 10)
  upper <- seq(9, 129, 10)
}

# Downloads data and writes plots to "all_plots.jpg" in the
# current working directory
make_main_plot <- function(){
  df <- read_data()
  wm <- point_world_map(df)
  ha <- hospital_addmissions(df)
  tt <- time_till_boxplots(df)
  p <- grid.arrange(wm,                             # First row with one plot spaning over 2 columns
               arrangeGrob(ha, tt, ncol = 2), # Second row with 2 plots in 2 different columns
               nrow = 2)
  p <- as_ggplot(p) +                                # transform to a ggplot
    draw_plot_label(label = c("A", "B", "C"), size = 25,
                    x = c(0, 0, 0.5), y = c(1, 0.5, 0.5)) # Add labels
  jpeg("all_plots.jpg", width = 1400, height = 1000)
  p
  dev.off()
  return(p)
}