# --- Pre-processing for Final Report Code ---

knitr::opts_chunk$set(echo = T)
setwd("~/bu/year3/spring/ma575/labs/week12")

# Libraries

options(scipen=999)
# data manipulation
library(tidyverse)
library(readxl)     
library(readr)
library(dplyr)

# misc stats
library(DescTools)
library(Hmisc)
library(lm.beta)

# tables
library(tableone)
library(flextable)
library(officer)

# graphics
library(ggplot2)
library(ggthemes)
library(GGally)
library(gridExtra)

# Plot format
stdplot <- 
  theme_economist() +
  theme(axis.text=element_text(size=8),
        axis.title = element_text(size=10, color="grey20"),
        axis.line=element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size=10, color="grey20"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() ,
        legend.key = element_blank() ,
        legend.title =  element_text(size=6, color="grey30") ,
        legend.text = element_text(size=6, color="grey30") )


# Read data
bikes <- read.csv("day.csv", header=T)

# Rename variables
rename_bikes <- c("day_no", "date", "season", "year", "month", "holiday", "day_name", "workday", "weather", "temp", "adj_temp", "humid", "wind", "casual", "registered", "rentals")
names(bikes) <- rename_bikes

# Date Format
bikes$date <- as.Date(bikes$date)

# Additional variables; adjust existing variables

# create an index variable for day of year
bikes$day_yr <- ifelse(bikes$year ==0, bikes$day_no, bikes$day_no - 365)

# change temperature in degrees F
bikes$tempF <- 17.0224 + 85.22*bikes$temp
bikes$adj_tempF <- 17.0224 + 85.22*bikes$adj_temp

# rescale humidity and wind
bikes$humid <- bikes$humid*100
bikes$wind <- bikes$wind*100

# center numerical variables
bikes$adj_tempF <- (bikes$adj_tempF - mean(bikes$adj_tempF))
bikes$humid <- (bikes$humid - mean(bikes$humid))
bikes$wind <- (bikes$wind - mean(bikes$wind))

# Split data by user type and year

bikes_long <- bikes %>% pivot_longer(c("casual", "registered"), names_to="user_type", values_to = "user_rent")
bikes_11 <- bikes_long %>% filter(year==0)   
bikes_12 <- bikes_long %>% filter(year==1)   

# split data by user type for training 2011
train_reg <- bikes_11 %>% filter(user_type=="registered")
train_cas <- bikes_11 %>% filter(user_type=="casual")
train_cas_work   <- train_cas %>% filter(workday==1)   # N=250
train_cas_nowork <- train_cas %>% filter(workday==0)   # N=115

# save these 2012 data for validating after models are created with 2011 data
val_reg <- bikes_12 %>% filter(user_type=="registered")
val_cas <- bikes_12 %>% filter(user_type=="casual")

# split validation into work and non-work days for casual users
val_cas_work   <- val_cas %>% filter(workday==1)   # N=116
val_cas_nowork <- val_cas %>% filter(workday==0)   # N=250

