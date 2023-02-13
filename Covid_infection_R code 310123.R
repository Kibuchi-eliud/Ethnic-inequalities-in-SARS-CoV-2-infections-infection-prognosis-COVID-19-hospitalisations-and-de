##################################################
##Title: EAVE II ethnicity project
##Code author(s): Sarah Amele <sarah.amele@glasgow.ac.uk
##Description: define covid infection
##################################################
rm(list=ls())
#Libraries
#install.packages('dplyr')
#install.packages("tidyverse")
#install.packages('expss')
#install.packages('zoo')
#install.packages('BiocManager')
#install.packages('pacman')

library(dplyr)
library(tidyverse)
library(expss)
library(zoo)
library(skimr)
library(ggplot2)
library(data.table)

##############################################################################
#load datasets
##############################################################################
setwd("")

load(file = "covid_test1.RData")
load(file = "nrsdeath2.RData")

##############################################################################
#clean data
##############################################################################
#covid test
#sumarise data
skim(covid_test1) 
  
table(covid_test1$test_result )
table(covid_test1$test_type ) 

#####################
#only keep positive tests and PCR tests
covid_test_pos <- covid_test1 %>%
  filter(test_result == "POSITIVE") %>%
  filter(test_type == "PCR")

table(covid_test_pos$test_result ) #
table(covid_test_pos$test_type ) 

skim(covid_test_pos) 

##############################################################################
#check for death before positive test
covid_test_death <- left_join(covid_test_pos, nrsdeath2,  by = "ID") 
skim(covid_test_death)              

covid_test_death$error_death <- NA
covid_test_death$error_death[!is.na(covid_test_death$date_of_death_n)] <- 0
covid_test_death$error_death[covid_test_death$date_of_death_n < covid_test_death$date_ecoss_specimen_n] <- 1

table(covid_test_death$error_death) 


#difference between dates
error_death_ctest <- covid_test_death %>%
  filter(error_death == 1) %>%
  mutate(error_death_diff = date_ecoss_specimen_n - date_of_death_n) %>%
  select(ID, date_of_death_n,  date_ecoss_specimen_n, error_death_diff)

summary(error_death_ctest)
skim(error_death_ctest)

#drop tests after death and only keep relavent variables
covid_test_death1 <- covid_test_death %>%
  filter(is.na(error_death) | error_death == 0)  %>%
  select(ID, test_result, test_type, date_ecoss_specimen_n, wave_test)

skim(covid_test_death1)             
##############################################################################
#sort by date
covidtest_event <- covid_test_death1 %>%
  arrange(ID, date_ecoss_specimen_n) %>%
  mutate(covidtest_event = 1)

table(covidtest_event$covidtest_event)


#####################
#keep one event per individual - reduplication
covidtest_event$dup_id <- duplicated(covidtest_event$ID)

table(covidtest_event$dup_id)

#delete dups
class(covidtest_event$dup_id)
covidtest_event1 <- covidtest_event[covidtest_event$dup_id == FALSE, ]

table(covidtest_event1$dup_id)
table(covidtest_event1$covidtest_event)

#drop variables
covidtest_event$dup_id <- NULL 
covidtest_event1$dup_id <- NULL 

##############################################################################
#save derived dataset
##############################################################################
setwd("")

save(covidtest_event, file = "covidtest_event.RData")
save(covidtest_event1, file = "covidtest_event1.RData")
