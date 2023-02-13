##################################################
##Title: EAVE II ethnicity project 
##Code author(s): Sarah Amele <sarah.amele@glasgow.ac.uk>
##Description: define covid hosp uses SMR01 data and covid test data - based on code from Elliot
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
library(gmodels)
library(zoo)
pacman::p_load(skimr,ggplot2,data.table)

##############################################################################
#load datasets
##############################################################################
setwd("")

load(file = "smr01_2.RData")
load(file = "covid_test1.RData")
load(file = "nrsdeath2.RData")

#cohort end date - most recent monthly data drop
#a_end <- as.Date("2022-04-17")

#cohort start date - start of pandemic
a_begin <- as.Date("2020-03-01")

##############################################################################
#clean data
##############################################################################
#SMR01 - 
skim(smr01_2) 
#arrange episodes into chronological order for each stay
smr01_order <- smr01_2 %>%
  arrange(ID, admission_date_n, discharge_date_n)
#aggregate to stay level, use first admission type/main diagnosis
######################
#covid events
table(smr01_order$covidhosp_a) 
table(smr01_order$covidhosp_p)  
table(smr01_order$covidhosp_s) 


##############################################################################
#check for death before hospital admission
#merge SMR01 with death data
smr01_death <- left_join(smr01_order, nrsdeath2,  by = "ID")   
skim(smr01_death) 

smr01_death$error_death <- NA
smr01_death$error_death[!is.na(smr01_death$date_of_death_n)] <- 0
smr01_death$error_death[smr01_death$date_of_death_n < smr01_death$admission_date_n] <-  1

table(smr01_death$error_death) 

#difference between error dates
error_death_smr <- smr01_death %>%
  filter(error_death == 1) %>%
  mutate(error_death_diff = admission_date_n - date_of_death_n) %>%
  select(ID, date_of_death_n,  admission_date_n, error_death_diff)

summary(error_death_smr)
skim(error_death_smr)

#drop admissions after death
smr01_death1 <- smr01_death %>%
  filter(is.na(error_death)| error_death == 0) %>%
  select(ID:other_operation_3, wave_hosp)


skim(smr01_death1)

##############################################################################
#covid test
#sumarise data
names(covid_test1)
levels(as.factor(covid_test1$test_result)); summary(as.factor(covid_test1$test_result))
summary(as.factor(covid_test1$test_type ))
skim(covid_test1)

#only keep positive tests and PCR tests
covid_test_pos <- covid_test1 %>%
  filter(test_result == "POSITIVE") %>%
  filter(test_type == "PCR")
skim(covid_test_pos)


#####################
#check for death before positive test
covid_test_death <- left_join(covid_test_pos, nrsdeath2,  by = "ID") 
skim(covid_test_death) 

covid_test_death$error_death <- NA
covid_test_death$error_death[!is.na(covid_test_death$date_of_death_n)] <- 0
covid_test_death$error_death[covid_test_death$date_of_death_n < covid_test_death$date_ecoss_specimen_n] <- 1

table(covid_test_death$error_death)


#drop tests after death and only keep relavent variables
covid_test_death1 <- covid_test_death %>%
  filter(is.na(error_death) | error_death == 0)  %>%
  select(ID, test_result, test_type, date_ecoss_specimen_n)
skim(covid_test_death1)
 

##############################################################################
#merge data
##############################################################################
#merge smr01 data with covid test data
smr01_test <- left_join(smr01_death1, covid_test_death1,  by = "ID") 
skim(smr01_test)

#####################
#covid test while in hospital?
smr01_test <- smr01_test %>%
  mutate(test_in_stay = ifelse(date_ecoss_specimen_n > admission_date_n & date_ecoss_specimen_n <= discharge_date_n, 1,0))
table(smr01_test$test_in_stay) 
#####################
#difference in days between positive test and admission
smr01_test$test_adm_diff <- as.numeric(difftime(smr01_test$date_ecoss_specimen_n, smr01_test$admission_date_n, units = c("days")))

#covid positive within 28 days of admission?
smr01_test$categories <- cut(smr01_test$test_adm_diff, breaks = c(-Inf, -29, 0, 28, Inf), labels = c(">28days before", "<28days before", "<28days after", ">28days after"))
table(smr01_test$categories)
                  

#covid admissions - based on pos pcr
smr01_event <- smr01_test %>%
  mutate(covidhosp_inf = ifelse(test_adm_diff >= -28 & test_adm_diff <= 0, 1,0)) %>%
  mutate(covidhosp_infonly = ifelse(covidhosp_a == 0 & test_adm_diff >= -28 & test_adm_diff <= 0, 1,0))

table(smr01_event$covidhosp_a)  
table(smr01_event$covidhosp_inf) 
table(smr01_event$covidhosp_infonly) 
table(smr01_event$covidhosp_a, smr01_event$covidhosp_inf) 

#covid admissions - based on ICD code or pos pcr
smr01_event <- smr01_event %>%
  mutate(covidhosp_event = ifelse(covidhosp_a == 1 | covidhosp_inf == 1, 1,0))

table(smr01_event$covidhosp_event)

smr01_event <- smr01_event %>%
  mutate(covidhosp_event_p = ifelse(covidhosp_p == 1, 1, 0))

#drop non covid hospitalisations and sort by date
smr01_event1 <- smr01_event %>%
  filter(covidhosp_event == 1 ) %>%
  arrange(ID, admission_date_n, desc(covidhosp_a), desc(test_adm_diff))

table(smr01_event1$covidhosp_a)  
table(smr01_event1$covidhosp_infonly) 
table(smr01_event1$covidhosp_event) 
table(smr01_event1$covidhosp_a, smr01_event1$covidhosp_inf) 


#####################
#keep one event per individual - deduplication
smr01_event1$dup_id <- duplicated(smr01_event1$ID)

table(smr01_event1$dup_id) 

#delete dups
class(smr01_event1$dup_id)
smr01_event2 <- smr01_event1[smr01_event1$dup_id == FALSE, ]

table(smr01_event2$dup_id) 
table(smr01_event2$covidhosp_event) 
table(smr01_event2$covidhosp_a)  

#drop variables
smr01_event1$dup_id <- NULL 
smr01_event2$dup_id <- NULL 


#
#TEST - how many non ICD but infection 28 days before
smr01_event_TEST <- smr01_event %>%
  filter(covidhosp_infonly == 1 ) %>%
  arrange(ID, desc(test_adm_diff))
skim(smr01_event_TEST)

smr01_event_TEST$dup_id <- duplicated(smr01_event_TEST$ID)
table(smr01_event_TEST$dup_id) #


##############################################################################
#save derived dataset
##############################################################################
setwd("")
save(smr01_event, file = "smr01_event.RData")
save(smr01_event1, file = "smr01_event1.RData")
save(smr01_event2, file = "smr01_event2.RData")
