##################################################
##Title: EAVE II ethnicity project 
##
##Code author(s): Sarah Amele <sarah.amele@glasgow.ac.uk>
##
##Description: define covid death uses NRS death data and covid test data - based on code from Elliot
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
library(gmodels)
library(skimr)
library(ggplot2)
library(data.table)

##############################################################################
#load datasets
##############################################################################
setwd("")

load(file = "nrsdeath2.RData")
load(file = "covid_test1.RData")

##############################################################################
#clean data
##############################################################################
#nrs deaths
CrossTable(nrsdeath2$coviddeath_a ) 
CrossTable(nrsdeath2$coviddeath_p )  
CrossTable(nrsdeath2$coviddeath_s ) 
##############################################################################
#covid test
#sumarise data
CrossTable(covid_test1$test_result) 
CrossTable(covid_test1$test_type ) 

skim(covid_test1)

#only keep positive tests and PCR tests
covid_test_pos <- covid_test1 %>%
  filter(test_result == "POSITIVE") %>%
  filter(test_type == "PCR")
summary(as.factor(covid_test_pos$test_result)) 


#####################
#check for death before positive test
covid_test_death <- left_join(covid_test_pos, nrsdeath2,  by = "ID")   
skim(covid_test_death) #

covid_test_death$error_death <- NA
covid_test_death$error_death[!is.na(covid_test_death$date_of_death_n)] <- 0
covid_test_death$error_death[covid_test_death$date_of_death_n < covid_test_death$date_ecoss_specimen_n] <- 1

CrossTable(covid_test_death$error_death) #   

#drop tests after death and only keep relevant variables
covid_test_death1 <- covid_test_death %>%
  filter(is.na(error_death) | error_death == 0)  %>%
  select(ID, test_result, test_type, date_ecoss_specimen_n)
skim(covid_test_death1)


##############################################################################
#merge data
##############################################################################
#merge death data with covid test data
nrsdeath_test <- left_join(nrsdeath2, covid_test_death1,  by = "ID") 
skim(nrsdeath_test)          
#####################
#difference in days between positive test and death
nrsdeath_test$test_adm_diff <- as.numeric(difftime(nrsdeath_test$date_ecoss_specimen_n, nrsdeath_test$date_of_death_n, units = c("days")))

#covid positive within 28 days of death?
nrsdeath_test$categories <- cut(nrsdeath_test$test_adm_diff, breaks = c(-Inf, -29, 0, 28, Inf), labels = c(">28days before", "<28days before", "<28days after", ">28days after"))
CrossTable(nrsdeath_test$categories)

#covid death - based on pos pcr only
nrsdeath_event <- nrsdeath_test %>%
  mutate(coviddeath_inf = ifelse(test_adm_diff >= -28 & test_adm_diff <= 0, 1,0))  %>%
  mutate(coviddeath_infonly = ifelse(coviddeath_a == 0 & test_adm_diff >= -28 & test_adm_diff <= 0, 1,0))

CrossTable(nrsdeath_event$coviddeath_inf) 
CrossTable(nrsdeath_event$coviddeath_infonly) 
CrossTable(nrsdeath_event$coviddeath_a, nrsdeath_event$coviddeath_inf) 

#covid death - based on ICD code or pos pcr
nrsdeath_event <- nrsdeath_event %>%
  mutate(coviddeath_event = ifelse(coviddeath_a == 1 | coviddeath_inf == 1, 1,0))

CrossTable(nrsdeath_event$coviddeath_event)


nrsdeath_event <- nrsdeath_event %>%
  mutate(coviddeath_event_p = ifelse(coviddeath_p ==1, 1, 0))

#####################
#drop non covid deaths and sort by date
nrsdeath_event1 <- nrsdeath_event %>%
  filter(coviddeath_event == 1 ) %>%
  arrange(ID, desc(test_adm_diff))

CrossTable(nrsdeath_event1$coviddeath_a, nrsdeath_event1$coviddeath_inf)
CrossTable(nrsdeath_event1$coviddeath_infonly)


#####################
#keep one event per individual - deduplication
nrsdeath_event1$dup_id <- duplicated(nrsdeath_event1$ID)

CrossTable(nrsdeath_event1$dup_id)


#delete dups
class(nrsdeath_event1$dup_id)
nrsdeath_event2 <- nrsdeath_event1[nrsdeath_event1$dup_id == FALSE, ]

table(nrsdeath_event2$dup_id)
table(nrsdeath_event2$coviddeath_event) 
table(nrsdeath_event2$coviddeath_a) 

#drop variables
nrsdeath_event1$dup_id <- NULL
nrsdeath_event2$dup_id <- NULL 


#####################
#TEST - how many non ICD but infection 28 days before
nrsdeath_event_TEST <- nrsdeath_event %>%
  filter(coviddeath_infonly == 1 ) %>%
  arrange(ID, desc(test_adm_diff))

nrsdeath_event_TEST$dup_id <- duplicated(nrsdeath_event_TEST$ID)
table(nrsdeath_event_TEST$dup_id)


##############################################################################
#save dervived dataset
##############################################################################
setwd("")

save(nrsdeath_event, file = "nrsdeath_event.RData")
save(nrsdeath_event1, file = "nrsdeath_event1.RData")
save(nrsdeath_event2, file = "nrsdeath_event2.RData")

