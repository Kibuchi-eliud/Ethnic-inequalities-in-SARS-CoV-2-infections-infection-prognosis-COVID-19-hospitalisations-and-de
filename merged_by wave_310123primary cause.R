##################################################
##Title: EAVE II ethnicity project 
##
##Code author(s): Sarah Amele <sarah.amele@glasgow.ac.uk>
##
##Description: merge covid outcome data together with denominator - BY WAVE (Primary)
##################################################
#rm(list-ls())
#Libraries
#install.packages('dplyr')
#install.packages('expss')
#install.packages("tidyverse")
#install.packages('zoo')
#install.packages('BiocManager')
#install.packages('pacman')

library(dplyr)
library(expss)
library(tidyverse)
library(zoo)
library(skimr)
library(ggplot2)
library(data.table)
library(gmodels) 

##############################################################################
#load datasets
##############################################################################
setwd("")

load(file = "denominator.RData")
load(file = "nrsdeath2.RData")
load(file = "nrsdeath_event2.RData")
load(file = "covidtest_event.RData")
load(file = "smr01_event1.RData")

##########################
#cohort start date - start of pandemic
a_begin <- as.Date("2020-03-01")

#cohort end date - most recent monthly data drop
a_end <- as.Date("2022-04-17")

##########################
#wave1
w1_start <- as.Date("2020-03-01")

w1_end <- as.Date("2020-07-31")

#wave2
w2_start <- as.Date("2020-08-01")

w2_end <- as.Date("2021-04-30")

#wave3
w3_start <- as.Date("2021-05-01")

w3_end <- as.Date("2021-12-17")

#wave4
w4_start <- as.Date("2021-12-18")
w4_end <- as.Date("2022-04-17")

##############################################################################
#set up denominator dataset
##############################################################################

#keep relevant variables from denominator 
denominator1 <- denominator %>%
  select(ID, ethnicid_s, ethnicid_c, census, date, source, ethnic_recode, ethnic_grp_recode, phs_elv, ethnic_cen_phs, ethnicgrp_cen_phs, cen_phs, 
         sex, age1, age_c, age_5y, hb2019, simd2020_sc_decile) 


table(denominator1$census)
table(denominator1$phs_elv) 
table(denominator1$cen_phs) 
table(denominator1$ethnic_cen_phs)

table(denominator1$age_c);summary(as.factor(denominator1$age_c))
table(denominator1$age_5y) 
table(denominator1$sex);summary(as.factor(denominator1$sex))
table(denominator1$hb2019);summary(as.factor(denominator1$hb2019))
table(denominator1$simd2020_sc_decile) 

#delete individuals without census (ethnicity) data
denominator2 <- denominator1 %>%
  filter(cen_phs == 1)  

#delete individuals without age data
denominator3 <- subset(denominator2, !is.na(age1)) 
#delete individuals without HB data
denominator4 <- subset(denominator3, !is.na(hb2019)) 
#====================================================================================================
#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
denominator4$ethnicgrp_cen_phs <- recode_factor(denominator4$ethnicgrp_cen_phs, White = "White", 'Mixed or multiple ethnic groups'= "Mixed or multiple ethnic groups", Asian ="Asian", 'African, Caribbean or Black'="African Caribbean or Black", 'Other Ethnic Group'="Other Ethnic Group")
levels(denominator4$ethnicgrp_cen_phs); summary(denominator4$ethnicgrp_cen_phs)

#sex
denominator4$sex <- as.factor(denominator4$sex); summary(denominator4$sex);levels(denominator4$sex)
denominator4$sex <- recode_factor(denominator4$sex, '1'="male",'2'="female")

#hb
denominator4$hb2019 <-as.factor(denominator4$hb2019);levels(denominator4$hb2019); summary(denominator4$hb2019)
denominator4$hb2019 <- recode_factor(denominator4$hb2019, "S08000015" ="Ayrshire and Arran",
                                     "S08000016" ="Borders", 
                                     "S08000017" = "Dumfries and Galloway",
                                     "S08000019" = "Forth Valley",
                                     "S08000020" = "Grampian",                                
                                     "S08000022" = "Highland",
                                     "S08000024" = "Lothian",
                                     "S08000025" = "Orkney",
                                     "S08000026" = "Shetland",
                                     "S08000028" ="Western Isles",
                                     "S08000029" = "Fife",                               
                                     "S08000030" = "Tayside",
                                     "S08000031" = "Greater Glasgow and Clyde",
                                     "S08000032" = "Lanarkshire"); summary(denominator4$hb2019)
#Age 
summary(denominator4$age_5y)

##############################################################################
#denominator for each wave
##############################################################################
#create death variable
names(nrsdeath2)
nrsdeath2$death = 1
#merge denominator
deno_nrs <- left_join(denominator4, nrsdeath2, by = "ID")
#re-assign NA as 0
deno_nrs$death[is.na(deno_nrs$death)] <- 0  

##############################################################################
#wave 1 - includes everyone
deno_w1 <- deno_nrs  %>%
  mutate(wave1_ed = if_else(death == 1 & date_of_death_n <= w1_end, date_of_death_n,
                    if_else(death == 1 & date_of_death_n > w1_end, w1_end,
                    if_else(death == 0, w1_end, as.Date("2010-10-10"))))) %>%
  mutate(wave1_fu = as.numeric(wave1_ed - w1_start))

#wave 2
deno_w2 <- deno_nrs %>%
  filter(wave_death %in% c(NA, "Wave 2", "Wave 3", "Wave 4")) %>%
  mutate(wave2_ed = if_else(death == 1 & date_of_death_n <= w2_end, date_of_death_n,
                    if_else(death == 1 & date_of_death_n > w2_end, w2_end,
                    if_else(death == 0, w2_end, as.Date("2010-10-10"))))) %>%
  mutate(wave2_fu = as.numeric(wave2_ed - w2_start))
#
#wave 3
deno_w3 <- deno_nrs %>%
  filter(wave_death %in% c(NA, "Wave 3", "Wave 4")) %>%
  mutate(wave3_ed = if_else(death == 1 & date_of_death_n <= w3_end, date_of_death_n,
                    if_else(death == 1 & date_of_death_n > w3_end, w3_end,
                    if_else(death == 0, w3_end, as.Date("2010-10-10"))))) %>%
  mutate(wave3_fu = as.numeric(wave3_ed - w3_start))

#wave 4
deno_w4 <- deno_nrs %>%
  filter(wave_death %in% c(NA, "Wave 4") )%>%
  mutate(wave4_ed = if_else(death == 1 & date_of_death_n <= w4_end, date_of_death_n,
                    if_else(death == 1 & date_of_death_n > w4_end, w4_end,
                    if_else(death == 0, w4_end, as.Date("2010-10-10"))))) %>%
  mutate(wave4_fu = as.numeric(wave4_ed - w4_start))


##############################################################################
#set up outcome datasets
##############################################################################
table(covidtest_event$covidtest_event)  
table(smr01_event1$covidhosp_event)   

##########################
#only keep relevant variables - prognosis
smr01_wave <- smr01_event1 %>%
  select(ID:covidhosp_a, wave_hosp, covidhosp_inf:covidhosp_event_p)

nrsdeath_event3 <- nrsdeath_event2 %>%
  select(ID:coviddeath_a, coviddeath_inf:coviddeath_event_p) %>%
  mutate(death = 1)

##############################################################################
#split infection data by wave
##############################################################################
table(covidtest_event$covidtest_event)
table(covidtest_event$wave_test)  

#wave 1
covidtest_event_w1 <- covidtest_event %>%
  filter(wave_test == "Wave 1") %>%
  mutate(covidtest_fu_w1 = as.numeric(date_ecoss_specimen_n - w1_start))

  
#wave 2
covidtest_event_w2 <- covidtest_event %>%
  filter(wave_test == "Wave 2") %>%
  mutate(covidtest_fu_w2 = as.numeric(date_ecoss_specimen_n - w2_start))

#wave 3
covidtest_event_w3 <- covidtest_event %>%
  filter(wave_test == "Wave 3") %>%
  mutate(covidtest_fu_w3 = as.numeric(date_ecoss_specimen_n - w3_start))

#wave 4
covidtest_event_w4 <- covidtest_event %>%
  filter(wave_test == "Wave 4") %>%
  mutate(covidtest_fu_w4 = as.numeric(date_ecoss_specimen_n - w4_start))

##############################################################################
#split hospitalization data by wave
##############################################################################
table(smr01_wave$covidhosp_event) 
table(smr01_wave$wave_hosp)  

#wave 1
covidhosp_event_w1 <- smr01_wave %>%
  filter(wave_hosp == "Wave 1") %>%
  mutate(covidhosp_fu_w1 = as.numeric(admission_date_n - w1_start))

#wave 2
covidhosp_event_w2 <- smr01_wave %>%
  filter(wave_hosp == "Wave 2") %>%
  mutate(covidhosp_fu_w2 = as.numeric(admission_date_n - w2_start))

#wave 3
covidhosp_event_w3 <- smr01_wave %>%
  filter(wave_hosp == "Wave 3") %>%
  mutate(covidhosp_fu_w3 = as.numeric(admission_date_n - w3_start))

#wave 4
covidhosp_event_w4 <- smr01_wave %>%
  filter(wave_hosp == "Wave 4") %>%
  mutate(covidhosp_fu_w4 = as.numeric(admission_date_n - w4_start))

##############################################################################
#split death data by wave
##############################################################################
table(nrsdeath_event3$coviddeath_event) #
table(nrsdeath_event3$wave_death)  

#wave 1
coviddeath_event_w1 <- nrsdeath_event3 %>%
  filter(wave_death == "Wave 1") %>%
  mutate(coviddeath_fu_w1 = as.numeric(date_of_death_n - w1_start)) %>%
  arrange(ID, coviddeath_fu_w1)

#wave 2
coviddeath_event_w2 <- nrsdeath_event3 %>%
  filter(wave_death == "Wave 2") %>%
  mutate(coviddeath_fu_w2 = as.numeric(date_of_death_n - w2_start)) %>%
  arrange(ID, coviddeath_fu_w2)

#wave 3
coviddeath_event_w3 <- nrsdeath_event3 %>%
  filter(wave_death == "Wave 3") %>%
  mutate(coviddeath_fu_w3 = as.numeric(date_of_death_n - w3_start)) %>%
  arrange(ID, coviddeath_fu_w3)

#wave 4
coviddeath_event_w4 <- nrsdeath_event3 %>%
  filter(wave_death == "Wave 4") %>%
  mutate(coviddeath_fu_w4 = as.numeric(date_of_death_n - w4_start)) %>%
  arrange(ID, coviddeath_fu_w4)

##############################################################################
#merge hosp and death data together - by WAVE
##############################################################################
#wave 1
wave1_hosdea <- full_join(covidhosp_event_w1, coviddeath_event_w1, by = "ID") 
skim(wave1_hosdea)  

#wave 2
wave2_hosdea <- full_join(covidhosp_event_w2, coviddeath_event_w2, by = "ID") 
skim(wave2_hosdea) 
#wave 3
wave3_hosdea <- full_join(covidhosp_event_w3, coviddeath_event_w3, by = "ID") 
skim(wave3_hosdea) 

#wave 4
wave4_hosdea <- full_join(covidhosp_event_w4, coviddeath_event_w4, by = "ID") 
skim(wave4_hosdea)

##############################################################################
#re-assign NA as 0
wave1_hosdea$covidhosp_event[is.na(wave1_hosdea$covidhosp_event)] <- 0
wave1_hosdea$coviddeath_event[is.na(wave1_hosdea$coviddeath_event)] <- 0
wave1_hosdea$covidhosp_event_p[is.na(wave1_hosdea$covidhosp_event_p)] <- 0
wave1_hosdea$coviddeath_event_p[is.na(wave1_hosdea$coviddeath_event_p)] <- 0

wave2_hosdea$covidhosp_event[is.na(wave2_hosdea$covidhosp_event)] <- 0
wave2_hosdea$coviddeath_event[is.na(wave2_hosdea$coviddeath_event)] <- 0
wave2_hosdea$covidhosp_event_p[is.na(wave2_hosdea$covidhosp_event_p)] <- 0
wave2_hosdea$coviddeath_event_p[is.na(wave2_hosdea$coviddeath_event_p)] <- 0

wave3_hosdea$covidhosp_event[is.na(wave3_hosdea$covidhosp_event)] <- 0
wave3_hosdea$coviddeath_event[is.na(wave3_hosdea$coviddeath_event)] <- 0
wave3_hosdea$covidhosp_event_p[is.na(wave3_hosdea$covidhosp_event_p)] <- 0
wave3_hosdea$coviddeath_event_p[is.na(wave3_hosdea$coviddeath_event_p)] <- 0

wave4_hosdea$covidhosp_event[is.na(wave4_hosdea$covidhosp_event)] <- 0
wave4_hosdea$coviddeath_event[is.na(wave4_hosdea$coviddeath_event)] <- 0
wave4_hosdea$covidhosp_event_p[is.na(wave4_hosdea$covidhosp_event_p)] <- 0
wave4_hosdea$coviddeath_event_p[is.na(wave4_hosdea$coviddeath_event_p)] <- 0
##############################################################################
#create combined hosp/death outcome
##############################################################################
#wave1
covidhosdea_event_w1 <-  wave1_hosdea %>%
  mutate(covidhosdea_event =  1) %>%
  mutate(covidhosdea_event_p = if_else(covidhosp_event_p ==1 & coviddeath_event_p==1,1,0)) %>%
  #determine FU time
  mutate(covidhosdea_fu_w1 = if_else(covidhosp_event == 1 & coviddeath_event == 0, covidhosp_fu_w1,
                             if_else(covidhosp_event == 0 & coviddeath_event == 1, coviddeath_fu_w1,
                             if_else(covidhosp_event == 1 & coviddeath_event == 1 &  covidhosp_fu_w1 <= coviddeath_fu_w1, covidhosp_fu_w1, 7000)))) %>%
  arrange(ID, covidhosdea_fu_w1)

#wave2
covidhosdea_event_w2 <-  wave2_hosdea %>%
  mutate(covidhosdea_event =  1) %>%
  mutate(covidhosdea_event_p = if_else(covidhosp_event_p ==1 & coviddeath_event_p==1,1,0)) %>%
  #determine FU time
  mutate(covidhosdea_fu_w2 = if_else(covidhosp_event == 1 & coviddeath_event == 0, covidhosp_fu_w2,
                             if_else(covidhosp_event == 0 & coviddeath_event == 1, coviddeath_fu_w2,
                             if_else(covidhosp_event == 1 & coviddeath_event == 1 &  covidhosp_fu_w2 <= coviddeath_fu_w2, covidhosp_fu_w2, 7000)))) %>%
  arrange(ID, covidhosdea_fu_w2)

#wave3
covidhosdea_event_w3 <-  wave3_hosdea %>%
  mutate(covidhosdea_event =  1) %>%
  mutate(covidhosdea_event_p = if_else(covidhosp_event_p ==1 & coviddeath_event_p==1,1,0)) %>%
  #determine FU time
  mutate(covidhosdea_fu_w3 = if_else(covidhosp_event == 1 & coviddeath_event == 0, covidhosp_fu_w3,
                             if_else(covidhosp_event == 0 & coviddeath_event == 1, coviddeath_fu_w3,
                             if_else(covidhosp_event == 1 & coviddeath_event == 1 &  covidhosp_fu_w3 <= coviddeath_fu_w3, covidhosp_fu_w3, 7000)))) %>%
  arrange(ID, covidhosdea_fu_w3)

#wave4
covidhosdea_event_w4 <-  wave4_hosdea %>%
  mutate(covidhosdea_event =  1) %>%
  mutate(covidhosdea_event_p = if_else(covidhosp_event_p ==1 & coviddeath_event_p==1,1,0)) %>%
  #determine FU time
  mutate(covidhosdea_fu_w4 = if_else(covidhosp_event == 1 & coviddeath_event == 0, covidhosp_fu_w4,
                             if_else(covidhosp_event == 0 & coviddeath_event == 1, coviddeath_fu_w4,
                             if_else(covidhosp_event == 1 & coviddeath_event == 1 &  covidhosp_fu_w4 <= coviddeath_fu_w4, covidhosp_fu_w4, 7000)))) %>%
  arrange(ID, covidhosdea_fu_w4)

##########################
#check
table(covidhosdea_event_w1$covidhosdea_event)   #   
table(covidhosdea_event_w2$covidhosdea_event)   #   
table(covidhosdea_event_w3$covidhosdea_event)   #    
table(covidhosdea_event_w4$covidhosdea_event)   #   


##############################################################################
#keep one event per individual - deduplication - INFECTION
##############################################################################
covidtest_event_w1$dup_id <- duplicated(covidtest_event_w1$ID)
covidtest_event_w2$dup_id <- duplicated(covidtest_event_w2$ID)
covidtest_event_w3$dup_id <- duplicated(covidtest_event_w3$ID)
covidtest_event_w4$dup_id <- duplicated(covidtest_event_w4$ID)

##########################
#check
table(covidtest_event_w1$dup_id) #
table(covidtest_event_w2$dup_id) #
table(covidtest_event_w3$dup_id) #
table(covidtest_event_w4$dup_id) #
##########################
#delete dups
covidtest1_event_w1 <- covidtest_event_w1[covidtest_event_w1$dup_id == FALSE, ]
covidtest1_event_w2 <- covidtest_event_w2[covidtest_event_w2$dup_id == FALSE, ]
covidtest1_event_w3 <- covidtest_event_w3[covidtest_event_w3$dup_id == FALSE, ]
covidtest1_event_w4 <- covidtest_event_w4[covidtest_event_w4$dup_id == FALSE, ]

table(covidtest1_event_w1$covidtest_event) #
table(covidtest1_event_w2$covidtest_event) #
table(covidtest1_event_w3$covidtest_event) # 
table(covidtest1_event_w4$covidtest_event) #

#delete dup variable
covidtest1_event_w1$dup_id <- NULL 
covidtest1_event_w2$dup_id <- NULL 
covidtest1_event_w3$dup_id <- NULL 
covidtest1_event_w4$dup_id <- NULL 

##############################################################################
#keep one event per individual - deduplication - HOSP
##############################################################################
covidhosp_event_w1$dup_id <- duplicated(covidhosp_event_w1$ID)
covidhosp_event_w2$dup_id <- duplicated(covidhosp_event_w2$ID)
covidhosp_event_w3$dup_id <- duplicated(covidhosp_event_w3$ID)
covidhosp_event_w4$dup_id <- duplicated(covidhosp_event_w4$ID)

##########################
#check
table(covidhosp_event_w1$dup_id) #
table(covidhosp_event_w2$dup_id) #
table(covidhosp_event_w3$dup_id) #
table(covidhosp_event_w4$dup_id) # 

##########################
#delete dups
covidhosp1_event_w1 <- covidhosp_event_w1[covidhosp_event_w1$dup_id == FALSE, ]
covidhosp1_event_w2 <- covidhosp_event_w2[covidhosp_event_w2$dup_id == FALSE, ]
covidhosp1_event_w3 <- covidhosp_event_w3[covidhosp_event_w3$dup_id == FALSE, ]
covidhosp1_event_w4 <- covidhosp_event_w4[covidhosp_event_w4$dup_id == FALSE, ]

table(covidhosp1_event_w1$covidhosp_event) #
table(covidhosp1_event_w2$covidhosp_event) # 
table(covidhosp1_event_w3$covidhosp_event) #  
table(covidhosp1_event_w4$covidhosp_event) # 

#delete dup variable
covidhosp1_event_w1$dup_id <- NULL 
covidhosp1_event_w2$dup_id <- NULL 
covidhosp1_event_w3$dup_id <- NULL 
covidhosp1_event_w4$dup_id <- NULL 

##############################################################################
#keep one event per individual - deduplication - HOSP/DEATH
##############################################################################
covidhosdea_event_w1$dup_id <- duplicated(covidhosdea_event_w1$ID)
covidhosdea_event_w2$dup_id <- duplicated(covidhosdea_event_w2$ID)
covidhosdea_event_w3$dup_id <- duplicated(covidhosdea_event_w3$ID)
covidhosdea_event_w4$dup_id <- duplicated(covidhosdea_event_w4$ID)

##########################
#check
table(covidhosdea_event_w1$dup_id) #
table(covidhosdea_event_w2$dup_id) #
table(covidhosdea_event_w3$dup_id) #
table(covidhosdea_event_w4$dup_id) # 

##########################
#delete dups
covidhosdea1_event_w1 <- covidhosdea_event_w1[covidhosdea_event_w1$dup_id == FALSE, ]
covidhosdea1_event_w2 <- covidhosdea_event_w2[covidhosdea_event_w2$dup_id == FALSE, ]
covidhosdea1_event_w3 <- covidhosdea_event_w3[covidhosdea_event_w3$dup_id == FALSE, ]
covidhosdea1_event_w4 <- covidhosdea_event_w4[covidhosdea_event_w4$dup_id == FALSE, ]

table(covidhosdea1_event_w1$covidhosdea_event) #
table(covidhosdea1_event_w2$covidhosdea_event) # 
table(covidhosdea1_event_w3$covidhosdea_event) #    
table(covidhosdea1_event_w4$covidhosdea_event) # 

#delete dup variable
covidhosdea1_event_w1$dup_id <- NULL 
covidhosdea1_event_w2$dup_id <- NULL 
covidhosdea1_event_w3$dup_id <- NULL 
covidhosdea1_event_w4$dup_id <- NULL 

##############################################################################
#merge INFECTION data with denominator 
##############################################################################
#wave 1
deno_test_w1 <- left_join(deno_w1, covidtest1_event_w1,  by = "ID") 

#wave 2
deno_test_w2 <- left_join(deno_w2, covidtest1_event_w2,  by = "ID") 

#wave 3
deno_test_w3 <- left_join(deno_w3, covidtest1_event_w3,  by = "ID") 

#wave 4
deno_test_w4 <- left_join(deno_w4, covidtest1_event_w4,  by = "ID") 

#re-assign NA as 0
deno_test_w1$covidtest_event[is.na(deno_test_w1$covidtest_event)] <- 0
deno_test_w2$covidtest_event[is.na(deno_test_w2$covidtest_event)] <- 0
deno_test_w3$covidtest_event[is.na(deno_test_w3$covidtest_event)] <- 0
deno_test_w4$covidtest_event[is.na(deno_test_w4$covidtest_event)] <- 0

table(deno_test_w1$covidtest_event) 
table(deno_test_w2$covidtest_event) 
table(deno_test_w3$covidtest_event)       
table(deno_test_w4$covidtest_event) 

##############################################################################
#create FU variable
#wave 1
deno_test_w1 <- deno_test_w1 %>%
  mutate(covidtest_fu = if_else(covidtest_event == 1, covidtest_fu_w1, wave1_fu)) 

#wave 2
deno_test_w2 <- deno_test_w2 %>%
  mutate(covidtest_fu = if_else(covidtest_event == 1, covidtest_fu_w2, wave2_fu)) 
  
#wave 3
deno_test_w3 <- deno_test_w3 %>%
  mutate(covidtest_fu = if_else(covidtest_event == 1, covidtest_fu_w3, wave3_fu)) 
  
#wave 4
deno_test_w4 <- deno_test_w4 %>%
  mutate(covidtest_fu = if_else(covidtest_event == 1, covidtest_fu_w4, wave4_fu))  
  
##############################################################################
#merge HOSPITALISATION data with denominator 
##############################################################################
#wave 1
deno_hosp_w1 <- left_join(deno_w1, covidhosp1_event_w1,  by = "ID")
#skim(deno_hosp_w1) 

#wave 2
deno_hosp_w2 <- left_join(deno_w2, covidhosp1_event_w2,  by = "ID")
#skim(deno_hosp_w2) 

#wave 3
deno_hosp_w3 <- left_join(deno_w3, covidhosp1_event_w3,  by = "ID") 
skim(deno_hosp_w3) 

#wave 4
deno_hosp_w4 <- left_join(deno_w4, covidhosp1_event_w4,  by = "ID")
skim(deno_hosp_w4) 

#re-assign NA as 0
deno_hosp_w1$covidhosp_event[is.na(deno_hosp_w1$covidhosp_event)] <- 0  
deno_hosp_w2$covidhosp_event[is.na(deno_hosp_w2$covidhosp_event)] <- 0  
deno_hosp_w3$covidhosp_event[is.na(deno_hosp_w3$covidhosp_event)] <- 0
deno_hosp_w4$covidhosp_event[is.na(deno_hosp_w4$covidhosp_event)] <- 0 
deno_hosp_w1$covidhosp_event_p[is.na(deno_hosp_w1$covidhosp_event_p)] <- 0  
deno_hosp_w2$covidhosp_event_p[is.na(deno_hosp_w2$covidhosp_event_p)] <- 0  
deno_hosp_w3$covidhosp_event_p[is.na(deno_hosp_w3$covidhosp_event_p)] <- 0
deno_hosp_w4$covidhosp_event_p[is.na(deno_hosp_w4$covidhosp_event_p)] <- 0 


table(deno_hosp_w1$covidhosp_event)  
table(deno_hosp_w2$covidhosp_event)   
table(deno_hosp_w3$covidhosp_event)       
table(deno_hosp_w4$covidhosp_event) 
##############################################################################
#create FU variable

#wave 1
deno_hosp_w1 <- deno_hosp_w1 %>%
  mutate(covidhosp_fu = if_else(covidhosp_event == 1, covidhosp_fu_w1, wave1_fu)) %>%
  mutate(covidhosp_fu_p = if_else(covidhosp_event_p == 1, covidhosp_fu_w1, wave1_fu))
  

#wave 2
deno_hosp_w2 <- deno_hosp_w2 %>%
  mutate(covidhosp_fu = if_else(covidhosp_event == 1, covidhosp_fu_w2, wave2_fu)) %>%
  mutate(covidhosp_fu_p = if_else(covidhosp_event_p == 1, covidhosp_fu_w2, wave2_fu))

#wave 3
deno_hosp_w3 <- deno_hosp_w3 %>%
  mutate(covidhosp_fu = if_else(covidhosp_event == 1, covidhosp_fu_w3, wave3_fu)) %>%
  mutate(covidhosp_fu_p = if_else(covidhosp_event_p == 1, covidhosp_fu_w3, wave3_fu))

#wave 4
deno_hosp_w4 <- deno_hosp_w4 %>%
  mutate(covidhosp_fu = if_else(covidhosp_event == 1, covidhosp_fu_w4, wave4_fu)) %>%
  mutate(covidhosp_fu_p = if_else(covidhosp_event_p == 1, covidhosp_fu_w4, wave4_fu))


##############################################################################
#merge DEATH data with denominator 
##############################################################################
#wave 1
deno_death_w1 <- left_join(deno_w1, coviddeath_event_w1,  by = "ID") 

#wave 2
deno_death_w2 <- left_join(deno_w2, coviddeath_event_w2,  by = "ID")

#wave 3
deno_death_w3 <- left_join(deno_w3, coviddeath_event_w3,  by = "ID")
#wave 4
deno_death_w4 <- left_join(deno_w4, coviddeath_event_w4,  by = "ID") 

#re-assign NA as 0
deno_death_w1$coviddeath_event[is.na(deno_death_w1$coviddeath_event)] <- 0  
deno_death_w2$coviddeath_event[is.na(deno_death_w2$coviddeath_event)] <- 0  
deno_death_w3$coviddeath_event[is.na(deno_death_w3$coviddeath_event)] <- 0
deno_death_w4$coviddeath_event[is.na(deno_death_w4$coviddeath_event)] <- 0 
deno_death_w1$coviddeath_event_p[is.na(deno_death_w1$coviddeath_event_p)] <- 0  
deno_death_w2$coviddeath_event_p[is.na(deno_death_w2$coviddeath_event_p)] <- 0  
deno_death_w3$coviddeath_event_p[is.na(deno_death_w3$coviddeath_event_p)] <- 0
deno_death_w4$coviddeath_event_p[is.na(deno_death_w4$coviddeath_event_p)] <- 0 

table(deno_death_w1$coviddeath_event) 
table(deno_death_w2$coviddeath_event)       
table(deno_death_w3$coviddeath_event)      
table(deno_death_w4$coviddeath_event)   

##############################################################################
#create FU variable
#wave 1
deno_death_w1 <- deno_death_w1 %>%
  mutate(coviddeath_fu = if_else(coviddeath_event == 1, coviddeath_fu_w1, wave1_fu)) %>%
  mutate(coviddeath_fu_p = if_else(coviddeath_event_p == 1, coviddeath_fu_w1, wave1_fu)) 

#wave 2
deno_death_w2 <- deno_death_w2 %>%
  mutate(coviddeath_fu = if_else(coviddeath_event == 1, coviddeath_fu_w2, wave2_fu)) %>%
  mutate(coviddeath_fu_p = if_else(coviddeath_event_p == 1, coviddeath_fu_w2, wave2_fu)) 

#wave 3
deno_death_w3 <- deno_death_w3 %>%
  mutate(coviddeath_fu = if_else(coviddeath_event == 1, coviddeath_fu_w3, wave3_fu)) %>%
  mutate(coviddeath_fu_p = if_else(coviddeath_event_p == 1, coviddeath_fu_w3, wave3_fu))

#wave 4
deno_death_w4 <- deno_death_w4 %>%
  mutate(coviddeath_fu = if_else(coviddeath_event == 1, coviddeath_fu_w4, wave4_fu)) %>%
  mutate(coviddeath_fu_p = if_else(coviddeath_event_p == 1, coviddeath_fu_w4, wave4_fu))

##############################################################################
#merge HOSP/DEATH data with denominator 
##############################################################################
#wave 1
deno_hosdea_w1 <- left_join(deno_w1, covidhosdea1_event_w1,  by = "ID") 

#wave 2
deno_hosdea_w2 <- left_join(deno_w2, covidhosdea1_event_w2,  by = "ID") 

#wave 3
deno_hosdea_w3 <- left_join(deno_w3, covidhosdea1_event_w3,  by = "ID") 


#wave 4
deno_hosdea_w4 <- left_join(deno_w4, covidhosdea1_event_w4,  by = "ID")

#re-assign NA as 0
deno_hosdea_w1$covidhosdea_event[is.na(deno_hosdea_w1$covidhosdea_event)] <- 0  
deno_hosdea_w2$covidhosdea_event[is.na(deno_hosdea_w2$covidhosdea_event)] <- 0  
deno_hosdea_w3$covidhosdea_event[is.na(deno_hosdea_w3$covidhosdea_event)] <- 0
deno_hosdea_w4$covidhosdea_event[is.na(deno_hosdea_w4$covidhosdea_event)] <- 0 
deno_hosdea_w1$covidhosdea_event_p[is.na(deno_hosdea_w1$covidhosdea_event_p)] <- 0  
deno_hosdea_w2$covidhosdea_event_p[is.na(deno_hosdea_w2$covidhosdea_event_p)] <- 0  
deno_hosdea_w3$covidhosdea_event_p[is.na(deno_hosdea_w3$covidhosdea_event_p)] <- 0
deno_hosdea_w4$covidhosdea_event_p[is.na(deno_hosdea_w4$covidhosdea_event_p)] <- 0 


table(deno_hosdea_w1$covidhosdea_event)  
table(deno_hosdea_w2$covidhosdea_event)  
table(deno_hosdea_w3$covidhosdea_event)       
table(deno_hosdea_w4$covidhosdea_event)  

##############################################################################
#create FU variable

#wave 1
deno_hosdea_w1 <- deno_hosdea_w1 %>%
  mutate(covidhosdea_fu = if_else(covidhosdea_event == 1, covidhosdea_fu_w1, wave1_fu)) %>%
  mutate(covidhosdea_fu_p = if_else(covidhosdea_event_p == 1, covidhosdea_fu_w1, wave1_fu))

#wave 2
deno_hosdea_w2 <- deno_hosdea_w2 %>%
  mutate(covidhosdea_fu = if_else(covidhosdea_event == 1, covidhosdea_fu_w2, wave2_fu)) %>%
  mutate(covidhosdea_fu_p = if_else(covidhosdea_event_p == 1, covidhosdea_fu_w2, wave2_fu))

#wave 3
deno_hosdea_w3 <- deno_hosdea_w3 %>%
  mutate(covidhosdea_fu = if_else(covidhosdea_event == 1, covidhosdea_fu_w3, wave3_fu)) %>%
  mutate(covidhosdea_fu_p = if_else(covidhosdea_event_p == 1, covidhosdea_fu_w3, wave3_fu))

#wave 4
deno_hosdea_w4 <- deno_hosdea_w4 %>%
  mutate(covidhosdea_fu = if_else(covidhosdea_event == 1, covidhosdea_fu_w4, wave4_fu)) %>%
  mutate(covidhosdea_fu_p = if_else(covidhosdea_event_p == 1, covidhosdea_fu_w4, wave4_fu))

##############################################################################
#INFECTION by ethnicity - aggregated (census + PHS)
##############################################################################
setwd("")
#wave 1
deno_w1_eth<- CrossTable(deno_test_w1$ethnicgrp_cen_phs, deno_test_w1$cen_phs);deno_w1_eth
write.csv(deno_w1_eth, "deno_w1_eth.csv")

deno_w1_ethgrp_event<- CrossTable(deno_test_w1$ethnicgrp_cen_phs, deno_test_w1$covidtest_event)
write.csv(deno_w1_ethgrp_event, "deno_w1_ethgrp_event.csv")

deno_w1_ethgrp_age<- CrossTable(deno_test_w1$covidtest_event, deno_test_w1$age_5y);deno_w1_ethgrp_age
write.csv(deno_w1_ethgrp_age, "deno_w1_ethgrp_age.csv")

#wave 2
deno_w2_eth<- CrossTable(deno_test_w2$ethnicgrp_cen_phs, deno_test_w2$cen_phs);deno_w2_eth
write.csv(deno_w2_eth,"deno_w2_eth.csv")

deno_w2_ethgrp_even <- CrossTable(deno_test_w2$ethnicgrp_cen_phs, deno_test_w2$covidtest_event);deno_w2_ethgrp_even
write.csv(deno_w2_ethgrp_even,"deno_w2_ethgrp_even.csv")

#wave 3
deno_w3_eth<- CrossTable(deno_test_w3$ethnicgrp_cen_phs, deno_test_w3$cen_phs);deno_w3_eth
write.csv(deno_w3_eth,"deno_w3_eth.csv")

deno_w3_ethgrp_even<- CrossTable(deno_test_w3$ethnicgrp_cen_phs, deno_test_w3$covidtest_event);deno_w3_ethgrp_even
write.csv(deno_w3_ethgrp_even,"deno_w3_ethgrp_even.csv")
          
#wave 4
deno_w4_eth <- CrossTable(deno_test_w4$ethnicgrp_cen_phs, deno_test_w4$cen_phs);deno_w4_eth
write.csv(deno_w4_eth,"deno_w4_eth.csv")

deno_w4_ethgrp_event<- CrossTable(deno_test_w4$ethnicgrp_cen_phs, deno_test_w4$covidtest_event); deno_w4_ethgrp_event
write.csv(deno_w4_ethgrp_event,"deno_w4_ethgrp_event.csv")

##############################################################################
#HOSP by ethnicity - aggregated (census + PHS)
##############################################################################
#wave 1
deno_w1_hosp_eth<- CrossTable(deno_hosp_w1$ethnicgrp_cen_phs, deno_hosp_w1$cen_phs);
write.csv(deno_w1_hosp_eth,"deno_w1_hosp_eth.csv")

deno_w1_hosp_event<- CrossTable(deno_hosp_w1$ethnicgrp_cen_phs, deno_hosp_w1$covidhosp_event);deno_w1_hosp_event
write.csv(deno_w1_hosp_event,"deno_w1_hosp_event.csv")

#wave 2
deno_w2_hosp_eth<- CrossTable(deno_hosp_w2$ethnicgrp_cen_phs, deno_hosp_w2$cen_phs);deno_w2_hosp_eth
write.csv(deno_w2_hosp_eth,"deno_w2_hosp_eth.csv")


deno_w2_hosp_event<- CrossTable(deno_hosp_w2$ethnicgrp_cen_phs, deno_hosp_w2$covidhosp_event);deno_w2_hosp_event
write.csv(deno_w2_hosp_event,"deno_w2_hosp_event.csv")

#wave 3
deno_w3_hosp_eth<- CrossTable(deno_hosp_w3$ethnicgrp_cen_phs, deno_hosp_w3$cen_phs);deno_w3_hosp_eth
write.csv(deno_w3_hosp_eth,"deno_w3_hosp_eth.csv")

deno_w3_hosp_event<- CrossTable(deno_hosp_w3$ethnicgrp_cen_phs, deno_hosp_w3$covidhosp_event);deno_w3_hosp_event
write.csv(deno_w3_hosp_event,"deno_w3_hosp_event.csv")

#wave 4
deno_w4_hosp_eth<- CrossTable(deno_hosp_w4$ethnicgrp_cen_phs, deno_hosp_w4$cen_phs); deno_w4_hosp_eth
write.csv(deno_w4_hosp_eth,"deno_w4_hosp_eth.csv")

deno_w4_hosp_event<- CrossTable(deno_hosp_w4$ethnicgrp_cen_phs, deno_hosp_w4$covidhosp_event);deno_w4_hosp_event
write.csv(deno_w4_hosp_event,"deno_w4_hosp_event.csv")

##############################################################################
#DEATH by ethnicity - aggregated (census + PHS)
##############################################################################
#wave 1
deno_w1_death_eth<- CrossTable(deno_death_w1$ethnicgrp_cen_phs, deno_death_w1$cen_phs);deno_w1_death_eth
write.csv(deno_w1_death_eth,"deno_w1_death_eth.csv")

deno_w1_death_event<- CrossTable(deno_death_w1$ethnicgrp_cen_phs, deno_death_w1$coviddeath_event);deno_w1_death_event
write.csv(deno_w1_death_event,"deno_w1_death_event.csv")

#wave 2
deno_w2_death_eth <- CrossTable(deno_death_w2$ethnicgrp_cen_phs, deno_death_w2$cen_phs);deno_w2_death_eth
write.csv(deno_w2_death_eth,"deno_w2_death_eth.csv")

deno_w2_death_event<- CrossTable(deno_death_w2$ethnicgrp_cen_phs, deno_death_w2$coviddeath_event);deno_w2_death_event
write.csv(deno_w2_death_event,"deno_w2_death_event.csv")

#wave 3
deno_w3_death_eth <- CrossTable(deno_death_w3$ethnicgrp_cen_phs, deno_death_w3$cen_phs);deno_w3_death_eth
write.csv(deno_w3_death_eth,"deno_w3_death_eth.csv")

deno_w3_death_event<-CrossTable(deno_death_w3$ethnicgrp_cen_phs, deno_death_w3$coviddeath_event);deno_w3_death_event
write.csv(deno_w3_death_event,"deno_w3_death_event.csv")

#wave 4
deno_w4_death_eth <- CrossTable(deno_death_w4$ethnicgrp_cen_phs, deno_death_w4$cen_phs); deno_w4_death_eth
write.csv(deno_w4_death_eth,"deno_w4_death_eth.csv")

deno_w4_death_event<- CrossTable(deno_death_w4$ethnicgrp_cen_phs, deno_death_w4$coviddeath_event); deno_w4_death_event
write.csv(deno_w4_death_event,"deno_w4_death_event.csv")

##############################################################################
#HOSP/DEATH by ethnicity - aggregated (census + PHS)
##############################################################################
#wave 1
deno_w1_hosp_death_eth<- CrossTable(deno_hosdea_w1$ethnicgrp_cen_phs, deno_hosdea_w1$cen_phs);deno_w1_hosp_death_eth<
 write.csv(deno_w1_hosp_death_eth,"deno_w1_hosp_death_eth.csv") 
  
deno_w1_hosp_death_event<- CrossTable(deno_hosdea_w1$ethnicgrp_cen_phs, deno_hosdea_w1$covidhosdea_event);deno_w1_hosp_death_event
write.csv(deno_w1_hosp_death_event,"deno_w1_hosp_death_event.csv")

#wave 2
deno_w2_hosp_death_eth<- CrossTable(deno_hosdea_w2$ethnicgrp_cen_phs, deno_hosdea_w2$cen_phs);deno_w2_hosp_death_eth
write.csv(deno_w2_hosp_death_eth,"deno_w2_hosp_death_eth.csv")
  
deno_w2_hosp_death_event<- CrossTable(deno_hosdea_w2$ethnicgrp_cen_phs, deno_hosdea_w2$covidhosdea_event);deno_w2_hosp_death_event
write.csv(deno_w2_hosp_death_event,"deno_w2_hosp_death_event.csv")

#wave 3
deno_w3_hosp_death_eth<- CrossTable(deno_hosdea_w3$ethnicgrp_cen_phs, deno_hosdea_w3$cen_phs);deno_w3_hosp_death_eth
write.csv(deno_w3_hosp_death_eth,"deno_w3_hosp_death_eth.csv")

deno_w3_hosp_death_event<- CrossTable(deno_hosdea_w3$ethnicgrp_cen_phs, deno_hosdea_w3$covidhosdea_event);deno_w3_hosp_death_event
write.csv(deno_w3_hosp_death_event,"deno_w3_hosp_death_event.csv")

#wave 4
deno_w4_hosp_death_eth<- CrossTable(deno_hosdea_w4$ethnicgrp_cen_phs, deno_hosdea_w4$cen_phs);deno_w4_hosp_death_eth
write.csv(deno_w4_hosp_death_eth,"deno_w4_hosp_death_eth.csv")

deno_w4_hosp_death_event<- CrossTable(deno_hosdea_w4$ethnicgrp_cen_phs, deno_hosdea_w4$covidhosdea_event);deno_w4_hosp_death_event
write.csv(deno_w4_hosp_death_event,"deno_w4_hosp_death_event.csv")

##############################################################################
#save dervived dataset
##############################################################################
save(deno_test_w1, file = "deno_test_w1_p.RData")
save(deno_test_w2, file = "deno_test_w2_p.RData")
save(deno_test_w3, file = "deno_test_w3_p.RData")
save(deno_test_w4, file = "deno_test_w4_p.RData")

save(deno_hosp_w1, file = "deno_hosp_w1_p.RData")
save(deno_hosp_w2, file = "deno_hosp_w2_p.RData")
save(deno_hosp_w3, file = "deno_hosp_w3_p.RData")
save(deno_hosp_w4, file = "deno_hosp_w4_p.RData")

save(deno_death_w1, file = "deno_death_w1_p.RData")
save(deno_death_w2, file = "deno_death_w2_p.RData")
save(deno_death_w3, file = "deno_death_w3_p.RData")
save(deno_death_w4, file = "deno_death_w4_p.RData")

save(deno_hosdea_w1, file = "deno_hosdea_w1_p.RData")
save(deno_hosdea_w2, file = "deno_hosdea_w2_p.RData")
save(deno_hosdea_w3, file = "deno_hosdea_w3_p.RData")
save(deno_hosdea_w4, file = "deno_hosdea_w4_p.RData")
