##################################################
##Title: EAVE II ethnicity project 
##Code author(s): Sarah Amele <sarah.amele@glasgow.ac.uk>
##Description: survival analysis - by wave 
###==============================================================================
rm(list=ls())
library(dplyr)
library(expss)
library(tidyverse)
library(zoo)
library(skimr)
library(ggplot2)
library(data.table)
library(survival)
library(survminer)
library(broom)
library(Publish)

##############################################################################
#HOSPITALISATION cox models - adjusted for age, sex, and health board 
##############################################################################
load(file = "deno_hosp_w1_p.RData")
load(file = "deno_hosp_w2_p.RData")
load(file = "deno_hosp_w3_p.RData")
load(file = "deno_hosp_w4_p.RData")
#======================================================================================

#=================================================================================
#wave 1

summary(deno_hosp_w1$ethnicid_s) # missing
deno_hosp_w1_s <- deno_hosp_w1[complete.cases(deno_hosp_w1$ethnicid_s),]
summary(deno_hosp_w1_s$ethnicid_s);levels(deno_hosp_w1_s$ethnicid_s)

deno_hosp_w1<- deno_hosp_w1_s
#wave 1

#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_hosp_w1$ethnicid_s <- recode_factor(deno_hosp_w1$ethnicid_s, 'White Scottish'="White Scottish",'White Other British'="White Other British",'White Irish' ="White Irish", 'White Gypsy/Traveller'="White Gypsy/Traveller", 'White Polish'="White Polish",'Other White' ="Other White",                                               "Mixed or multiple ethnic groups"= "Mixed or multiple ethnic groups","Pakistani, Pakistani Scottish or Pakistani British"="Pakistani Pakistani Scottish or Pakistani British", "Indian, Indian Scottish or Indian British"="Indian Indian Scottish or Indian British",                                               "Bangladeshi, Bangladeshi Scottish or Bangladeshi British" ="Bangladeshi Bangladeshi Scottish or Bangladeshi British","Chinese, Chinese Scottish or Chinese British" ="Chinese Chinese Scottish or Chinese British","Other Asian" = "Other Asian",                                               'African' ="African",'Caribbean or Black'="Caribbean or Black", 'Arab, Arab Scottish or Arab British'="Arab Arab Scottish or Arab British",'Other Ethnic Group' ="Other Ethnic Group")
levels(deno_hosp_w1$ethnicid_s); summary(deno_hosp_w1$ethnicid_s)

#sex
deno_hosp_w1$sex <- as.factor(deno_hosp_w1$sex); summary(deno_hosp_w1$sex);levels(deno_hosp_w1$sex)
deno_hosp_w1$sex <- recode_factor(deno_hosp_w1$sex, '1'="male",'2'="female")

#hb
deno_hosp_w1$hb2019 <-as.factor(deno_hosp_w1$hb2019);levels(deno_hosp_w1$hb2019); summary(deno_hosp_w1$hb2019)
deno_hosp_w1$hb2019 <- recode_factor(deno_hosp_w1$hb2019, "S08000015" ="Ayrshire and Arran",
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
                                     "S08000032" = "Lanarkshire"); summary(deno_hosp_w1$hb2019)
#Age 
summary(deno_hosp_w1$age_5y)

#=========================================
coxhb_hosp_w1 <- coxph(Surv(covidhosp_fu, covidhosp_event_p) ~ ethnicid_s + age_5y + sex + hb2019, data = deno_hosp_w1)
summary(coxhb_hosp_w1)
#export output
coxhb_hosp_w1_out<- capture.output(publish(coxhb_hosp_w1, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosp_w1_out)),collapse="\n"),file="coxhb_hosp_w1_out.csv")

#===========================================================================================================

#wave 2
summary(deno_hosp_w2$ethnicid_s)
deno_hosp_w2_s <- deno_hosp_w2[complete.cases(deno_hosp_w2$ethnicid_s),]
summary(deno_hosp_w2_s$ethnicid_s);levels(deno_hosp_w2_s$ethnicid_s)

deno_hosp_w2<- deno_hosp_w2_s
#wave 1

#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_hosp_w2$ethnicid_s <- recode_factor(deno_hosp_w2$ethnicid_s, 'White Scottish'="White Scottish",'White Other British'="White Other British",'White Irish' ="White Irish", 'White Gypsy/Traveller'="White Gypsy/Traveller", 'White Polish'="White Polish",'Other White' ="Other White",                                               "Mixed or multiple ethnic groups"= "Mixed or multiple ethnic groups","Pakistani, Pakistani Scottish or Pakistani British"="Pakistani Pakistani Scottish or Pakistani British", "Indian, Indian Scottish or Indian British"="Indian Indian Scottish or Indian British",                                               "Bangladeshi, Bangladeshi Scottish or Bangladeshi British" ="Bangladeshi Bangladeshi Scottish or Bangladeshi British","Chinese, Chinese Scottish or Chinese British" ="Chinese Chinese Scottish or Chinese British","Other Asian" = "Other Asian",                                               'African' ="African",'Caribbean or Black'="Caribbean or Black", 'Arab, Arab Scottish or Arab British'="Arab Arab Scottish or Arab British",'Other Ethnic Group' ="Other Ethnic Group")
levels(deno_hosp_w2$ethnicid_s); summary(deno_hosp_w2$ethnicid_s)

#sex
deno_hosp_w2$sex <- as.factor(deno_hosp_w2$sex); summary(deno_hosp_w2$sex);levels(deno_hosp_w2$sex)
deno_hosp_w2$sex <- recode_factor(deno_hosp_w2$sex, '1'="male",'2'="female")

#hb
deno_hosp_w2$hb2019 <-as.factor(deno_hosp_w2$hb2019);levels(deno_hosp_w2$hb2019); summary(deno_hosp_w2$hb2019)
deno_hosp_w2$hb2019 <- recode_factor(deno_hosp_w2$hb2019, "S08000015" ="Ayrshire and Arran",
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
                                     "S08000032" = "Lanarkshire"); summary(deno_hosp_w2$hb2019)
#Age 
summary(deno_hosp_w2$age_5y)
#============================================
coxhb_hosp_w2 <- coxph(Surv(covidhosp_fu, covidhosp_event_p) ~ ethnicid_s + age_5y + sex + hb2019, data = deno_hosp_w2)
summary(coxhb_hosp_w2)
#export output
coxhb_hosp_w2_out<- capture.output(publish(coxhb_hosp_w2, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosp_w2_out)),collapse="\n"),file="coxhb_hosp_w2_out.csv")

#=================================================================================================

#wave 3

summary(deno_hosp_w3$ethnicid_s) 
deno_hosp_w3_s <- deno_hosp_w3[complete.cases(deno_hosp_w3$ethnicid_s),]
summary(deno_hosp_w3_s$ethnicid_s);levels(deno_hosp_w3_s$ethnicid_s)

deno_hosp_w3<- deno_hosp_w3_s

#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_hosp_w3$ethnicid_s <- recode_factor(deno_hosp_w3$ethnicid_s, 'White Scottish'="White Scottish",'White Other British'="White Other British",'White Irish' ="White Irish", 'White Gypsy/Traveller'="White Gypsy/Traveller", 'White Polish'="White Polish",'Other White' ="Other White",                                               "Mixed or multiple ethnic groups"= "Mixed or multiple ethnic groups","Pakistani, Pakistani Scottish or Pakistani British"="Pakistani Pakistani Scottish or Pakistani British", "Indian, Indian Scottish or Indian British"="Indian Indian Scottish or Indian British",                                               "Bangladeshi, Bangladeshi Scottish or Bangladeshi British" ="Bangladeshi Bangladeshi Scottish or Bangladeshi British","Chinese, Chinese Scottish or Chinese British" ="Chinese Chinese Scottish or Chinese British","Other Asian" = "Other Asian",                                               'African' ="African",'Caribbean or Black'="Caribbean or Black", 'Arab, Arab Scottish or Arab British'="Arab Arab Scottish or Arab British",'Other Ethnic Group' ="Other Ethnic Group")
levels(deno_hosp_w3$ethnicid_s); summary(deno_hosp_w3$ethnicid_s)

#sex
deno_hosp_w3$sex <- as.factor(deno_hosp_w3$sex); summary(deno_hosp_w3$sex);levels(deno_hosp_w3$sex)
deno_hosp_w3$sex <- recode_factor(deno_hosp_w3$sex, '1'="male",'2'="female")

#hb
deno_hosp_w3$hb2019 <-as.factor(deno_hosp_w3$hb2019);levels(deno_hosp_w3$hb2019); summary(deno_hosp_w3$hb2019)
deno_hosp_w3$hb2019 <- recode_factor(deno_hosp_w3$hb2019, "S08000015" ="Ayrshire and Arran",
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
                                     "S08000032" = "Lanarkshire"); summary(deno_hosp_w3$hb2019)
#Age 
summary(deno_hosp_w3$age_5y)
#=========================
coxhb_hosp_w3 <- coxph(Surv(covidhosp_fu, covidhosp_event_p) ~ ethnicid_s + age_5y + sex + hb2019, data = deno_hosp_w3)
summary(coxhb_hosp_w3)
#export output
coxhb_hosp_w3_out<- capture.output(publish(coxhb_hosp_w3, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosp_w3_out)),collapse="\n"),file="coxhb_hosp_w3_out.csv")

#================================================================================================================
#wave 4
summary(deno_hosp_w4$ethnicid_s) # missing
deno_hosp_w4_s <- deno_hosp_w4[complete.cases(deno_hosp_w4$ethnicid_s),]
summary(deno_hosp_w4_s$ethnicid_s);levels(deno_hosp_w4_s$ethnicid_s)

deno_hosp_w4<- deno_hosp_w4_s

#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_hosp_w4$ethnicid_s <- recode_factor(deno_hosp_w4$ethnicid_s, 'White Scottish'="White Scottish",'White Other British'="White Other British",'White Irish' ="White Irish", 'White Gypsy/Traveller'="White Gypsy/Traveller", 'White Polish'="White Polish",'Other White' ="Other White",                                               "Mixed or multiple ethnic groups"= "Mixed or multiple ethnic groups","Pakistani, Pakistani Scottish or Pakistani British"="Pakistani Pakistani Scottish or Pakistani British", "Indian, Indian Scottish or Indian British"="Indian Indian Scottish or Indian British",                                               "Bangladeshi, Bangladeshi Scottish or Bangladeshi British" ="Bangladeshi Bangladeshi Scottish or Bangladeshi British","Chinese, Chinese Scottish or Chinese British" ="Chinese Chinese Scottish or Chinese British","Other Asian" = "Other Asian",                                               'African' ="African",'Caribbean or Black'="Caribbean or Black", 'Arab, Arab Scottish or Arab British'="Arab Arab Scottish or Arab British",'Other Ethnic Group' ="Other Ethnic Group")
levels(deno_hosp_w4$ethnicid_s); summary(deno_hosp_w4$ethnicid_s)

#sex
deno_hosp_w4$sex <- as.factor(deno_hosp_w4$sex); summary(deno_hosp_w4$sex);levels(deno_hosp_w4$sex)
deno_hosp_w4$sex <- recode_factor(deno_hosp_w4$sex, '1'="male",'2'="female")

#hb
deno_hosp_w4$hb2019 <-as.factor(deno_hosp_w4$hb2019);levels(deno_hosp_w4$hb2019); summary(deno_hosp_w4$hb2019)
deno_hosp_w4$hb2019 <- recode_factor(deno_hosp_w4$hb2019, "S08000015" ="Ayrshire and Arran",
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
                                     "S08000032" = "Lanarkshire"); summary(deno_hosp_w4$hb2019)
#Age 
summary(deno_hosp_w4$age_5y)
#===================================
coxhb_hosp_w4 <- coxph(Surv(covidhosp_fu, covidhosp_event_p) ~ ethnicid_s + age_5y + sex + hb2019, data = deno_hosp_w4)
summary(coxhb_hosp_w4)
#export output
coxhb_hosp_w4_out<- capture.output(publish(coxhb_hosp_w4, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosp_w4_out)),collapse="\n"),file="coxhb_hosp_w4_out.csv")
#================================================================================================

#=================================================================================================
# AGE AND SEX 
#===================================================================================================

#========================================
#wave 1

#=========================================
coxhb_hosp_w1 <- coxph(Surv(covidhosp_fu, covidhosp_event_p) ~ ethnicid_s + age_5y + sex, data = deno_hosp_w1)
summary(coxhb_hosp_w1)
#export output
coxhb_hosp_w1_out<- capture.output(publish(coxhb_hosp_w1, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosp_w1_out)),collapse="\n"),file="coxhb_hosp_w1_out.csv")
#======================================================

#Wave 2
coxhb_hosp_w2 <- coxph(Surv(covidhosp_fu, covidhosp_event_p) ~ ethnicid_s + age_5y + sex, data = deno_hosp_w2)
summary(coxhb_hosp_w2)
#export output
coxhb_hosp_w2_out<- capture.output(publish(coxhb_hosp_w2, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosp_w2_out)),collapse="\n"),file="coxhb_hosp_w2_out.csv")

#========================================================

#Wave 3
coxhb_hosp_w3 <- coxph(Surv(covidhosp_fu, covidhosp_event_p) ~ ethnicid_s + age_5y + sex, data = deno_hosp_w3)
summary(coxhb_hosp_w3)
#export output
coxhb_hosp_w3_out<- capture.output(publish(coxhb_hosp_w3, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosp_w3_out)),collapse="\n"),file="coxhb_hosp_w3_out.csv")
#==========================================================

#Wave 4
coxhb_hosp_w4 <- coxph(Surv(covidhosp_fu, covidhosp_event_p) ~ ethnicid_s + age_5y + sex, data = deno_hosp_w4)
summary(coxhb_hosp_w4)
#export output
coxhb_hosp_w4_out<- capture.output(publish(coxhb_hosp_w4, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosp_w4_out)),collapse="\n"),file="coxhb_hosp_w4_out.csv")


##############################################################################
#DEATH cox models - adjusted for age, sex, and health board 
##############################################################################
load(file = "deno_death_w1.RData")
load(file = "deno_death_w2.RData")
load(file = "deno_death_w3.RData")
load(file = "deno_death_w4.RData")

#==============================================================
#AGE. SEX and HB
#===============================================================
setwd("")
#==========================================================

#Wave 1 
summary(deno_death_w1$ethnicid_s) ##missing
deno_death_w1_s <- deno_death_w1[complete.cases(deno_death_w1$ethnicid_s),]
summary(deno_death_w1_s$ethnicid_s);levels(deno_death_w1_s$ethnicid_s)

deno_death_w1<- deno_death_w1_s

#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_death_w1$ethnicid_s <- recode_factor(deno_death_w1$ethnicid_s, 'White Scottish'="White Scottish",'White Other British'="White Other British",'White Irish' ="White Irish", 'White Gypsy/Traveller'="White Gypsy/Traveller", 'White Polish'="White Polish",'Other White' ="Other White",                                               "Mixed or multiple ethnic groups"= "Mixed or multiple ethnic groups","Pakistani, Pakistani Scottish or Pakistani British"="Pakistani Pakistani Scottish or Pakistani British", "Indian, Indian Scottish or Indian British"="Indian Indian Scottish or Indian British",                                               "Bangladeshi, Bangladeshi Scottish or Bangladeshi British" ="Bangladeshi Bangladeshi Scottish or Bangladeshi British","Chinese, Chinese Scottish or Chinese British" ="Chinese Chinese Scottish or Chinese British","Other Asian" = "Other Asian",                                               'African' ="African",'Caribbean or Black'="Caribbean or Black", 'Arab, Arab Scottish or Arab British'="Arab Arab Scottish or Arab British",'Other Ethnic Group' ="Other Ethnic Group")
levels(deno_death_w1$ethnicid_s); summary(deno_death_w1$ethnicid_s)

#sex
deno_death_w1$sex <- as.factor(deno_death_w1$sex); summary(deno_death_w1$sex);levels(deno_death_w1$sex)
deno_death_w1$sex <- recode_factor(deno_death_w1$sex, '1'="male",'2'="female")

#hb
deno_death_w1$hb2019 <-as.factor(deno_death_w1$hb2019);levels(deno_death_w1$hb2019); summary(deno_death_w1$hb2019)
deno_death_w1$hb2019 <- recode_factor(deno_death_w1$hb2019, "S08000015" ="Ayrshire and Arran",
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
                                      "S08000032" = "Lanarkshire"); summary(deno_death_w1$hb2019)
#Age 
summary(deno_death_w1$age_5y)
#==========================
coxhb_death_w1 <- coxph(Surv(coviddeath_fu, coviddeath_event) ~ ethnicid_s + age_5y + sex + hb2019, data = deno_death_w1)
summary(coxhb_death_w1)
#export output
coxhb_death_w1_out<- capture.output(publish(coxhb_death_w1, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_death_w1_out)),collapse="\n"),file="coxhb_death_w1_out.csv")
#=====================================================================================================================
#wave 2
summary(deno_death_w2$ethnicid_s) 
deno_death_w2_s <- deno_death_w2[complete.cases(deno_death_w2$ethnicid_s),]
summary(deno_death_w2_s$ethnicid_s);levels(deno_death_w2_s$ethnicid_s)

deno_death_w2<- deno_death_w2_s

#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_death_w2$ethnicid_s <- recode_factor(deno_death_w2$ethnicid_s, 'White Scottish'="White Scottish",'White Other British'="White Other British",'White Irish' ="White Irish", 'White Gypsy/Traveller'="White Gypsy/Traveller", 'White Polish'="White Polish",'Other White' ="Other White",                                               "Mixed or multiple ethnic groups"= "Mixed or multiple ethnic groups","Pakistani, Pakistani Scottish or Pakistani British"="Pakistani Pakistani Scottish or Pakistani British", "Indian, Indian Scottish or Indian British"="Indian Indian Scottish or Indian British",                                               "Bangladeshi, Bangladeshi Scottish or Bangladeshi British" ="Bangladeshi Bangladeshi Scottish or Bangladeshi British","Chinese, Chinese Scottish or Chinese British" ="Chinese Chinese Scottish or Chinese British","Other Asian" = "Other Asian",                                               'African' ="African",'Caribbean or Black'="Caribbean or Black", 'Arab, Arab Scottish or Arab British'="Arab Arab Scottish or Arab British",'Other Ethnic Group' ="Other Ethnic Group")
levels(deno_death_w2$ethnicid_s); summary(deno_death_w2$ethnicid_s)

#sex
deno_death_w2$sex <- as.factor(deno_death_w2$sex); summary(deno_death_w2$sex);levels(deno_death_w2$sex)
deno_death_w2$sex <- recode_factor(deno_death_w2$sex, '1'="male",'2'="female")

#hb
deno_death_w2$hb2019 <-as.factor(deno_death_w2$hb2019);levels(deno_death_w2$hb2019); summary(deno_death_w2$hb2019)
deno_death_w2$hb2019 <- recode_factor(deno_death_w2$hb2019, "S08000015" ="Ayrshire and Arran",
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
                                      "S08000032" = "Lanarkshire"); summary(deno_death_w2$hb2019)
#Age 
summary(deno_death_w2$age_5y)
#==========================================
coxhb_death_w2 <- coxph(Surv(coviddeath_fu, coviddeath_event) ~ ethnicid_s + age_5y + sex + hb2019, data = deno_death_w2)
summary(coxhb_death_w2)
#export output
coxhb_death_w2_out<- capture.output(publish(coxhb_death_w2, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_death_w2_out)),collapse="\n"),file="coxhb_death_w2_out.csv")
#============================================================================
#wave 3
summary(deno_death_w3$ethnicid_s)
deno_death_w3_s <- deno_death_w3[complete.cases(deno_death_w3$ethnicid_s),]
summary(deno_death_w3_s$ethnicid_s);levels(deno_death_w3_s$ethnicid_s)

deno_death_w3<- deno_death_w3_s

#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_death_w3$ethnicid_s <- recode_factor(deno_death_w3$ethnicid_s, 'White Scottish'="White Scottish",'White Other British'="White Other British",'White Irish' ="White Irish", 'White Gypsy/Traveller'="White Gypsy/Traveller", 'White Polish'="White Polish",'Other White' ="Other White",                                               "Mixed or multiple ethnic groups"= "Mixed or multiple ethnic groups","Pakistani, Pakistani Scottish or Pakistani British"="Pakistani Pakistani Scottish or Pakistani British", "Indian, Indian Scottish or Indian British"="Indian Indian Scottish or Indian British",                                               "Bangladeshi, Bangladeshi Scottish or Bangladeshi British" ="Bangladeshi Bangladeshi Scottish or Bangladeshi British","Chinese, Chinese Scottish or Chinese British" ="Chinese Chinese Scottish or Chinese British","Other Asian" = "Other Asian",                                               'African' ="African",'Caribbean or Black'="Caribbean or Black", 'Arab, Arab Scottish or Arab British'="Arab Arab Scottish or Arab British",'Other Ethnic Group' ="Other Ethnic Group")
levels(deno_death_w3$ethnicid_s); summary(deno_death_w3$ethnicid_s)

#sex
deno_death_w3$sex <- as.factor(deno_death_w3$sex); summary(deno_death_w3$sex);levels(deno_death_w3$sex)
deno_death_w3$sex <- recode_factor(deno_death_w3$sex, '1'="male",'2'="female")

#hb
deno_death_w3$hb2019 <-as.factor(deno_death_w3$hb2019);levels(deno_death_w3$hb2019); summary(deno_death_w3$hb2019)
deno_death_w3$hb2019 <- recode_factor(deno_death_w3$hb2019, "S08000015" ="Ayrshire and Arran",
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
                                      "S08000032" = "Lanarkshire"); summary(deno_death_w3$hb2019)
#Age 
summary(deno_death_w3$age_5y)
#=======================================
coxhb_death_w3 <- coxph(Surv(coviddeath_fu, coviddeath_event) ~ ethnicid_s + age_5y + sex + hb2019, data = deno_death_w3)
summary(coxhb_death_w3)
#export output
coxhb_death_w3_out<- capture.output(publish(coxhb_death_w3, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_death_w3_out)),collapse="\n"),file="coxhb_death_w3_out.csv")
#====================================================================================

#wave 4
summary(deno_death_w4$ethnicid_s)
deno_death_w4_s <- deno_death_w4[complete.cases(deno_death_w4$ethnicid_s),]
summary(deno_death_w4_s$ethnicid_s);levels(deno_death_w4_s$ethnicid_s)

deno_death_w4<- deno_death_w4_s

#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_death_w4$ethnicid_s <- recode_factor(deno_death_w4$ethnicid_s, 'White Scottish'="White Scottish",'White Other British'="White Other British",'White Irish' ="White Irish", 'White Gypsy/Traveller'="White Gypsy/Traveller", 'White Polish'="White Polish",'Other White' ="Other White",                                               "Mixed or multiple ethnic groups"= "Mixed or multiple ethnic groups","Pakistani, Pakistani Scottish or Pakistani British"="Pakistani Pakistani Scottish or Pakistani British", "Indian, Indian Scottish or Indian British"="Indian Indian Scottish or Indian British",                                               "Bangladeshi, Bangladeshi Scottish or Bangladeshi British" ="Bangladeshi Bangladeshi Scottish or Bangladeshi British","Chinese, Chinese Scottish or Chinese British" ="Chinese Chinese Scottish or Chinese British","Other Asian" = "Other Asian",                                               'African' ="African",'Caribbean or Black'="Caribbean or Black", 'Arab, Arab Scottish or Arab British'="Arab Arab Scottish or Arab British",'Other Ethnic Group' ="Other Ethnic Group")
levels(deno_death_w4$ethnicid_s); summary(deno_death_w4$ethnicid_s)

#sex
deno_death_w4$sex <- as.factor(deno_death_w4$sex); summary(deno_death_w4$sex);levels(deno_death_w4$sex)
deno_death_w4$sex <- recode_factor(deno_death_w4$sex, '1'="male",'2'="female")

#hb
deno_death_w4$hb2019 <-as.factor(deno_death_w4$hb2019);levels(deno_death_w4$hb2019); summary(deno_death_w4$hb2019)
deno_death_w4$hb2019 <- recode_factor(deno_death_w4$hb2019, "S08000015" ="Ayrshire and Arran",
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
                                      "S08000032" = "Lanarkshire"); summary(deno_death_w4$hb2019)
#Age 
summary(deno_death_w4$age_5y)
#============================================================
coxhb_death_w4 <- coxph(Surv(coviddeath_fu, coviddeath_event) ~ ethnicid_s + age_5y + sex + hb2019, data = deno_death_w4)
summary(coxhb_death_w4)
#export output
coxhb_death_w4_out<- capture.output(publish(coxhb_death_w4, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_death_w4_out)),collapse="\n"),file="coxhb_death_w4_out.csv")
#=====================================

#=================================================================================================
# AGE AND SEX 
#===================================================================================================
setwd("")
#========================================
#wave 1

coxhb_death_w1 <- coxph(Surv(coviddeath_fu, coviddeath_event) ~ ethnicid_s + age_5y + sex, data = deno_death_w1)
summary(coxhb_death_w1)
#export output
coxhb_death_w1_out<- capture.output(publish(coxhb_death_w1, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_death_w1_out)),collapse="\n"),file="coxhb_death_w1_out.csv")
#======================================================

#Wave 2
coxhb_death_w2 <- coxph(Surv(coviddeath_fu, coviddeath_event) ~ ethnicid_s + age_5y + sex, data = deno_death_w2)
summary(coxhb_death_w2)
#export output
coxhb_death_w2_out<- capture.output(publish(coxhb_death_w2, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_death_w2_out)),collapse="\n"),file="coxhb_death_w2_out.csv")

#========================================================

#Wave 3
coxhb_death_w3 <- coxph(Surv(coviddeath_fu, coviddeath_event) ~ ethnicid_s + age_5y + sex, data = deno_death_w3)
summary(coxhb_death_w3)
#export output
coxhb_death_w3_out<- capture.output(publish(coxhb_death_w3, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_death_w3_out)),collapse="\n"),file="coxhb_death_w3_out.csv")
#==========================================================

#Wave 4
coxhb_death_w4 <- coxph(Surv(coviddeath_fu, coviddeath_event) ~ ethnicid_s + age_5y + sex, data = deno_death_w4)
summary(coxhb_death_w4)
#export output
coxhb_death_w4_out<- capture.output(publish(coxhb_death_w4, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_death_w4_out)),collapse="\n"),file="coxhb_death_w4_out.csv")
#===============================================================================================================
#No adjustment
#======================================================================
setwd("")
#=======================================================
#wave 1

coxhb_death_w1 <- coxph(Surv(coviddeath_fu, coviddeath_event) ~ ethnicid_s , data = deno_death_w1)
summary(coxhb_death_w1)
#export output
coxhb_death_w1_out<- capture.output(publish(coxhb_death_w1, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_death_w1_out)),collapse="\n"),file="coxhb_death_w1_out.csv")
#======================================================

#Wave 2
coxhb_death_w2 <- coxph(Surv(coviddeath_fu, coviddeath_event) ~ ethnicid_s , data = deno_death_w2)
summary(coxhb_death_w2)
#export output
coxhb_death_w2_out<- capture.output(publish(coxhb_death_w2, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_death_w2_out)),collapse="\n"),file="coxhb_death_w2_out.csv")

#========================================================

#Wave 3
coxhb_death_w3 <- coxph(Surv(coviddeath_fu, coviddeath_event) ~ ethnicid_s , data = deno_death_w3)
summary(coxhb_death_w3)
#export output
coxhb_death_w3_out<- capture.output(publish(coxhb_death_w3, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_death_w3_out)),collapse="\n"),file="coxhb_death_w3_out.csv")
#==========================================================

#Wave 4
coxhb_death_w4 <- coxph(Surv(coviddeath_fu, coviddeath_event) ~ ethnicid_s , data = deno_death_w4)
summary(coxhb_death_w4)
#export output
coxhb_death_w4_out<- capture.output(publish(coxhb_death_w4, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_death_w4_out)),collapse="\n"),file="coxhb_death_w4_out.csv")

##############################################################################
#HOSP/DEATH cox models - adjusted for age, sex, and health board
##############################################################################

setwd("")

load(file = "deno_hosdea_w1.RData")
load(file = "deno_hosdea_w2.RData")
load(file = "deno_hosdea_w3.RData")
load(file = "deno_hosdea_w4.RData")

#==============================================================
#data management 
#================================================================
summary(deno_hosdea_w1$ethnicid_s) 
deno_hosdea_w1_s <- deno_hosdea_w1[complete.cases(deno_hosdea_w1$ethnicid_s),]
summary(deno_hosdea_w1_s$ethnicid_s);levels(deno_hosdea_w1_s$ethnicid_s)

deno_hosdea_w1<- deno_hosdea_w1_s

#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_hosdea_w1$ethnicid_s <- recode_factor(deno_hosdea_w1$ethnicid_s, 'White Scottish'="White Scottish",'White Other British'="White Other British",'White Irish' ="White Irish", 'White Gypsy/Traveller'="White Gypsy/Traveller", 'White Polish'="White Polish",'Other White' ="Other White",                                               "Mixed or multiple ethnic groups"= "Mixed or multiple ethnic groups","Pakistani, Pakistani Scottish or Pakistani British"="Pakistani Pakistani Scottish or Pakistani British", "Indian, Indian Scottish or Indian British"="Indian Indian Scottish or Indian British",                                               "Bangladeshi, Bangladeshi Scottish or Bangladeshi British" ="Bangladeshi Bangladeshi Scottish or Bangladeshi British","Chinese, Chinese Scottish or Chinese British" ="Chinese Chinese Scottish or Chinese British","Other Asian" = "Other Asian",                                               'African' ="African",'Caribbean or Black'="Caribbean or Black", 'Arab, Arab Scottish or Arab British'="Arab Arab Scottish or Arab British",'Other Ethnic Group' ="Other Ethnic Group")
levels(deno_hosdea_w1$ethnicid_s); summary(deno_hosdea_w1$ethnicid_s)

#sex
deno_hosdea_w1$sex <- as.factor(deno_hosdea_w1$sex); summary(deno_hosdea_w1$sex);levels(deno_hosdea_w1$sex)
deno_hosdea_w1$sex <- recode_factor(deno_hosdea_w1$sex, '1'="male",'2'="female")

#hb
deno_hosdea_w1$hb2019 <-as.factor(deno_hosdea_w1$hb2019);levels(deno_hosdea_w1$hb2019); summary(deno_hosdea_w1$hb2019)
deno_hosdea_w1$hb2019 <- recode_factor(deno_hosdea_w1$hb2019, "S08000015" ="Ayrshire and Arran",
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
                                       "S08000032" = "Lanarkshire"); summary(deno_hosdea_w1$hb2019)
#Age 
summary(deno_hosdea_w1$age_5y)

#======================================================================================================
#wave 2
summary(deno_hosdea_w2$ethnicid_s) 
deno_hosdea_w2_s <- deno_hosdea_w2[complete.cases(deno_hosdea_w2$ethnicid_s),]
summary(deno_hosdea_w2_s$ethnicid_s);levels(deno_hosdea_w2_s$ethnicid_s)

deno_hosdea_w2<- deno_hosdea_w2_s

#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_hosdea_w2$ethnicid_s <- recode_factor(deno_hosdea_w2$ethnicid_s, 'White Scottish'="White Scottish",'White Other British'="White Other British",'White Irish' ="White Irish", 'White Gypsy/Traveller'="White Gypsy/Traveller", 'White Polish'="White Polish",'Other White' ="Other White",                                               "Mixed or multiple ethnic groups"= "Mixed or multiple ethnic groups","Pakistani, Pakistani Scottish or Pakistani British"="Pakistani Pakistani Scottish or Pakistani British", "Indian, Indian Scottish or Indian British"="Indian Indian Scottish or Indian British",                                               "Bangladeshi, Bangladeshi Scottish or Bangladeshi British" ="Bangladeshi Bangladeshi Scottish or Bangladeshi British","Chinese, Chinese Scottish or Chinese British" ="Chinese Chinese Scottish or Chinese British","Other Asian" = "Other Asian",                                               'African' ="African",'Caribbean or Black'="Caribbean or Black", 'Arab, Arab Scottish or Arab British'="Arab Arab Scottish or Arab British",'Other Ethnic Group' ="Other Ethnic Group")
levels(deno_hosdea_w2$ethnicid_s); summary(deno_hosdea_w2$ethnicid_s)

#sex
deno_hosdea_w2$sex <- as.factor(deno_hosdea_w2$sex); summary(deno_hosdea_w2$sex);levels(deno_hosdea_w2$sex)
deno_hosdea_w2$sex <- recode_factor(deno_hosdea_w2$sex, '1'="male",'2'="female")

#hb
deno_hosdea_w2$hb2019 <-as.factor(deno_hosdea_w2$hb2019);levels(deno_hosdea_w2$hb2019); summary(deno_hosdea_w2$hb2019)
deno_hosdea_w2$hb2019 <- recode_factor(deno_hosdea_w2$hb2019, "S08000015" ="Ayrshire and Arran",
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
                                       "S08000032" = "Lanarkshire"); summary(deno_hosdea_w2$hb2019)
#Age 
summary(deno_hosdea_w2$age_5y)
#==================================

#========================================================================================
#wave 3
summary(deno_hosdea_w3$ethnicid_s)
deno_hosdea_w3_s <- deno_hosdea_w3[complete.cases(deno_hosdea_w3$ethnicid_s),]
summary(deno_hosdea_w3_s$ethnicid_s);levels(deno_hosdea_w3_s$ethnicid_s)

deno_hosdea_w3<- deno_hosdea_w3_s

#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_hosdea_w3$ethnicid_s <- recode_factor(deno_hosdea_w3$ethnicid_s, 'White Scottish'="White Scottish",'White Other British'="White Other British",'White Irish' ="White Irish", 'White Gypsy/Traveller'="White Gypsy/Traveller", 'White Polish'="White Polish",'Other White' ="Other White",                                               "Mixed or multiple ethnic groups"= "Mixed or multiple ethnic groups","Pakistani, Pakistani Scottish or Pakistani British"="Pakistani Pakistani Scottish or Pakistani British", "Indian, Indian Scottish or Indian British"="Indian Indian Scottish or Indian British",                                               "Bangladeshi, Bangladeshi Scottish or Bangladeshi British" ="Bangladeshi Bangladeshi Scottish or Bangladeshi British","Chinese, Chinese Scottish or Chinese British" ="Chinese Chinese Scottish or Chinese British","Other Asian" = "Other Asian",                                               'African' ="African",'Caribbean or Black'="Caribbean or Black", 'Arab, Arab Scottish or Arab British'="Arab Arab Scottish or Arab British",'Other Ethnic Group' ="Other Ethnic Group")
levels(deno_hosdea_w3$ethnicid_s); summary(deno_hosdea_w3$ethnicid_s)

#sex
deno_hosdea_w3$sex <- as.factor(deno_hosdea_w3$sex); summary(deno_hosdea_w3$sex);levels(deno_hosdea_w3$sex)
deno_hosdea_w3$sex <- recode_factor(deno_hosdea_w3$sex, '1'="male",'2'="female")

#hb
deno_hosdea_w3$hb2019 <-as.factor(deno_hosdea_w3$hb2019);levels(deno_hosdea_w3$hb2019); summary(deno_hosdea_w3$hb2019)
deno_hosdea_w3$hb2019 <- recode_factor(deno_hosdea_w3$hb2019, "S08000015" ="Ayrshire and Arran",
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
                                       "S08000032" = "Lanarkshire"); summary(deno_hosdea_w3$hb2019)
#Age 
summary(deno_hosdea_w3$age_5y)
#====================================================================================

#wave 4
summary(deno_hosdea_w4$ethnicid_s) 
deno_hosdea_w4_s <- deno_hosdea_w4[complete.cases(deno_hosdea_w4$ethnicid_s),]
summary(deno_hosdea_w4_s$ethnicid_s);levels(deno_hosdea_w4_s$ethnicid_s)

deno_hosdea_w4<- deno_hosdea_w4_s

#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_hosdea_w4$ethnicid_s <- recode_factor(deno_hosdea_w4$ethnicid_s, 'White Scottish'="White Scottish",'White Other British'="White Other British",'White Irish' ="White Irish", 'White Gypsy/Traveller'="White Gypsy/Traveller", 'White Polish'="White Polish",'Other White' ="Other White",                                               "Mixed or multiple ethnic groups"= "Mixed or multiple ethnic groups","Pakistani, Pakistani Scottish or Pakistani British"="Pakistani Pakistani Scottish or Pakistani British", "Indian, Indian Scottish or Indian British"="Indian Indian Scottish or Indian British",                                               "Bangladeshi, Bangladeshi Scottish or Bangladeshi British" ="Bangladeshi Bangladeshi Scottish or Bangladeshi British","Chinese, Chinese Scottish or Chinese British" ="Chinese Chinese Scottish or Chinese British","Other Asian" = "Other Asian",                                               'African' ="African",'Caribbean or Black'="Caribbean or Black", 'Arab, Arab Scottish or Arab British'="Arab Arab Scottish or Arab British",'Other Ethnic Group' ="Other Ethnic Group")
levels(deno_hosdea_w4$ethnicid_s); summary(deno_hosdea_w4$ethnicid_s)

#sex
deno_hosdea_w4$sex <- as.factor(deno_hosdea_w4$sex); summary(deno_hosdea_w4$sex);levels(deno_hosdea_w4$sex)
deno_hosdea_w4$sex <- recode_factor(deno_hosdea_w4$sex, '1'="male",'2'="female")

#hb
deno_hosdea_w4$hb2019 <-as.factor(deno_hosdea_w4$hb2019);levels(deno_hosdea_w4$hb2019); summary(deno_hosdea_w4$hb2019)
deno_hosdea_w4$hb2019 <- recode_factor(deno_hosdea_w4$hb2019, "S08000015" ="Ayrshire and Arran",
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
                                       "S08000032" = "Lanarkshire"); summary(deno_hosdea_w4$hb2019)
#Age 
summary(deno_hosdea_w4$age_5y)

#============================================================================================
# age, sex, health board 
#=========================
setwd("")
#=========================================================================================
#Wave 1 
coxhb_hosdea_w1 <- coxph(Surv(covidhosdea_fu, covidhosdea_event) ~ ethnicid_s + age_5y + sex + hb2019, data = deno_hosdea_w1)
summary(coxhb_hosdea_w1)
#export output
coxhb_hosdea_w1_out<- capture.output(publish(coxhb_hosdea_w1, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosdea_w1_out)),collapse="\n"),file="coxhb_hosdea_w1_out.csv")
#=======================================================

#Wave 2
coxhb_hosdea_w2 <- coxph(Surv(covidhosdea_fu, covidhosdea_event) ~ ethnicid_s + age_5y + sex + hb2019, data = deno_hosdea_w2)
summary(coxhb_hosdea_w2)
#export output
coxhb_hosdea_w2_out<- capture.output(publish(coxhb_hosdea_w2, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosdea_w2_out)),collapse="\n"),file="coxhb_hosdea_w2_out.csv")
#========================================================

#Wave 3 
coxhb_hosdea_w3 <- coxph(Surv(covidhosdea_fu, covidhosdea_event) ~ ethnicid_s + age_5y + sex + hb2019, data = deno_hosdea_w3)
summary(coxhb_hosdea_w3)
#========================
#export output
coxhb_hosdea_w3_out<- capture.output(publish(coxhb_hosdea_w3, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosdea_w3_out)),collapse="\n"),file="coxhb_hosdea_w3_out.csv")
#==========================================

#wave 4
coxhb_hosdea_w4 <- coxph(Surv(covidhosdea_fu, covidhosdea_event) ~ ethnicid_s + age_5y + sex + hb2019, data = deno_hosdea_w4)
summary(coxhb_hosdea_w4)
#========================
#export output
coxhb_hosdea_w4_out<- capture.output(publish(coxhb_hosdea_w4, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosdea_w4_out)),collapse="\n"),file="coxhb_hosdea_w4_out.csv")

#=================================================================================================
# AGE AND SEX 
#===================================================================================================
setwd("")
#========================================
#wave 1
coxhb_hosdea_w1 <- coxph(Surv(covidhosdea_fu, covidhosdea_event) ~ ethnicid_s + age_5y + sex, data = deno_hosdea_w1)
summary(coxhb_hosdea_w1)
#export output
coxhb_hosdea_w1_out<- capture.output(publish(coxhb_hosdea_w1, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosdea_w1_out)),collapse="\n"),file="coxhb_hosdea_w1_out.csv")

#======================================================

#Wave 2
coxhb_hosdea_w2 <- coxph(Surv(covidhosdea_fu, covidhosdea_event) ~ ethnicid_s + age_5y + sex, data = deno_hosdea_w2)
summary(coxhb_hosdea_w2)
#export output
coxhb_hosdea_w2_out<- capture.output(publish(coxhb_hosdea_w2, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosdea_w2_out)),collapse="\n"),file="coxhb_hosdea_w2_out.csv")

#========================================================

#Wave 3
coxhb_hosdea_w3 <- coxph(Surv(covidhosdea_fu, covidhosdea_event) ~ ethnicid_s + age_5y + sex, data = deno_hosdea_w3)
summary(coxhb_hosdea_w3)
#========================
#export output
coxhb_hosdea_w3_out<- capture.output(publish(coxhb_hosdea_w3, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosdea_w3_out)),collapse="\n"),file="coxhb_hosdea_w3_out.csv")
#==========================================================

#Wave 4
coxhb_hosdea_w4 <- coxph(Surv(covidhosdea_fu, covidhosdea_event) ~ ethnicid_s + age_5y + sex, data = deno_hosdea_w4)
summary(coxhb_hosdea_w4)
#========================
#export output
coxhb_hosdea_w4_out<- capture.output(publish(coxhb_hosdea_w4, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosdea_w4_out)),collapse="\n"),file="coxhb_hosdea_w4_out.csv")

#=========================================================================================================


#===============================================================================================================
#No adjustment
#======================================================================
setwd("")
#=======================================================

#wave 1
coxhb_hosdea_w1 <- coxph(Surv(covidhosdea_fu, covidhosdea_event) ~ ethnicid_s , data = deno_hosdea_w1)
summary(coxhb_hosdea_w1)
#export output
coxhb_hosdea_w1_out<- capture.output(publish(coxhb_hosdea_w1, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosdea_w1_out)),collapse="\n"),file="coxhb_hosdea_w1_out.csv")

#======================================================

#Wave 2
coxhb_hosdea_w2 <- coxph(Surv(covidhosdea_fu, covidhosdea_event) ~ ethnicid_s , data = deno_hosdea_w2)
summary(coxhb_hosdea_w2)
#export output
coxhb_hosdea_w2_out<- capture.output(publish(coxhb_hosdea_w2, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosdea_w2_out)),collapse="\n"),file="coxhb_hosdea_w2_out.csv")

#========================================================

#Wave 3
coxhb_hosdea_w3 <- coxph(Surv(covidhosdea_fu, covidhosdea_event) ~ ethnicid_s , data = deno_hosdea_w3)
summary(coxhb_hosdea_w3)
#========================
#export output
coxhb_hosdea_w3_out<- capture.output(publish(coxhb_hosdea_w3, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosdea_w3_out)),collapse="\n"),file="coxhb_hosdea_w3_out.csv")
#==========================================================

#Wave 4
coxhb_hosdea_w4 <- coxph(Surv(covidhosdea_fu, covidhosdea_event) ~ ethnicid_s , data = deno_hosdea_w4)
summary(coxhb_hosdea_w4)
#========================
#export output
coxhb_hosdea_w4_out<- capture.output(publish(coxhb_hosdea_w4, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosdea_w4_out)),collapse="\n"),file="coxhb_hosdea_w4_out.csv")











