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
library(janitor)

##############################################################################
#load infection 
##############################################################################
setwd("")

load(file = "deno_test_w1.RData")
load(file = "deno_test_w2.RData")
load(file = "deno_test_w3.RData")
load(file = "deno_test_w4.RData")
#========================================================================================================
#INFECTION cox models - adjusted for age, sex, and health board 
#==================================================================
setwd("")

#==========================================
#wave 1

#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_test_w1$ethnicgrp_cen_phs <- recode_factor(deno_test_w1$ethnicgrp_cen_phs, White = "White", 'Mixed or multiple ethnic groups'= "Mixed or multiple ethnic groups", Asian ="Asian", 'African, Caribbean or Black'="African Caribbean or Black", 'Other Ethnic Group'="Other Ethnic Group")
levels(deno_test_w1$ethnicgrp_cen_phs); summary(deno_test_w1$ethnicgrp_cen_phs)

#sex
deno_test_w1$sex <- as.factor(deno_test_w1$sex); summary(deno_test_w1$sex);levels(deno_test_w1$sex)
deno_test_w1$sex <- recode_factor(deno_test_w1$sex, '1'="male",'2'="female")

#hb
deno_test_w1$hb2019 <-as.factor(deno_test_w1$hb2019);levels(deno_test_w1$hb2019); summary(deno_test_w1$hb2019)
deno_test_w1$hb2019 <- recode_factor(deno_test_w1$hb2019, "S08000015" ="Ayrshire and Arran",
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
                                     "S08000032" = "Lanarkshire"); summary(deno_test_w1$hb2019)
#Age 
summary(deno_test_w1$age_5y)
 #=========================
ct1<- tabyl(deno_test_w1,covidtest_event, ethnicgrp_cen_phs)
ct1

coxhb_test_w1 <- coxph(Surv(covidtest_fu, covidtest_event) ~ ethnicgrp_cen_phs + age_5y + sex + hb2019, data = deno_test_w1)
summary(coxhb_test_w1)
#export output
coxhb_test_w1_out<- capture.output(publish(coxhb_test_w1, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_test_w1_out)),collapse="\n"),file="coxhb_test_w1_out.csv")

#===================================================================================
#wave 2
#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_test_w2$ethnicgrp_cen_phs <- recode_factor(deno_test_w2$ethnicgrp_cen_phs, White = "White", 'Mixed or multiple ethnic groups'= "Mixed or multiple ethnic groups", Asian ="Asian", 'African, Caribbean or Black'="African Caribbean or Black", 'Other Ethnic Group'="Other Ethnic Group")
levels(deno_test_w2$ethnicgrp_cen_phs); summary(deno_test_w2$ethnicgrp_cen_phs)
#sex
deno_test_w2$sex <- as.factor(deno_test_w2$sex); summary(deno_test_w2$sex);levels(deno_test_w2$sex)
deno_test_w2$sex <- recode_factor(deno_test_w2$sex, '1'="male",'2'="female")
#hb
deno_test_w2$hb2019 <-as.factor(deno_test_w2$hb2019);levels(deno_test_w2$hb2019); summary(deno_test_w2$hb2019)
deno_test_w2$hb2019 <- recode_factor(deno_test_w2$hb2019, "S08000015" ="Ayrshire and Arran",
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
                                     "S08000032" = "Lanarkshire"); summary(deno_test_w2$hb2019)
#Age 
summary(deno_test_w2$age_5y)
#===============================================
ct2<- tabyl(deno_test_w2,covidtest_event, ethnicgrp_cen_phs)
ct2
coxhb_test_w2 <- coxph(Surv(covidtest_fu, covidtest_event) ~ ethnicgrp_cen_phs + age_5y + sex + hb2019, data = deno_test_w2)
summary(coxhb_test_w2)
#export output
coxhb_test_w2_out<- capture.output(publish(coxhb_test_w2, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_test_w2_out)),collapse="\n"),file="coxhb_test_w2_out.csv")

#==========================================================================================================
#wave 3
#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_test_w3$ethnicgrp_cen_phs <- recode_factor(deno_test_w3$ethnicgrp_cen_phs, White = "White", 'Mixed or multiple ethnic groups'= "Mixed or multiple ethnic groups", Asian ="Asian", 'African, Caribbean or Black'="African Caribbean or Black", 'Other Ethnic Group'="Other Ethnic Group")
levels(deno_test_w3$ethnicgrp_cen_phs); summary(deno_test_w3$ethnicgrp_cen_phs)
#sex
deno_test_w3$sex <- as.factor(deno_test_w3$sex); summary(deno_test_w3$sex);levels(deno_test_w3$sex)
deno_test_w3$sex <- recode_factor(deno_test_w3$sex, '1'="male",'2'="female")
#hb
deno_test_w3$hb2019 <-as.factor(deno_test_w3$hb2019);levels(deno_test_w3$hb2019); summary(deno_test_w3$hb2019)
deno_test_w3$hb2019 <- recode_factor(deno_test_w3$hb2019, "S08000015" ="Ayrshire and Arran",
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
                                     "S08000032" = "Lanarkshire"); summary(deno_test_w3$hb2019)
#Age 
summary(deno_test_w3$age_5y)
#==============================================
ct3<- tabyl(deno_test_w3,covidtest_event, ethnicgrp_cen_phs)
ct3
coxhb_test_w3 <- coxph(Surv(covidtest_fu, covidtest_event) ~ ethnicgrp_cen_phs + age_5y + sex + hb2019, data = deno_test_w3)
summary(coxhb_test_w3)
#export output
coxhb_test_w3_out<- capture.output(publish(coxhb_test_w3, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_test_w3_out)),collapse="\n"),file="coxhb_test_w3_out.csv")
#==========================================================================================================================
#wave 4
#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_test_w4$ethnicgrp_cen_phs <- recode_factor(deno_test_w4$ethnicgrp_cen_phs, White = "White", 'Mixed or multiple ethnic groups'= "Mixed or multiple ethnic groups", Asian ="Asian", 'African, Caribbean or Black'="African Caribbean or Black", 'Other Ethnic Group'="Other Ethnic Group")
levels(deno_test_w4$ethnicgrp_cen_phs); summary(deno_test_w4$ethnicgrp_cen_phs)
#sex
deno_test_w4$sex <- as.factor(deno_test_w4$sex); summary(deno_test_w4$sex);levels(deno_test_w4$sex)
deno_test_w4$sex <- recode_factor(deno_test_w4$sex, '1'="male",'2'="female")
#hb
deno_test_w4$hb2019 <-as.factor(deno_test_w4$hb2019);levels(deno_test_w4$hb2019); summary(deno_test_w4$hb2019)
deno_test_w4$hb2019 <- recode_factor(deno_test_w4$hb2019, "S08000015" ="Ayrshire and Arran",
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
                                     "S08000032" = "Lanarkshire"); summary(deno_test_w4$hb2019)
#Age 
summary(deno_test_w4$age_5y)
#=========================================================
ct4<- tabyl(deno_test_w4,covidtest_event, ethnicgrp_cen_phs)
ct4


coxhb_test_w4 <- coxph(Surv(covidtest_fu, covidtest_event) ~ ethnicgrp_cen_phs + age_5y + sex + hb2019, data = deno_test_w4)
summary(coxhb_test_w4)
#export output
coxhb_test_w4_out<- capture.output(publish(coxhb_test_w4, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_test_w4_out)),collapse="\n"),file="coxhb_test_w4_out.csv")
#=================================================================================================
# AGE AND SEX 
#===================================================================================================
setwd("")
#========================================
#wave 1

deno_test_w1 <- coxph(Surv(covidtest_fu, covidtest_event) ~ ethnicgrp_cen_phs + age_5y + sex, data = deno_test_w1)
summary(deno_test_w1)
#export output
deno_test_w1_out<- capture.output(publish(deno_test_w1, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",deno_test_w1_out)),collapse="\n"),file="deno_test_w1_out.csv")
#======================================================

#Wave 2
deno_test_w2 <- coxph(Surv(covidtest_fu, covidtest_event) ~ ethnicgrp_cen_phs + age_5y + sex, data = deno_test_w2)
summary(deno_test_w2)
#export output
deno_test_w2_out<- capture.output(publish(deno_test_w2, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",deno_test_w2_out)),collapse="\n"),file="deno_test_w2_out.csv")

#========================================================

#Wave 3
deno_test_w3 <- coxph(Surv(covidtest_fu, covidtest_event) ~ ethnicgrp_cen_phs + age_5y + sex, data = deno_test_w3)
summary(deno_test_w3)
#export output
deno_test_w3_out<- capture.output(publish(deno_test_w3, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",deno_test_w3_out)),collapse="\n"),file="deno_test_w3_out.csv")
#==========================================================

#Wave 4
deno_test_w4 <- coxph(Surv(covidtest_fu, covidtest_event) ~ ethnicgrp_cen_phs + age_5y + sex, data = deno_test_w4)
summary(deno_test_w4)
#export output
deno_test_w4_out<- capture.output(publish(deno_test_w4, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",deno_test_w4_out)),collapse="\n"),file="deno_test_w4_out.csv")


#=========================================================================================================
#No adjustment 
#=========================================
setwd("")
#===============================================
#wave 1

deno_test_w1 <- coxph(Surv(covidtest_fu, covidtest_event) ~ ethnicgrp_cen_phs, data = deno_test_w1)
summary(deno_test_w1)
#export output
deno_test_w1_out<- capture.output(publish(deno_test_w1, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",deno_test_w1_out)),collapse="\n"),file="deno_test_w1_out.csv")
#======================================================

#Wave 2
deno_test_w2 <- coxph(Surv(covidtest_fu, covidtest_event) ~ ethnicgrp_cen_phs , data = deno_test_w2)
summary(deno_test_w2)
#export output
deno_test_w2_out<- capture.output(publish(deno_test_w2, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",deno_test_w2_out)),collapse="\n"),file="deno_test_w2_out.csv")

#========================================================

#Wave 3
deno_test_w3 <- coxph(Surv(covidtest_fu, covidtest_event) ~ ethnicgrp_cen_phs , data = deno_test_w3)
summary(deno_test_w3)
#export output
deno_test_w3_out<- capture.output(publish(deno_test_w3, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",deno_test_w3_out)),collapse="\n"),file="deno_test_w3_out.csv")
#==========================================================

#Wave 4
deno_test_w4 <- coxph(Surv(covidtest_fu, covidtest_event) ~ ethnicgrp_cen_phs , data = deno_test_w4)
summary(deno_test_w4)
#export output
deno_test_w4_out<- capture.output(publish(deno_test_w4, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",deno_test_w4_out)),collapse="\n"),file="deno_test_w4_out.csv")


##############################################################################
#HOSPITALISATION cox models - adjusted for age, sex, and health board 
##############################################################################
load(file = "deno_hosp_w1.RData")
load(file = "deno_hosp_w2.RData")
load(file = "deno_hosp_w3.RData")
load(file = "deno_hosp_w4.RData")
#=================================================================================
#wave 1

#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_hosp_w1$ethnicgrp_cen_phs <- recode_factor(deno_hosp_w1$ethnicgrp_cen_phs, White = "White", 'Mixed or multiple ethnic groups'= "Mixed or multiple ethnic groups", Asian ="Asian", 'African, Caribbean or Black'="African Caribbean or Black", 'Other Ethnic Group'="Other Ethnic Group")
levels(deno_hosp_w1$ethnicgrp_cen_phs); summary(deno_hosp_w1$ethnicgrp_cen_phs)

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
ct5<- tabyl(deno_hosp_w1,covidhosp_event, ethnicgrp_cen_phs)
ct5

coxhb_hosp_w1 <- coxph(Surv(covidhosp_fu, covidhosp_event) ~ ethnicgrp_cen_phs + age_5y + sex + hb2019, data = deno_hosp_w1)
summary(coxhb_hosp_w1)
#export output
coxhb_hosp_w1_out<- capture.output(publish(coxhb_hosp_w1, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosp_w1_out)),collapse="\n"),file="coxhb_hosp_w1_out.csv")
#===========================================================================================================

#wave 2

#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_hosp_w2$ethnicgrp_cen_phs <- recode_factor(deno_hosp_w2$ethnicgrp_cen_phs, White = "White", 'Mixed or multiple ethnic groups'= "Mixed or multiple ethnic groups", Asian ="Asian", 'African, Caribbean or Black'="African Caribbean or Black", 'Other Ethnic Group'="Other Ethnic Group")
levels(deno_hosp_w2$ethnicgrp_cen_phs); summary(deno_hosp_w2$ethnicgrp_cen_phs)

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
ct6<- tabyl(deno_hosp_w2,covidhosp_event, ethnicgrp_cen_phs)
ct6

coxhb_hosp_w2 <- coxph(Surv(covidhosp_fu, covidhosp_event) ~ ethnicgrp_cen_phs + age_5y + sex + hb2019, data = deno_hosp_w2)
summary(coxhb_hosp_w2)
#export output
coxhb_hosp_w2_out<- capture.output(publish(coxhb_hosp_w2, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosp_w2_out)),collapse="\n"),file="coxhb_hosp_w2_out.csv")

#=================================================================================================

#wave 3

#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_hosp_w3$ethnicgrp_cen_phs <- recode_factor(deno_hosp_w3$ethnicgrp_cen_phs, White = "White", 'Mixed or multiple ethnic groups'= "Mixed or multiple ethnic groups", Asian ="Asian", 'African, Caribbean or Black'="African Caribbean or Black", 'Other Ethnic Group'="Other Ethnic Group")
levels(deno_hosp_w3$ethnicgrp_cen_phs); summary(deno_hosp_w3$ethnicgrp_cen_phs)

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
ct7<- tabyl(deno_hosp_w3,covidhosp_event, ethnicgrp_cen_phs)
ct7

coxhb_hosp_w3 <- coxph(Surv(covidhosp_fu, covidhosp_event) ~ ethnicgrp_cen_phs + age_5y + sex + hb2019, data = deno_hosp_w3)
summary(coxhb_hosp_w3)
#export output
coxhb_hosp_w3_out<- capture.output(publish(coxhb_hosp_w3, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosp_w3_out)),collapse="\n"),file="coxhb_hosp_w3_out.csv")

#================================================================================================================
#wave 4

#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_hosp_w4$ethnicgrp_cen_phs <- recode_factor(deno_hosp_w4$ethnicgrp_cen_phs, White = "White", 'Mixed or multiple ethnic groups'= "Mixed or multiple ethnic groups", Asian ="Asian", 'African, Caribbean or Black'="African Caribbean or Black", 'Other Ethnic Group'="Other Ethnic Group")
levels(deno_hosp_w4$ethnicgrp_cen_phs); summary(deno_hosp_w4$ethnicgrp_cen_phs)

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
ct8<- tabyl(deno_hosp_w4,covidhosp_event, ethnicgrp_cen_phs)
ct8

coxhb_hosp_w4 <- coxph(Surv(covidhosp_fu, covidhosp_event) ~ ethnicgrp_cen_phs + age_5y + sex + hb2019, data = deno_hosp_w4)
summary(coxhb_hosp_w4)
#export output
coxhb_hosp_w4_out<- capture.output(publish(coxhb_hosp_w4, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosp_w4_out)),collapse="\n"),file="coxhb_hosp_w4_out.csv")

#=================================================================================================
# AGE AND SEX 
#===================================================================================================
#wave 1
#=========================================
coxhb_hosp_w1 <- coxph(Surv(covidhosp_fu, covidhosp_event) ~ ethnicgrp_cen_phs + age_5y + sex, data = deno_hosp_w1)
summary(coxhb_hosp_w1)
#export output
coxhb_hosp_w1_out<- capture.output(publish(coxhb_hosp_w1, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosp_w1_out)),collapse="\n"),file="coxhb_hosp_w1_out.csv")
#======================================================

#Wave 2
coxhb_hosp_w2 <- coxph(Surv(covidhosp_fu, covidhosp_event) ~ ethnicgrp_cen_phs + age_5y + sex, data = deno_hosp_w2)
summary(coxhb_hosp_w2)
#export output
coxhb_hosp_w2_out<- capture.output(publish(coxhb_hosp_w2, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosp_w2_out)),collapse="\n"),file="coxhb_hosp_w2_out.csv")

#========================================================
#Wave 3
coxhb_hosp_w3 <- coxph(Surv(covidhosp_fu, covidhosp_event) ~ ethnicgrp_cen_phs + age_5y + sex, data = deno_hosp_w3)
summary(coxhb_hosp_w3)
#export output
coxhb_hosp_w3_out<- capture.output(publish(coxhb_hosp_w3, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosp_w3_out)),collapse="\n"),file="coxhb_hosp_w3_out.csv")
#==========================================================

#Wave 4
coxhb_hosp_w4 <- coxph(Surv(covidhosp_fu, covidhosp_event) ~ ethnicgrp_cen_phs + age_5y + sex, data = deno_hosp_w4)
summary(coxhb_hosp_w4)
#export output
coxhb_hosp_w4_out<- capture.output(publish(coxhb_hosp_w4, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosp_w4_out)),collapse="\n"),file="coxhb_hosp_w4_out.csv")

#=========================================================================================================
#No adjustment 
#=========================================
#wave 1
#=========================================
coxhb_hosp_w1 <- coxph(Surv(covidhosp_fu, covidhosp_event) ~ ethnicgrp_cen_phs , data = deno_hosp_w1)
summary(coxhb_hosp_w1)
#export output
coxhb_hosp_w1_out<- capture.output(publish(coxhb_hosp_w1, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosp_w1_out)),collapse="\n"),file="coxhb_hosp_w1_out.csv")
#======================================================

#Wave 2
coxhb_hosp_w2 <- coxph(Surv(covidhosp_fu, covidhosp_event) ~ ethnicgrp_cen_phs , data = deno_hosp_w2)
summary(coxhb_hosp_w2)
#export output
coxhb_hosp_w2_out<- capture.output(publish(coxhb_hosp_w2, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosp_w2_out)),collapse="\n"),file="coxhb_hosp_w2_out.csv")

#========================================================

#Wave 3
coxhb_hosp_w3 <- coxph(Surv(covidhosp_fu, covidhosp_event) ~ ethnicgrp_cen_phs , data = deno_hosp_w3)
summary(coxhb_hosp_w3)
#export output
coxhb_hosp_w3_out<- capture.output(publish(coxhb_hosp_w3, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosp_w3_out)),collapse="\n"),file="coxhb_hosp_w3_out.csv")
#==========================================================

#Wave 4
coxhb_hosp_w4 <- coxph(Surv(covidhosp_fu, covidhosp_event) ~ ethnicgrp_cen_phs , data = deno_hosp_w4)
summary(coxhb_hosp_w4)
#export output
coxhb_hosp_w4_out<- capture.output(publish(coxhb_hosp_w4, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosp_w4_out)),collapse="\n"),file="coxhb_hosp_w4_out.csv")



##############################################################################
#DEATH cox models - adjusted for age, sex, and health board 
##############################################################################

setwd("")

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
#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_death_w1$ethnicgrp_cen_phs <- recode_factor(deno_death_w1$ethnicgrp_cen_phs, White = "White", 'Mixed or multiple ethnic groups'= "Mixed or multiple ethnic groups", Asian ="Asian", 'African, Caribbean or Black'="African Caribbean or Black", 'Other Ethnic Group'="Other Ethnic Group")
levels(deno_death_w1$ethnicgrp_cen_phs); summary(deno_death_w1$ethnicgrp_cen_phs)

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
ct9<- tabyl(deno_death_w1,coviddeath_event, ethnicgrp_cen_phs)
ct9

coxhb_death_w1 <- coxph(Surv(coviddeath_fu, coviddeath_event) ~ ethnicgrp_cen_phs + age_5y + sex + hb2019, data = deno_death_w1)
summary(coxhb_death_w1)
#export output
coxhb_death_w1_out<- capture.output(publish(coxhb_death_w1, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_death_w1_out)),collapse="\n"),file="coxhb_death_w1_out.csv")
#=====================================================================================================================
#wave 2
#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_death_w2$ethnicgrp_cen_phs <- recode_factor(deno_death_w2$ethnicgrp_cen_phs, White = "White", 'Mixed or multiple ethnic groups'= "Mixed or multiple ethnic groups", Asian ="Asian", 'African, Caribbean or Black'="African Caribbean or Black", 'Other Ethnic Group'="Other Ethnic Group")
levels(deno_death_w2$ethnicgrp_cen_phs); summary(deno_death_w2$ethnicgrp_cen_phs)

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
ct10<- tabyl(deno_death_w2,coviddeath_event, ethnicgrp_cen_phs)
ct10

coxhb_death_w2 <- coxph(Surv(coviddeath_fu, coviddeath_event) ~ ethnicgrp_cen_phs + age_5y + sex + hb2019, data = deno_death_w2)
summary(coxhb_death_w2)
#export output
coxhb_death_w2_out<- capture.output(publish(coxhb_death_w2, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_death_w2_out)),collapse="\n"),file="coxhb_death_w2_out.csv")
#============================================================================
#wave 3

#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_death_w3$ethnicgrp_cen_phs <- recode_factor(deno_death_w3$ethnicgrp_cen_phs, White = "White", 'Mixed or multiple ethnic groups'= "Mixed or multiple ethnic groups", Asian ="Asian", 'African, Caribbean or Black'="African Caribbean or Black", 'Other Ethnic Group'="Other Ethnic Group")
levels(deno_death_w3$ethnicgrp_cen_phs); summary(deno_death_w3$ethnicgrp_cen_phs)

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
ct11<- tabyl(deno_death_w3,coviddeath_event, ethnicgrp_cen_phs)
ct11

coxhb_death_w3 <- coxph(Surv(coviddeath_fu, coviddeath_event) ~ ethnicgrp_cen_phs + age_5y + sex + hb2019, data = deno_death_w3)
summary(coxhb_death_w3)
#export output
coxhb_death_w3_out<- capture.output(publish(coxhb_death_w3, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_death_w3_out)),collapse="\n"),file="coxhb_death_w3_out.csv")
#====================================================================================

#wave 4

#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_death_w4$ethnicgrp_cen_phs <- recode_factor(deno_death_w4$ethnicgrp_cen_phs, White = "White", 'Mixed or multiple ethnic groups'= "Mixed or multiple ethnic groups", Asian ="Asian", 'African, Caribbean or Black'="African Caribbean or Black", 'Other Ethnic Group'="Other Ethnic Group")
levels(deno_death_w4$ethnicgrp_cen_phs); summary(deno_death_w4$ethnicgrp_cen_phs)

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
ct12<- tabyl(deno_death_w4,coviddeath_event, ethnicgrp_cen_phs)
ct12

coxhb_death_w4 <- coxph(Surv(coviddeath_fu, coviddeath_event) ~ ethnicgrp_cen_phs + age_5y + sex + hb2019, data = deno_death_w4)
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

coxhb_death_w1 <- coxph(Surv(coviddeath_fu, coviddeath_event) ~ ethnicgrp_cen_phs + age_5y + sex, data = deno_death_w1)
summary(coxhb_death_w1)
#export output
coxhb_death_w1_out<- capture.output(publish(coxhb_death_w1, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_death_w1_out)),collapse="\n"),file="coxhb_death_w1_out.csv")
#======================================================

#Wave 2
coxhb_death_w2 <- coxph(Surv(coviddeath_fu, coviddeath_event) ~ ethnicgrp_cen_phs + age_5y + sex, data = deno_death_w2)
summary(coxhb_death_w2)
#export output
coxhb_death_w2_out<- capture.output(publish(coxhb_death_w2, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_death_w2_out)),collapse="\n"),file="coxhb_death_w2_out.csv")

#========================================================

#Wave 3
coxhb_death_w3 <- coxph(Surv(coviddeath_fu, coviddeath_event) ~ ethnicgrp_cen_phs + age_5y + sex, data = deno_death_w3)
summary(coxhb_death_w3)
#export output
coxhb_death_w3_out<- capture.output(publish(coxhb_death_w3, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_death_w3_out)),collapse="\n"),file="coxhb_death_w3_out.csv")
#==========================================================

#Wave 4
coxhb_death_w4 <- coxph(Surv(coviddeath_fu, coviddeath_event) ~ ethnicgrp_cen_phs + age_5y + sex, data = deno_death_w4)
summary(coxhb_death_w4)
#export output
coxhb_death_w4_out<- capture.output(publish(coxhb_death_w4, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_death_w4_out)),collapse="\n"),file="coxhb_death_w4_out.csv")

#=========================================================================================================

#===============================================================================================================
#No adjustment
#======================================================================
setwd("")
#=======================================================
#wave 1

coxhb_death_w1 <- coxph(Surv(coviddeath_fu, coviddeath_event) ~ ethnicgrp_cen_phs , data = deno_death_w1)
summary(coxhb_death_w1)
#export output
coxhb_death_w1_out<- capture.output(publish(coxhb_death_w1, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_death_w1_out)),collapse="\n"),file="coxhb_death_w1_out.csv")
#======================================================

#Wave 2
coxhb_death_w2 <- coxph(Surv(coviddeath_fu, coviddeath_event) ~ ethnicgrp_cen_phs , data = deno_death_w2)
summary(coxhb_death_w2)
#export output
coxhb_death_w2_out<- capture.output(publish(coxhb_death_w2, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_death_w2_out)),collapse="\n"),file="coxhb_death_w2_out.csv")

#========================================================

#Wave 3
coxhb_death_w3 <- coxph(Surv(coviddeath_fu, coviddeath_event) ~ ethnicgrp_cen_phs , data = deno_death_w3)
summary(coxhb_death_w3)
#export output
coxhb_death_w3_out<- capture.output(publish(coxhb_death_w3, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_death_w3_out)),collapse="\n"),file="coxhb_death_w3_out.csv")
#==========================================================

#Wave 4
coxhb_death_w4 <- coxph(Surv(coviddeath_fu, coviddeath_event) ~ ethnicgrp_cen_phs , data = deno_death_w4)
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
setwd("")
#================================================================
#wave 1
#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_hosdea_w1$ethnicgrp_cen_phs <- recode_factor(deno_hosdea_w1$ethnicgrp_cen_phs, White = "White", 'Mixed or multiple ethnic groups'= "Mixed or multiple ethnic groups", Asian ="Asian", 'African, Caribbean or Black'="African Caribbean or Black", 'Other Ethnic Group'="Other Ethnic Group")
levels(deno_hosdea_w1$ethnicgrp_cen_phs); summary(deno_hosdea_w1$ethnicgrp_cen_phs)

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

#=================================
ct1<- tabyl(deno_hosdea_w1,covidhosdea_event, ethnicgrp_cen_phs)
ct1

coxhb_hosdea_w1 <- coxph(Surv(covidhosdea_fu, covidhosdea_event) ~ ethnicgrp_cen_phs + age_5y + sex + hb2019, data = deno_hosdea_w1)
summary(coxhb_hosdea_w1)
#export output
coxhb_hosdea_w1_out<- capture.output(publish(coxhb_hosdea_w1, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosdea_w1_out)),collapse="\n"),file="coxhb_hosdea_w1_out.csv")
#======================================================================================================
#wave 2

#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_hosdea_w2$ethnicgrp_cen_phs <- recode_factor(deno_hosdea_w2$ethnicgrp_cen_phs, White = "White", 'Mixed or multiple ethnic groups'= "Mixed or multiple ethnic groups", Asian ="Asian", 'African, Caribbean or Black'="African Caribbean or Black", 'Other Ethnic Group'="Other Ethnic Group")
levels(deno_hosdea_w2$ethnicgrp_cen_phs); summary(deno_hosdea_w2$ethnicgrp_cen_phs)

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
ct2<- tabyl(deno_hosdea_w2,covidhosdea_event, ethnicgrp_cen_phs)
ct2

coxhb_hosdea_w2 <- coxph(Surv(covidhosdea_fu, covidhosdea_event) ~ ethnicgrp_cen_phs + age_5y + sex + hb2019, data = deno_hosdea_w2)
summary(coxhb_hosdea_w2)
#export output
coxhb_hosdea_w2_out<- capture.output(publish(coxhb_hosdea_w2, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosdea_w2_out)),collapse="\n"),file="coxhb_hosdea_w2_out.csv")

#========================================================================================
#wave 3
#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_hosdea_w3$ethnicgrp_cen_phs <- recode_factor(deno_hosdea_w3$ethnicgrp_cen_phs, White = "White", 'Mixed or multiple ethnic groups'= "Mixed or multiple ethnic groups", Asian ="Asian", 'African, Caribbean or Black'="African Caribbean or Black", 'Other Ethnic Group'="Other Ethnic Group")
levels(deno_hosdea_w3$ethnicgrp_cen_phs); summary(deno_hosdea_w3$ethnicgrp_cen_phs)

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
#=====================================
ct3<- tabyl(deno_hosdea_w3,covidhosdea_event, ethnicgrp_cen_phs)
ct3

coxhb_hosdea_w3 <- coxph(Surv(covidhosdea_fu, covidhosdea_event) ~ ethnicgrp_cen_phs + age_5y + sex + hb2019, data = deno_hosdea_w3)
summary(coxhb_hosdea_w3)
#========================
#export output
coxhb_hosdea_w3_out<- capture.output(publish(coxhb_hosdea_w3, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosdea_w3_out)),collapse="\n"),file="coxhb_hosdea_w3_out.csv")
#===========================================

#wave 4
#Remove comma in category = "African, Caribbean or Black"  to enable .csv export
deno_hosdea_w4$ethnicgrp_cen_phs <- recode_factor(deno_hosdea_w4$ethnicgrp_cen_phs, White = "White", 'Mixed or multiple ethnic groups'= "Mixed or multiple ethnic groups", Asian ="Asian", 'African, Caribbean or Black'="African Caribbean or Black", 'Other Ethnic Group'="Other Ethnic Group")
levels(deno_hosdea_w4$ethnicgrp_cen_phs); summary(deno_hosdea_w4$ethnicgrp_cen_phs)

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
#===============================================
ct4<- tabyl(deno_hosdea_w4,covidhosdea_event, ethnicgrp_cen_phs)
ct4


coxhb_hosdea_w4 <- coxph(Surv(covidhosdea_fu, covidhosdea_event) ~ ethnicgrp_cen_phs + age_5y + sex + hb2019, data = deno_hosdea_w4)
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
coxhb_hosdea_w1 <- coxph(Surv(covidhosdea_fu, covidhosdea_event) ~ ethnicgrp_cen_phs + age_5y + sex, data = deno_hosdea_w1)
summary(coxhb_hosdea_w1)
#export output
coxhb_hosdea_w1_out<- capture.output(publish(coxhb_hosdea_w1, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosdea_w1_out)),collapse="\n"),file="coxhb_hosdea_w1_out.csv")

#======================================================

#Wave 2
coxhb_hosdea_w2 <- coxph(Surv(covidhosdea_fu, covidhosdea_event) ~ ethnicgrp_cen_phs + age_5y + sex, data = deno_hosdea_w2)
summary(coxhb_hosdea_w2)
#export output
coxhb_hosdea_w2_out<- capture.output(publish(coxhb_hosdea_w2, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosdea_w2_out)),collapse="\n"),file="coxhb_hosdea_w2_out.csv")

#========================================================

#Wave 3
coxhb_hosdea_w3 <- coxph(Surv(covidhosdea_fu, covidhosdea_event) ~ ethnicgrp_cen_phs + age_5y + sex, data = deno_hosdea_w3)
summary(coxhb_hosdea_w3)
#========================
#export output
coxhb_hosdea_w3_out<- capture.output(publish(coxhb_hosdea_w3, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosdea_w3_out)),collapse="\n"),file="coxhb_hosdea_w3_out.csv")
#==========================================================

#Wave 4
coxhb_hosdea_w4 <- coxph(Surv(covidhosdea_fu, covidhosdea_event) ~ ethnicgrp_cen_phs + age_5y + sex, data = deno_hosdea_w4)
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
coxhb_hosdea_w1 <- coxph(Surv(covidhosdea_fu, covidhosdea_event) ~ ethnicgrp_cen_phs , data = deno_hosdea_w1)
summary(coxhb_hosdea_w1)
#export output
coxhb_hosdea_w1_out<- capture.output(publish(coxhb_hosdea_w1, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosdea_w1_out)),collapse="\n"),file="coxhb_hosdea_w1_out.csv")

#======================================================

#Wave 2
coxhb_hosdea_w2 <- coxph(Surv(covidhosdea_fu, covidhosdea_event) ~ ethnicgrp_cen_phs , data = deno_hosdea_w2)
summary(coxhb_hosdea_w2)
#export output
coxhb_hosdea_w2_out<- capture.output(publish(coxhb_hosdea_w2, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosdea_w2_out)),collapse="\n"),file="coxhb_hosdea_w2_out.csv")

#========================================================

#Wave 3
coxhb_hosdea_w3 <- coxph(Surv(covidhosdea_fu, covidhosdea_event) ~ ethnicgrp_cen_phs , data = deno_hosdea_w3)
summary(coxhb_hosdea_w3)
#========================
#export output
coxhb_hosdea_w3_out<- capture.output(publish(coxhb_hosdea_w3, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosdea_w3_out)),collapse="\n"),file="coxhb_hosdea_w3_out.csv")
#==========================================================

#Wave 4
coxhb_hosdea_w4 <- coxph(Surv(covidhosdea_fu, covidhosdea_event) ~ ethnicgrp_cen_phs , data = deno_hosdea_w4)
summary(coxhb_hosdea_w4)
#========================
#export output
coxhb_hosdea_w4_out<- capture.output(publish(coxhb_hosdea_w4, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",coxhb_hosdea_w4_out)),collapse="\n"),file="coxhb_hosdea_w4_out.csv")

#===================================================================================================================
#==================================================================================================================

#================================================================================================================================
#=================================================================================================================================
#prog post infection
setwd("")

#===============================
#hosp/death
cox_progi_hosdea <- coxph(Surv(progi_hosdea_fu, progi_hosdea) ~ ethnicgrp_cen_phs + age_5y + sex + hb2019, data = cenprog_inf1)
summary(cox_progi_hosdea)
#export output
cox_progi_hosdea_out<- capture.output(publish(cox_progi_hosdea, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",cox_progi_hosdea_out)),collapse="\n"),file="cox_progi_hosdea_out.csv")

#======================================
#hosp
cox_progi_hosp <- coxph(Surv(progi_hosp_fu, progi_hosp) ~ ethnicgrp_cen_phs + age_5y + sex + hb2019, data = cenprog_inf1)
summary(cox_progi_hosp)
#export output
cox_progi_hosp_out<- capture.output(publish(cox_progi_hosp, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",cox_progi_hosp_out)),collapse="\n"),file="cox_progi_hosp_out.csv")

#=====================================
#death
cox_progi_death <- coxph(Surv(progi_death_fu, progi_death) ~ ethnicgrp_cen_phs + age_5y + sex + hb2019, data = cenprog_inf1)
summary(cox_progi_death)
#export output
cox_progi_death_out<- capture.output(publish(cox_progi_death, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",cox_progi_death_out)),collapse="\n"),file="cox_progi_death_out.csv")


#===============================================================================================================================
#===============================================================================================================================
#prog post hosp

setwd("")
#================================

#death
cox_progh_death <- coxph(Surv(progh_death_fu, progh_death) ~ ethnicgrp_cen_phs + age_5y + sex + hb2019, data = cenprog_hosp1)
summary(cox_progh_death)
#export output
cox_progh_death_out<- capture.output(publish(cox_progh_death, org=TRUE))
cat(paste(gsub("^,|,$","",gsub("[|]",",",cox_progh_death_out)),collapse="\n"),file="cox_progh_death_out.csv")

#=====================










