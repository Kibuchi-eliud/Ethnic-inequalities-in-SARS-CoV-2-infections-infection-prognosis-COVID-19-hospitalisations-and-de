#==============================================================================
  #Forest plots for COVID -19 and ethnicity outcomes paper 
  #Eliud Kibuchi
  #28 September 2022
  #=============================================================================
rm(list=ls())

#set working directory
setwd("N:/Mediation analysis/outcomes paper/forest_plots")
list.files()

library(ggplot2)
library(gridExtra)
library(dplyr)
library(reshape2)
library(ggridges)
library(patchwork)
library(cowplot)

data_full_d<-read.csv("N:/Mediation analysis/outcomes paper/forest_plots/Covid19_ethnic by waves_300123_update.csv",stringsAsFactors = F)
str(data_full_d)
data_full_d$HR <- as.numeric(data_full_d$HR)
names(data_full_d)
data_full_d$ethnic_grp<-as.factor(data_full_d$ethnic_grp);levels(data_full_d$ethnic_grp)
data_full_d$eth<-factor(data_full_d$ethnic_grp,levels=c("White","White Scottish",
                                                        "White British",
                                                        "White Irish",
                                                        "White Gypsy",
                                                        "White Polish",
                                                        "Other White",
                                                        "Mixed",
                                                        "Mixed-disagg",
                                                        "Asian",
                                                        "Pakistani",
                                                        "Indian",
                                                        "Bangladeshi",
                                                        "Chinese",
                                                        "Other Asian","Black","African",
                                                        "Caribbean ","Other",
                                                        "Arab","Other-disagg"),ordered = T)

#=======
#Remove "other group" - The “all other ethnic group” category was removed due to the difficulty of interpreting results for this heterogeneous group.
#Make aggregate ethnic groups bold
#levels(data_full_d$eth); summary(data_full_d$eth)
#ethnic_report<- c("White","White Scottish",
                  #"White British",
                  #"White Irish",
                  #"White Gypsy",
                  #"White Polish",
                 # "Other White",
#Mixed",
                 # "Mixed-disagg",
                  #"Asian",
                 # "Pakistani",
                 # "Indian",
                  #"Bangladeshi",
                 # "Chinese",
                  #"Other Asian","Black","African",
                  #"Caribbean ",
                  #"Arab")

#data_full<- data_full_d%>% filter(eth%in%ethnic_report);summary(data_full$eth)
#data_full$eth<-factor(data_full$eth);levels(data_full$eth);summary(data_full$eth)

#=======
data_full <- data_full_d
#Make aggregate ethnic groups bold
bold_ethnic <- c("White", "Mixed","Asian","Black","Other")
bold_eth_labels <- ifelse(levels(data_full$eth) %in% bold_ethnic, yes = "bold", no = "plain")

data_full <- replace(data_full, is.na(data_full), "")

print(data_full)

#Combine observations and events 
data_full$Obs_events<- paste(data_full$observations, "|", data_full$events)
data_full$bold_Obs_events<-ifelse(levels(data_full$eth) %in% bold_ethnic, yes = "bold", no = "plain")
data_full$bold_HR_CI<- ifelse(levels(data_full$eth) %in% bold_ethnic, yes = "bold", no = "plain")

data_full$lower <- as.numeric(data_full$lower)
data_full$upper <- as.numeric(data_full$upper)


#==========================================================================================
##==========================================================================================
#=========================================================================================
                         #Hospitalisations and deaths
#============================================================================================
datafull_hspdth_wv1 <- subset(data_full, Covid19_outcome=="Hospitalisation and death"& Wave==1); head(datafull_hspdth_wv1 )
names(datafull_hspdth_wv1)

#hazard ratio and error bars for hospitalizations and deaths wave 1
datafull_hspdth_wv1$HR <- as.numeric(datafull_hspdth_wv1$HR)

P_hspdthwv1<-ggplot(datafull_hspdth_wv1, aes(x=HR,y=eth))+
  geom_point(shape = 18, size = 5)+
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25)+
  labs(title="Wave 1",
       y="Ethnicity",tag="A")+
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.0, size=14,face="bold"),
        axis.text.y = element_text(face = rev(bold_eth_labels),size=12),
        plot.tag = element_text(size = 25,face="bold"),
        plot.tag.position = c(0.1, 1),
        legend.title = element_text(color = "blue", size = 10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 22),
        #axis.line.x.bottom=element_line(size = 0.5, colour = "black", linetype=1),
        #axis.line.x.top=element_line(size = 1.5, colour = "black", linetype=1))+
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(expand = c(0, 0))+ #limits = c(0, 16.0)
  theme(plot.margin = unit(c(1,0,1,0), "cm"))+
  guides(colour = guide_legend(title.hjust = 0.5))

P_hspdthwv1

## hazard ratio and CI values
P2_hspdthwv1<- ggplot(datafull_hspdth_wv1, aes(y=eth)) +
  geom_text(aes(y = (eth), x = 1, label = HR_CI,fontface = bold_HR_CI),size = 4) + 
   ggtitle("HR[95%CI]")+
  theme_minimal()+
  theme(plot.margin = unit(c(0,0,0,0),'cm'),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits=rev)+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P2_hspdthwv1
#========================================================================
#Wave 2
#=================

datafull_hspdth_wv2<- subset(data_full, Covid19_outcome=="Hospitalisation and death"& Wave==2); head(datafull_hspdth_wv2)
names(datafull_hspdth_wv2)

#hazard ratio and error bars for hospitalizations and deaths wave 1
datafull_hspdth_wv2$HR <- as.numeric(datafull_hspdth_wv2$HR)

P_hspdthwv2<-ggplot(datafull_hspdth_wv2, aes(x=HR,y=eth))+
  geom_point(shape = 18, size = 5)+
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25)+
  labs(title="Wave 2")+
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.0, size=14,face="bold"),
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y.left = element_line(colour = "black"),
        #axis.line.x.bottom=element_line(size = 0.5, colour = "black", linetype=1),
        #axis.line.x.top=element_line(size = 1.5, colour = "black", linetype=1))+
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(expand = c(0, 0))+ #limits = c(0, 16.0)
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P_hspdthwv2

## hazard ratio and CI values
P2_hspdthwv2<- ggplot(datafull_hspdth_wv2, aes(y=eth)) +
  geom_text(aes(y = (eth), x = 1, label = HR_CI,fontface = bold_HR_CI),size = 4) + 
  ggtitle("HR[95%CI]")+
  theme_minimal()+
  theme(plot.margin = unit(c(0,0,0,0),'cm'),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits=rev)+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P2_hspdthwv2
#=======================================
#Wave 3
#=================
datafull_hspdth_wv3<- subset(data_full, Covid19_outcome=="Hospitalisation and death"& Wave==3); head(datafull_hspdth_wv3)
names(datafull_hspdth_wv3)

#hazard ratio and error bars for hospitalizations and deaths wave 1
datafull_hspdth_wv3$HR <- as.numeric(datafull_hspdth_wv3$HR)

P_hspdthwv3<-ggplot(datafull_hspdth_wv3, aes(x=HR,y=eth))+
  geom_point(shape = 18, size = 5)+
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25)+
  labs(title="Wave 3")+
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.0, size=14,face="bold"),
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y.left = element_line(colour = "black"),
       # axis.line.x.bottom=element_line(size = 0.5, colour = "black", linetype=1),
        #axis.line.x.top=element_line(size = 1.5, colour = "black", linetype=1))+
       axis.text.x=element_blank(),
       axis.ticks.x=element_blank())+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(limits = c(0, 16.0),expand = c(0, 0))+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P_hspdthwv3

## hazard ratio and CI values
P2_hspdthwv3<- ggplot(datafull_hspdth_wv3, aes(y=eth)) +
  geom_text(aes(y = (eth), x = 1, label = HR_CI,fontface = bold_HR_CI),size = 4) + 
  ggtitle("HR[95%CI]")+
  theme_minimal()+
  theme(plot.margin = unit(c(0,0,0,0),'cm'),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits=rev)+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P2_hspdthwv3

#==========================
#Wave 4
datafull_hspdth_wv4<- subset(data_full, Covid19_outcome=="Hospitalisation and death"& Wave==4); head(datafull_hspdth_wv4)
names(datafull_hspdth_wv4)

#hazard ratio and error bars for hospitalizations and deaths wave 1
datafull_hspdth_wv4$HR <- as.numeric(datafull_hspdth_wv4$HR)

P_hspdthwv4<-ggplot(datafull_hspdth_wv4, aes(x=HR,y=eth))+
  geom_point(shape = 18, size = 5)+
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25)+
  labs(title="Wave 4")+
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.0, size=14,face="bold"),
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y.left = element_line(colour = "black"),
        #axis.line.x.bottom=element_line(size = 0.5, colour = "black", linetype=1),
        #axis.line.x.top=element_line(size = 1.5, colour = "black", linetype=1))+
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(limits = c(0, 16.0),expand = c(0, 0))+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P_hspdthwv4

## hazard ratio and CI values
P2_hspdthwv4<- ggplot(datafull_hspdth_wv4, aes(y=eth)) +
  geom_text(aes(y = (eth), x = 1, label = HR_CI,fontface = bold_HR_CI),size = 4) + 
  ggtitle("HR[95%CI]")+
  theme_minimal()+
  theme(plot.margin = unit(c(0,0,0,0),'cm'),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits=rev)+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P2_hspdthwv4

#=================================================================================
#Hospitalisations 
#============================
datafull_hsp_wv1 <- subset(data_full, Covid19_outcome=="Hospitalisation"& Wave==1); head(datafull_hsp_wv1 )
names(datafull_hsp_wv1)

#hazard ratio and error bars for hospitalizations and deaths wave 1
datafull_hsp_wv1$HR <- as.numeric(datafull_hsp_wv1$HR)

P_hspwv1<-ggplot(datafull_hsp_wv1, aes(x=HR,y=eth))+
  geom_point(shape = 18, size = 5)+
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25)+
  labs(title="Wave 1",
       y="Ethnicity",tag="B")+
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.0, size=14,face="bold"),
        axis.text.y = element_text(face = rev(bold_eth_labels),size=12),
        plot.tag = element_text(size = 25,face="bold"),
        plot.tag.position = c(0.1, 1),
        axis.ticks.y=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 22),
        #axis.line.x.bottom=element_line(size = 0.5, colour = "black", linetype=1),
        #axis.line.x.top=element_line(size = 1.5, colour = "black", linetype=1))+
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(expand = c(0, 0))+ #limits = c(0, 16.0),
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P_hspwv1

## hazard ratio and CI values
P2_hspwv1<- ggplot(datafull_hsp_wv1, aes(y=eth)) +
  geom_text(aes(y = (eth), x = 1, label = HR_CI,fontface = bold_HR_CI),size = 4) + 
  ggtitle("HR[95%CI]")+
  theme_minimal()+
  theme(plot.margin = unit(c(0,0,0,0),'cm'),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits=rev)+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P2_hspwv1
#========================================================================
#Wave 2
#=================

datafull_hsp_wv2<- subset(data_full, Covid19_outcome=="Hospitalisation"& Wave==2); head(datafull_hsp_wv2)
names(datafull_hsp_wv2)

#hazard ratio and error bars for hospitalizations and deaths wave 1
datafull_hsp_wv2$HR <- as.numeric(datafull_hsp_wv2$HR)

P_hspwv2<-ggplot(datafull_hsp_wv2, aes(x=HR,y=eth))+
  geom_point(shape = 18, size = 5)+
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25)+
  labs(title="Wave 2")+
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.0, size=14,face="bold"),
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y.left = element_line(colour = "black"),
        #axis.line.x.bottom=element_line(size = 0.5, colour = "black", linetype=1),
        #axis.line.x.top=element_line(size = 1.5, colour = "black", linetype=1))+
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(expand = c(0, 0))+ #limits = c(0, 16.0),
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P_hspwv2

## hazard ratio and CI values
P2_hspwv2<- ggplot(datafull_hsp_wv2, aes(y=eth)) +
  geom_text(aes(y = (eth), x = 1, label = HR_CI,fontface = bold_HR_CI),size = 4) + 
  ggtitle("HR[95%CI]")+
  theme_minimal()+
  theme(plot.margin = unit(c(0,0,0,0),'cm'),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits=rev)+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P2_hspwv2
#=======================================
#Wave 3
#=================
datafull_hsp_wv3<- subset(data_full, Covid19_outcome=="Hospitalisation"& Wave==3); head(datafull_hsp_wv3)
names(datafull_hsp_wv3)

#hazard ratio and error bars for hospitalizations and deaths wave 1
datafull_hsp_wv3$HR <- as.numeric(datafull_hsp_wv3$HR)

P_hspwv3<-ggplot(datafull_hsp_wv3, aes(x=HR,y=eth))+
  geom_point(shape = 18, size = 5)+
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25)+
  labs(title="Wave 3")+
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.0, size=14,face="bold"),
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y.left = element_line(colour = "black"),
        #axis.line.x.bottom=element_line(size = 0.5, colour = "black", linetype=1),
        #axis.line.x.top=element_line(size = 1.5, colour = "black", linetype=1))+
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(expand = c(0, 0))+ #limits = c(0, 16.0),
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P_hspwv3

## hazard ratio and CI values
P2_hspwv3<- ggplot(datafull_hsp_wv3, aes(y=eth)) +
  geom_text(aes(y = (eth), x = 1, label = HR_CI,fontface = bold_HR_CI),size = 4) + 
  ggtitle("HR[95%CI]")+
  theme_minimal()+
  theme(plot.margin = unit(c(0,0,0,0),'cm'),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits=rev)+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P2_hspwv3

#==========================
#Wave 4
datafull_hsp_wv4<- subset(data_full, Covid19_outcome=="Hospitalisation"& Wave==4); head(datafull_hsp_wv4)
names(datafull_hsp_wv4)

#hazard ratio and error bars for hospitalizations and deaths wave 1
datafull_hsp_wv4$HR <- as.numeric(datafull_hsp_wv4$HR)

P_hspwv4<-ggplot(datafull_hsp_wv4, aes(x=HR,y=eth))+
  geom_point(shape = 18, size = 5)+
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25)+
  labs(title="Wave 4")+
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.0, size=14,face="bold"),
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y.left = element_line(colour = "black"),
        #axis.line.x.bottom=element_line(size = 0.5, colour = "black", linetype=1),
        #axis.line.x.top=element_line(size = 1.5, colour = "black", linetype=1))+
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(expand = c(0, 0))+ #limits = c(0, 16.0),
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P_hspwv4

## hazard ratio and CI values
P2_hspwv4<- ggplot(datafull_hsp_wv4, aes(y=eth)) +
  geom_text(aes(y = (eth), x = 1, label = HR_CI,fontface = bold_HR_CI),size = 4) + 
  ggtitle("HR[95%CI]")+
  theme_minimal()+
  theme(plot.margin = unit(c(0,0,0,0),'cm'),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits=rev)+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P2_hspwv4

#===============================================================================
#Deaths 
#==================================================
datafull_dth_wv1 <- subset(data_full, Covid19_outcome=="Death"& Wave==1); head(datafull_dth_wv1 )
names(datafull_dth_wv1)

#hazard ratio and error bars for hospitalizations and deaths wave 1
datafull_dth_wv1$HR <- as.numeric(datafull_dth_wv1$HR)

P_dthwv1<-ggplot(datafull_dth_wv1, aes(x=HR,y=eth))+
  geom_point(shape = 18, size = 5)+
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25)+
  labs(title="Wave 1",
       y="Ethnicity",tag="C")+
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.0, size=14,face="bold"),
        axis.text.y = element_text(face = rev(bold_eth_labels),size=12),
        axis.ticks.y=element_blank(),
        plot.tag = element_text(size = 25,face="bold"),
        plot.tag.position = c(0.1, 1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 22),
        #axis.line.x.bottom=element_line(size = 0.5, colour = "black", linetype=1),
        #axis.line.x.top=element_line(size = 1.5, colour = "black", linetype=1))+
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(expand = c(0, 0))+ #limits = c(0, 16.0),
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P_dthwv1

## hazard ratio and CI values
P2_dthwv1<- ggplot(datafull_dth_wv1, aes(y=eth)) +
  geom_text(aes(y = (eth), x = 1, label = HR_CI,fontface = bold_HR_CI),size = 4) + 
  ggtitle("HR[95%CI]")+
  theme_minimal()+
  theme(plot.margin = unit(c(0,0,0,0),'cm'),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits=rev)+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P2_dthwv1
#========================================================================
#Wave 2
#=================

datafull_dth_wv2<- subset(data_full, Covid19_outcome=="Death"& Wave==2); head(datafull_dth_wv2)
names(datafull_dth_wv2)

#hazard ratio and error bars for hospitalizations and deaths wave 1
datafull_dth_wv2$HR <- as.numeric(datafull_dth_wv2$HR)

P_dthwv2<-ggplot(datafull_dth_wv2, aes(x=HR,y=eth))+
  geom_point(shape = 18, size = 5)+
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25)+
  labs(title="Wave 2")+
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.0, size=14,face="bold"),
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y.left = element_line(colour = "black"),
        #axis.line.x.bottom=element_line(size = 0.5, colour = "black", linetype=1),
        #axis.line.x.top=element_line(size = 1.5, colour = "black", linetype=1))+
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(expand = c(0, 0))+ #limits = c(0, 16.0),
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P_dthwv2

## hazard ratio and CI values
P2_dthwv2<- ggplot(datafull_dth_wv2, aes(y=eth)) +
  geom_text(aes(y = (eth), x = 1, label = HR_CI,fontface = bold_HR_CI),size = 4) + 
  ggtitle("HR[95%CI]")+
  theme_minimal()+
  theme(plot.margin = unit(c(0,0,0,0),'cm'),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits=rev)+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P2_dthwv2
#=======================================
#Wave 3
#=================
datafull_dth_wv3<- subset(data_full, Covid19_outcome=="Death"& Wave==3); head(datafull_dth_wv3)
names(datafull_dth_wv3)

#hazard ratio and error bars for hospitalizations and deaths wave 1
datafull_dth_wv3$HR <- as.numeric(datafull_dth_wv3$HR)

P_dthwv3<-ggplot(datafull_dth_wv3, aes(x=HR,y=eth))+
  geom_point(shape = 18, size = 5)+
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25)+
  labs(title="Wave 3")+
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.0, size=14,face="bold"),
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y.left = element_line(colour = "black"),
        #axis.line.x.bottom=element_line(size = 0.5, colour = "black", linetype=1),
        #axis.line.x.top=element_line(size = 1.5, colour = "black", linetype=1))+
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(expand = c(0, 0))+ #limits = c(0, 16.0),
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P_dthwv3

## hazard ratio and CI values
P2_dthwv3<- ggplot(datafull_dth_wv3, aes(y=eth)) +
  geom_text(aes(y = (eth), x = 1, label = HR_CI,fontface = bold_HR_CI),size = 4) + 
  ggtitle("HR[95%CI]")+
  theme_minimal()+
  theme(plot.margin = unit(c(0,0,0,0),'cm'),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits=rev)+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P2_dthwv3

#==========================
#Wave 4
datafull_dth_wv4<- subset(data_full, Covid19_outcome=="Death"& Wave==4); head(datafull_dth_wv4)
names(datafull_dth_wv4)

#hazard ratio and error bars for hospitalizations and deaths wave 1
datafull_dth_wv4$HR <- as.numeric(datafull_dth_wv4$HR)

P_dthwv4<-ggplot(datafull_dth_wv4, aes(x=HR,y=eth))+
  geom_point(shape = 18, size = 5)+
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25)+
  labs(title="Wave 4")+
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.0, size=14,face="bold"),
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y.left = element_line(colour = "black"),
        #axis.line.x.bottom=element_line(size = 0.5, colour = "black", linetype=1),
        #axis.line.x.top=element_line(size = 1.5, colour = "black", linetype=1))+
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(expand = c(0, 0))+ #limits = c(0, 16.0),
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P_dthwv4

## hazard ratio and CI values
P2_dthwv4<- ggplot(datafull_dth_wv4, aes(y=eth)) +
  geom_text(aes(y = (eth), x = 1, label = HR_CI,fontface = bold_HR_CI),size = 4) + 
  ggtitle("HR[95%CI]")+
  theme_minimal()+
  theme(plot.margin = unit(c(0,0,0,0),'cm'),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits=rev)+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P2_dthwv4

#==========================================================================
#Infections 
#========================
datafull_inf_wv1 <- subset(data_full, Covid19_outcome=="Infection "& Wave==1); head(datafull_inf_wv1 )
names(datafull_inf_wv1)

#hazard ratio and error bars for hospitalizations and Infections wave 1
datafull_inf_wv1$HR <- as.numeric(datafull_inf_wv1$HR)

P_infwv1<-ggplot(datafull_inf_wv1, aes(x=HR,y=eth))+
  geom_point(shape = 18, size = 5)+
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25)+
  labs(title="Wave 1",
       y="Ethnicity",tag="D")+
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.0, size=14,face="bold"),
        axis.text.y = element_text(face = rev(bold_eth_labels),size=12),
        plot.tag = element_text(size = 25,face="bold"),
        plot.tag.position = c(0.1, 1),
        axis.ticks.y=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 22),
        #axis.line.x.bottom=element_line(size = 0.5, colour = "black", linetype=1),
        #axis.line.x.top=element_line(size = 1.5, colour = "black", linetype=1))+
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(expand = c(0, 0))+ #limits = c(0, 16.0),
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P_infwv1

## hazard ratio and CI values
P2_infwv1<- ggplot(datafull_inf_wv1, aes(y=eth)) +
  geom_text(aes(y = (eth), x = 1, label = HR_CI,fontface = bold_HR_CI),size = 4) + 
  ggtitle("HR[95%CI]")+
  theme_minimal()+
  theme(plot.margin = unit(c(0,0,0,0),'cm'),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits=rev)+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P2_infwv1
#========================================================================
#Wave 2
#=================

datafull_inf_wv2<- subset(data_full, Covid19_outcome=="Infection "& Wave==2); head(datafull_inf_wv2)
names(datafull_inf_wv2)

#hazard ratio and error bars for hospitalizations and Infectionss wave 1
datafull_inf_wv2$HR <- as.numeric(datafull_inf_wv2$HR)

P_infwv2<-ggplot(datafull_inf_wv2, aes(x=HR,y=eth))+
  geom_point(shape = 18, size = 5)+
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25)+
  labs(title="Wave 2")+
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.0, size=14,face="bold"),
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y.left = element_line(colour = "black"),
        #axis.line.x.bottom=element_line(size = 0.5, colour = "black", linetype=1),
        #axis.line.x.top=element_line(size = 1.5, colour = "black", linetype=1))+
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(expand = c(0, 0))+ #limits = c(0, 16.0),
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P_infwv2

## hazard ratio and CI values
P2_infwv2<- ggplot(datafull_inf_wv2, aes(y=eth)) +
  geom_text(aes(y = (eth), x = 1, label = HR_CI,fontface = bold_HR_CI),size = 4) + 
  ggtitle("HR[95%CI]")+
  theme_minimal()+
  theme(plot.margin = unit(c(0,0,0,0),'cm'),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits=rev)+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P2_infwv2
#=======================================
#Wave 3
#=================
datafull_inf_wv3<- subset(data_full, Covid19_outcome=="Infection "& Wave==3); head(datafull_inf_wv3)
names(datafull_inf_wv3)

#hazard ratio and error bars for hospitalizations and Infectionss wave 1
datafull_inf_wv3$HR <- as.numeric(datafull_inf_wv3$HR)

P_infwv3<-ggplot(datafull_inf_wv3, aes(x=HR,y=eth))+
  geom_point(shape = 18, size = 5)+
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25)+
  labs(title="Wave 3")+
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.0, size=14,face="bold"),
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y.left = element_line(colour = "black"),
        #axis.line.x.bottom=element_line(size = 0.5, colour = "black", linetype=1),
        #axis.line.x.top=element_line(size = 1.5, colour = "black", linetype=1))+
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(expand = c(0, 0))+
  theme(plot.margin = unit(c(1,0,1,0), "cm")) #limits = c(0, 16.0),
P_infwv3

## hazard ratio and CI values
P2_infwv3<- ggplot(datafull_inf_wv3, aes(y=eth)) +
  geom_text(aes(y = (eth), x = 1, label = HR_CI,fontface = bold_HR_CI),size = 4) + 
  ggtitle("HR[95%CI]")+
  theme_minimal()+
  theme(plot.margin = unit(c(0,0,0,0),'cm'),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits=rev)+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P2_infwv3

#==========================
#Wave 4
datafull_inf_wv4<- subset(data_full, Covid19_outcome=="Infection "& Wave==4); head(datafull_inf_wv4)
names(datafull_inf_wv4)

#hazard ratio and error bars for hospitalizations and Infectionss wave 1
datafull_inf_wv4$HR <- as.numeric(datafull_inf_wv4$HR)

P_infwv4<-ggplot(datafull_inf_wv4, aes(x=HR,y=eth))+
  geom_point(shape = 18, size = 5)+
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25)+
  labs(title="Wave 4")+
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.0, size=14,face="bold"),
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y.left = element_line(colour = "black"),
        #axis.line.x.bottom=element_line(size = 0.5, colour = "black", linetype=1),
        #axis.line.x.top=element_line(size = 1.5, colour = "black", linetype=1))+
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(expand = c(0, 0))+ #limits = c(0, 16.0)
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P_infwv4

## hazard ratio and CI values
P2_infwv4<- ggplot(datafull_inf_wv4, aes(y=eth)) +
  geom_text(aes(y = (eth), x = 1, label = HR_CI,fontface = bold_HR_CI),size = 4) + 
  ggtitle("HR[95%CI]")+
  theme_minimal()+
  theme(plot.margin = unit(c(0,0,0,0),'cm'),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits=rev)+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
P2_infwv4

#============================================================================
  #Plot layout
lay2<-  matrix(c(1,1,2,3,4,5,6,7,8,9,9,10,11,12,13,14,15,16,17,17,18,19,20,21,22,23,24,25,25,26,27,28,29,30,31,32),nrow=4,byrow=T)
layout(lay2)

plot_wave1<-grid.arrange(P_hspdthwv1,P2_hspdthwv1,P_hspdthwv2,P2_hspdthwv2,P_hspdthwv3,P2_hspdthwv3,P_hspdthwv4,P2_hspdthwv4,
                         P_hspwv1,P2_hspwv1,P_hspwv2,P2_hspwv2,P_hspwv3,P2_hspwv3,P_hspwv4,P2_hspwv4,
                         P_dthwv1,P2_dthwv1,P_dthwv2,P2_dthwv2,P_dthwv3,P2_dthwv3,P_dthwv4,P2_dthwv4,
                         P_infwv1,P2_infwv1,P_infwv2,P2_infwv2,P_infwv3,P2_infwv3,P_infwv4,P2_infwv4,
                         layout_matrix = lay2)

ggsave(file="Figure  3_300123v3.pdf",  plot_wave1,width =18,height=15,dpi=1000)
#=================================================================================
