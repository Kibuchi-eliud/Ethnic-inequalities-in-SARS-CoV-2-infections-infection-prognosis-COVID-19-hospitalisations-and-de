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

data_full_d<-read.csv("N:/Mediation analysis/outcomes paper/forest_plots/covid19_ethnic_disaggr_300123_updated.csv",stringsAsFactors = F)
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
#ethnic_report<- c("White","White Scottish","White British","White Irish","White Gypsy","White Polish","Other White",
#"Mixed","Mixed-disagg", "Asian", "Pakistani","Indian","Bangladeshi","Chinese", "Other Asian","Black","African","Caribbean ","Arab")

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

#View(data_full)
#summary(data_full$ethnic_grp)
#summary(data_full$eth)
##==========================================================================================
#=========================================================================================
#Hospitalisations and deaths
#============================
data_full1<- subset(data_full, Covid19_outcome=="Hospitalisation and death"); head(data_full1)
names(data_full1)

#Observations and events for hospitalisations and deaths
p1<-ggplot(data_full1, aes(y=eth)) +
  geom_text(aes(y = (eth), x = 1, label = Obs_events,fontface = bold_Obs_events), size = 4) + 
  theme_minimal()+
  labs(title="Observations|Events",
       y="Ethnicity",tag="A")+
  theme(plot.title = element_text(hjust = 0.5, size=12),
        plot.tag = element_text(size = 25,face="bold"),
        plot.tag.position = c(0.1, 1),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 22),
        axis.text.y = element_text(face = rev(bold_eth_labels),size=12))+
  scale_x_discrete(position = "top")+
  scale_y_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 15),limits=rev)+
 theme(plot.margin = unit(c(1,1,1,0), "cm"))
p1


#hazard ratio and error bars for hospitalizations and deaths
data_full1$HR <- as.numeric(data_full1$HR)
print(data_full1)
p2<-ggplot(data_full1, aes(x=HR,y=eth))+
  geom_point(shape = 18, size = 5)+
geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25)+
labs(title="Hospitalisation or death")+
geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.0, size=14,face="bold"),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        #axis.line.x.bottom=element_line(size = 0.5, colour = "black", linetype=1),
        #axis.line.x.top=element_line(size = 1.5, colour = "black", linetype=1))+
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_discrete(limits=rev)+
   scale_x_continuous(limits = c(0, 5),expand = c(0, 0))+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
p2


## hazard ratio and CI values
p3 <- ggplot(data_full1, aes(y=eth)) +
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
p3


#=========================================================================
#Hospitalisations
#================
data_hosp<- subset(data_full, Covid19_outcome=="Hospitalisation"); head(data_hosp)
names(data_hosp)

data_hosp2<-data_hosp %>% select(eth,observations, events) %>% melt(id.vars='eth',variable.name='label',value.name = 'numbers')

#Observations and events for hospitalisations
p4<-ggplot(data_hosp)+
  geom_text(aes(y = (eth), x = 1, label = Obs_events,fontface = bold_Obs_events), size = 4) + 
  theme_minimal()+
  labs(title="Observations|Events",tag="B")+
  theme(plot.title = element_text(hjust = 0.5, size=12),
        plot.tag = element_text(size = 25,face="bold"),
        plot.tag.position = c(0.1, 1),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y.left = element_line(colour = "black"))+
      labs(y = "")+
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits=rev, )+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
p4

#hazard ratio and error bars for hospitalisations
data_hosp$HR <- as.numeric(data_full1$HR)

p5<-ggplot(data_hosp, aes(x=HR,y=eth))+
  geom_point(shape = 18, size = 5)+
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25)+
  labs(title="Hospitalisation")+
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.25, size=14,face="bold"),
        plot.margin = unit(c(0,0,0,0),'cm'),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        #axis.line.x.bottom=element_line(size = 0.5, colour = "black", linetype=1))+
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_discrete(limits=rev, )+
  scale_x_continuous(limits = c(0, 5))+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
p5


p6 <- ggplot(data_hosp, aes(y=eth)) +
  geom_text(aes(y = (eth), x = 1, label = HR_CI,fontface = bold_HR_CI), size = 4) + 
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
  scale_y_discrete(limits=rev, )+
  scale_x_discrete(position = "top")+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
p6

#========================================================================
#Deaths 

data_death<- subset(data_full, Covid19_outcome=="Death"); head(data_death)
names(data_death)

#Observations and events for  deaths
p1_death<-ggplot(data_death, aes(y=eth)) +
  geom_text(aes(y = (eth), x = 1, label = Obs_events,fontface = bold_Obs_events), size = 4) + 
  theme_minimal()+
  labs(title="Observations|Events",
       y="Ethnicity",tag="C")+
   theme(plot.title = element_text(hjust = 0.5, size=12),
         plot.tag = element_text(size = 25,face="bold"),
         plot.tag.position = c(0.1, 1),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 22),
        axis.text.y = element_text(face = rev(bold_eth_labels),size=12))+
     scale_x_discrete(position = "top",)+
  scale_y_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 15),limits=rev)+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
p1_death


#hazard ratio and error bars for deaths
data_death$HR <- as.numeric(data_death$HR)
p2_death<-ggplot(data_death, aes(x=HR,y=eth))+
  geom_point(shape = 18, size = 5)+
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25)+
  labs(title="Death")+
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.25, size=14,face="bold"),
        plot.margin = unit(c(0,0,0,0),'cm'),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        #axis.line.x.bottom=element_line(size = 0.5, colour = "black", linetype=1))+
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_discrete(limits=rev, )+
  scale_x_continuous(limits = c(0, 5))+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

p2_death

## hazard ratio and CI values
p3_death <- ggplot(data_death, aes(y=eth)) +
  geom_text(aes(y = (eth), x = 1, label = HR_CI,fontface = bold_HR_CI), size = 4) + 
  ggtitle("HR[95%CI]")+
  theme_minimal()+
  theme(plot.margin = unit(c(0,0,0,0),'cm'),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  scale_y_discrete(limits=rev, )+
  scale_x_discrete(position = "top")+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
p3_death

#================================================================
#Infections 

data_inf<- subset(data_full, Covid19_outcome=="Infection"); head(data_inf)
names(data_inf)


#Observations and events for infections
p4_inf<-ggplot(data_inf)+
  geom_text(aes(y = (eth), x = 1, label = Obs_events,fontface = bold_Obs_events), size = 4) + 
  theme_minimal()+
  labs(title="Observations|Events",tag="D")+
 theme(plot.title = element_text(hjust = 0.5, size=12),
       plot.tag = element_text(size = 25,face="bold"),
       plot.tag.position = c(0.1, 1),
      panel.border = element_blank(),
              panel.grid.major = element_blank(),
              axis.title.x = element_blank(),
              axis.text.y = element_blank(),
              axis.line.y.left = element_line(colour = "black"))+
  labs(y = "")+
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits=rev, )+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
p4_inf


#hazard ratio and error bars for infections
data_inf$HR <- as.numeric(data_inf$HR)

p5_inf <-ggplot(data_inf, aes(x=HR,y=eth))+
  geom_point(shape = 18, size = 5)+
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25)+
  labs(title="Infection")+
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.25, size=14,face="bold"),
        plot.margin = unit(c(0,0,0,0),'cm'),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        #axis.line.x.bottom=element_line(size = 0.5, colour = "black", linetype=1))+
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_discrete(limits=rev, )+
  scale_x_continuous(limits = c(0, 5))+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
p5_inf


p6_inf <- ggplot(data_inf, aes(y=eth)) +
  geom_text(aes(y = (eth), x = 1, label = HR_CI,fontface = bold_HR_CI), size = 4) + 
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
  scale_y_discrete(limits=rev, )+
  scale_x_discrete(position = "top")+
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
p6_inf

#============================================================================
#Plot layout
 
lay2<-  matrix(c(1,1,2,3,4,4,5,6,7,7,8,9,10,10,11,12), nrow=2,byrow=T)
layout(lay2)

grid.arrange(p1,p2,p3,p4,p5,p6,p1_death,p2_death,p3_death,p4_inf,p5_inf,p6_inf,layout_matrix = lay2)
 
 p_testf6<-grid.arrange(p1,p2,p3,p4,p5,p6,p1_death,p2_death,p3_death,p4_inf,p5_inf,p6_inf,layout_matrix = lay2)


ggsave(file="Figure 1_300123v2.pdf", p_testf6,width = 14.5,height =16,dpi=1000)

#=================================================================================
