####Preliminary plots of extracted current studies 
install.packages("lubdriate")
install.packages("ggpubr")
install.packages("editrules")
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggpubr)
library(editrules)

var_raw<-read_csv("varMATRIX.csv")
glimpse(var_raw)


##histograms for:
#organization level overall
ggplot(var_raw, aes(x=organization_level))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title="Organization level distribution")

#organizarion level colored by study 
ggplot(var_raw, aes(x=organization_level, fill=as.factor(study_number)))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title="Study organization level distribution")

#organization level colored by variation color 
ggplot(var_raw, aes(x=organization_level, fill=variation_color))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title="Variation color in different organization levels")

#organization level colored by wave type 
ggplot(var_raw, aes(x=organization_level, fill=wave_type))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title="Variation wave type in different organization levels")

#organization level broken out by independent variable
ggplot(var_raw, aes(x=organization_level, fill=independent_variable))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title="Independent variables in different organization levels")

#independent variables overall
ggplot(var_raw, aes(x=independent_variable))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#dependent variables overall
ggplot(var_raw, aes(x=dependent_variable))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##############################################################################################################################
#scatterplot of GT (days) vs period of fluctuation (days)
ggplot(var_raw, aes(x=as.numeric(generation_time_standardized_days), y=flux_period_days, shape=`larger group`, color=organization_level))+
  geom_jitter(alpha=0.5, size=4)+
  theme_classic(base_size=15)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=11))+
  labs(title="Generation time (days) vs Period of fluctuation (days) by organization level")+
  xlab("Generation time (days)")+ 
  ylab("Period of fluctuation (days)")


##filtering genearation time and period 
#less 500 generation time (days) and less than 10 fluctuation period (days)
narrowdays_GTfp<-filter(var_raw, generation_time_standardized_days < 500, flux_period_days < 10)

plot8a<-ggplot(narrowdays_GTfp, aes(x=as.numeric(generation_time_standardized_days), y=flux_period_days, shape=organization_level, color=organization_level))+
  geom_jitter(alpha=0.5, size=4)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title="Generation time (less than 500 days) vs Period of fluctuation (less than 10 days) by organization level")+
  xlab("Generation time (days)")+
  ylab("Period of fluctuation (days)")


plot10<-ggplot(narrowdays_GTfp, aes(x=as.numeric(generation_time_standardized_days), y=flux_period_days, shape=`larger group`, color=organization_level))+
  geom_jitter(alpha=0.5, size=4)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title="Generation time (less than 500 days) vs Period of fluctuation (less than 10 days) by organization level")+
  xlab("Generation time (days)")+
  ylab("Period of fluctuation (days)")
  
#less than 2 days period of flux and less than 90 days generation time
slimdays_GTfp<-filter(var_raw, generation_time_standardized_days < 90, flux_period_days < 2)

plot11<-ggplot(slimdays_GTfp, aes(x=as.numeric(generation_time_standardized_days), y=flux_period_days, shape=organization_level, color=organization_level))+
  geom_jitter(alpha=0.5, size=4)+
  theme_classic()+
  labs(title="Generation time (less than 90 days) vs Period of fluctuation (less than 2 days) by organization level")+
  xlab("Generation time (days)")+
  ylab("Period of fluctuation (days)")


plot12<-ggplot(slimdays_GTfp, aes(x=as.numeric(generation_time_standardized_days), y=flux_period_days, shape=organization_level, color=organization_level))+
  geom_jitter(alpha=0.5, size=4)+
  theme_classic(base_size=15)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=11))+
  labs(title="Generation time (< 90 days) vs Period of fluctuation (< 2 days) by variation color")+
  xlab("Generation time (days)")+
  ylab("Period of fluctuation (days)")+
  facet_wrap(~variation_color)



plot13<-ggplot(slimdays_GTfp, aes(x=as.numeric(generation_time_standardized_days), y=flux_period_days, shape=`larger group`, color=organization_level))+
  geom_jitter(alpha=0.5, size=4)+
  theme_classic(base_size=15)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=11))+
  labs(title="Generation time (< 90 days) vs Period of fluctuation (< 2 days) by larger organism group")+
  xlab("Generation time (days)")+
  ylab("Period of fluctuation (days)")+
  facet_wrap(~variation_color)
  