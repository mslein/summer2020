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
plot1<-ggplot(var_raw, aes(x=organization_level))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title="Organization level distribution")

#organizarion level colored by study 
plot2<-ggplot(var_raw, aes(x=organization_level, fill=as.factor(study_number)))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title="Study organization level distribution")

#organization level colored by variation color 
plot3<-ggplot(var_raw, aes(x=organization_level, fill=variation_color))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title="Variation color in different organization levels")

#organization level colored by wave type 
plot8<-ggplot(var_raw, aes(x=organization_level, fill=wave_type))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title="Variation wave type in different organization levels")

#organization level broken out by independent variable
plot4<-ggplot(var_raw, aes(x=organization_level, fill=independent_variable))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title="Independent variables in different organization levels")

#independent variables overall
plot5<-ggplot(var_raw, aes(x=independent_variable))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#dependent variables overall
plot6<-ggplot(var_raw, aes(x=dependent_variable))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#dependent variables by genus
plot7<-ggplot(var_raw, aes(x=dependent_variable, fill=as.factor(organization_level)))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###################
ggplot(var_raw, aes(x=generation_time_standardized_days, y=duration_standardized_days, color=study_number))+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###filtering for majority studies generation time
var_majority<-filter(var_raw, generation_time_standardized_days < 110, duration_standardized_days < 100)
var_narrow<-filter(var_majority, flux_period_days < 5)

#scatterplot of GT vs experiment duration coded by organization level
ggplot(var_majority, aes(x=generation_time_standardized_days, y=duration_standardized_days, shape=organization_level, color=organization_level))+
  geom_jitter()+
  theme_classic()+
  labs(title="Generation time (days) vs Experiment duration (days) by organization level")
#scatterplot of GT vs period of fluctuation 
ggplot(var_raw, aes(x=generation_time_standardized_days, y=flux_period_days, shape=organization_level, color=organization_level))+
  geom_jitter(alpha=0.5, size=4)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title="Generation time (days) vs Period of fluctuation (days) by organization level")
#scatterplot of GT vs period of fluctuation (less than five day period)
ggplot(var_narrow, aes(x=as.numeric(generation_time_standardized_days), y=as.numeric(flux_period_days), color=organization_level))+
  geom_jitter(alpha=0.3, size=4, position = position_jitter(width = 0.5, height = 0.5))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title="Generation time (days) less than 100 days vs Period of fluctuation (days) less than 5 days by organization level")


######correlation plots#####
#generation time vs duration--kendall correlation
ggscatter(var_majority, x = "generation_time_standardized_days", y = "duration_standardized_days", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "Generation time (days)", ylab = "Experiment Duration (days)")
#generation time vs duration--pearson correlation 
ggscatter(var_majority, x = "generation_time_standardized_days", y = "duration_standardized_days", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Generation time (days)", ylab = "Experiment Duration (days)")

  