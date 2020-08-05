####Preliminary plots of extracted current studies 
install.packages("lubdriate")
install.packages("ggpubr")
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggpubr)

var_raw<-read.csv("varMATRIX.csv")
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

#scatterplot of GT vs experiment duration coded by organization level
ggplot(var_majority, aes(x=generation_time_standardized_days, y=duration_standardized_days, shape=organization_level, color=organization_level))+
  geom_jitter()+
  theme_classic()+
  labs(title="Generation time (days) vs Experiment duration (days) by organization level")
#scatterplot of GT vs period of flucuation 
ggplot(var_raw, aes(x=generation_time_standardized_days, y=flux_period_days, shape=organization_level, color=organization_level))+
  geom_jitter(alpha=0.5, size=4)+
  theme_classic()+
  labs(title="Generation time (days) vs Period of fluctuation (days) by organization level")



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

#filtering by organization level 
#individual 
var_individual<-filter(var_majority, organization_level == "individual", generation_time_standardized_days > 0)

#population 
var_pop<-filter(var_majority, organization_level == "population", generation_time_standardized_days > 0)

#community
var_comm<-filter(var_majority, organization_level == "community", generation_time_standardized_days > 0)


#generation time vs duration at individual level 
a<-ggscatter(var_individual, x = "generation_time_standardized_days", y = "duration_standardized_days", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "Generation time (days)", ylab = "Experiment Duration (days)")

#generation time vs duration at population level 
b<-ggscatter(var_pop, x = "generation_time_standardized_days", y = "duration_standardized_days", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "Generation time (days)", ylab = "Experiment Duration (days)")
#generation time vs duration at community level 
c<-ggscatter(var_comm, x = "generation_time_standardized_days", y = "duration_standardized_days", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "Generation time (days)", ylab = "Experiment Duration (days)")

  
  