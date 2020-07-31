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
  theme_classic()


#organization level colored by variation level 
plot2<-ggplot(var_raw, aes(x=organization_level, fill=variation_color))+
  geom_bar()+
  theme_classic()

#organization level broken out by independent variable
plot3<-ggplot(var_raw, aes(x=organization_level, fill=independent_variable))+
  geom_bar()+
  theme_classic()

#independent variables overall
plot4<-ggplot(var_raw, aes(x=independent_variable))+
  geom_bar()+
  theme_classic()

###################
ggplot(var_raw, aes(x=generation_time_standardized_days, y=duration_standardized_days, color=study_number))+
  geom_point()+
  theme_classic()

###filtering for majority studies generation time
var_marjorityGT<-filter(var_raw, generation_time_standardized_days < 110, duration_standardized_days < 100)

#scatterplot of GT vs experiment duration coded by organization level
ggplot(var_marjorityGT, aes(x=generation_time_standardized_days, y=duration_standardized_days, shape=organization_level))+
  geom_jitter()+
  theme_classic()

##correlation plot 

ggscatter(var_marjorityGT, x = "generation_time_standardized_days", y = "duration_standardized_days", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "Generation time (days)", ylab = "Experiment Duration (days)")

ggscatter(var_marjorityGT, x = "generation_time_standardized_days", y = "duration_standardized_days", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Generation time (days)", ylab = "Experiment Duration (days)")

  
  