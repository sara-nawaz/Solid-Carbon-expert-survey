library(dplyr)
library(ggplot2)
theme_set(
  theme_light() + theme(legend.position = "top")
)
library(tidyr)
library(glmnet)
library(broom)
library(questionr) # for odds ratio function
library(sjPlot)

setwd("~/Desktop/Stuff/Postdoc/Solid carbon/Expert survey/data and analysis/R scripts and data")
data <- read.csv("expert survey_cleaned.csv", sep= ',', stringsAsFactors=FALSE, header = TRUE)

##### Prepare mean scores, based on results from factor analysis ####
data$needNETs1mean=rowMeans(cbind(data$Q2_4, data$Q2_2),na.rm=TRUE)
data$needNETs2mean=rowMeans(cbind(data$Q2_3, data$Q2_1),na.rm=TRUE)

data$risk_lmean=rowMeans(cbind(data$Q8a_1, data$Q8a_2, data$Q8a_3, data$Q8a_4, data$Q8a_5, 
                                data$Q8a_6, data$Q8a_7, data$Q8a_8, data$Q8a_9, data$Q8a_10,
                                data$Q8a_11),na.rm=TRUE)
data$benefit_lmean=rowMeans(cbind(data$Q9a_1, data$Q9a_2, data$Q9a_3, data$Q9a_4, data$Q9a_5, 
                                   data$Q9a_6, data$Q9a_7, data$Q9a_8, data$Q9a_9, data$Q9a_10,
                                   data$Q9a_11),na.rm=TRUE)

data$governancemean=rowMeans(cbind(data$Q13_1, data$Q13_2, data$Q13_3, data$Q13_4, data$Q13_5),na.rm=TRUE)

data$climate_resp1mean=rowMeans(cbind(data$Q14_6, data$Q14_1, data$Q14_5, data$Q14_4, data$Q14_3),na.rm=TRUE)
data$climate_resp2mean=rowMeans(cbind(data$Q14_7, data$Q14_9, data$Q14_8),na.rm=TRUE)

data$trust1mean=rowMeans(cbind(data$Q12_6, data$Q12_5, data$Q12_5, data$Q12_9, data$Q12_10, 
                               data$Q12_8, data$Q12_11, data$Q12_7),na.rm=TRUE)
data$trust2mean=rowMeans(cbind(data$Q12_1, data$Q12_3, data$Q12_2) ,na.rm=TRUE)

data$risk_l.a <- rowMeans(cbind(data$Q8a_4, data$Q8a_3, data$Q8a_7, data$Q8a_6, data$Q8a_5))
data$risk_l.b <- rowMeans(cbind(data$Q8a_11, data$Q8a_9, data$Q8a_8, data$Q8a_10))


data$DAC <- as.factor(data$DAC)
data$DAC_offshore <- as.factor(data$DAC_offshore)
data$DAC_onshore <- as.factor(data$DAC_onshore)
data$alkalin <- as.factor(data$alkalin)
data$weathering <- as.factor(data$weathering)
data$fertil <- as.factor(data$fertil)
data$coast_rest <- as.factor(data$coast_rest)
data$afforest <- as.factor(data$afforest)
data$biochar <- as.factor(data$biochar)
data$soil <- as.factor(data$soil)

# Compile dependent and independent variables into a data frame for regression analysis
NETdata <- data.frame(data$support, data$DAC, data$DAC_offshore, data$DAC_onshore, data$alkalin, data$weathering, 
                      data$fertil, data$coast_rest, data$afforest, data$biochar, data$soil, 
                      data$needNETs1mean, data$needNETs2mean, data$risk_l.a, data$risk_l.b, 
                      data$governancemean, data$climate_resp1mean, data$climate_resp2mean, data$age, data$gender,
                      data$Q20, data$trust1mean,data$trust2mean)
NETdata<-na.omit(NETdata)


##### Make CSVs with results from regressions on each NET type ####
DACreg <- glm(formula = as.factor(data$DAC) ~ data$needNETs1mean + data$needNETs2mean + data$risk_l.a 
              + data$risk_l.b + data$governancemean + data$climate_resp1mean + data$climate_resp2mean
              + data$trust1mean + data$trust2mean + data$age + data$gender + data$Q20, 
              family = binomial(link="logit"), data = NETdata)
summary(DACreg)
DAC_or <- odds.ratio(DACreg, level=0.95)
DAC_or
write.csv(DAC_or, file="DAC_or.csv")

DAC_offshorereg <- glm(formula = as.factor(data$DAC_offshore) ~ data$needNETs1mean + data$needNETs2mean + data$risk_l.a 
                       + data$risk_l.b + data$governancemean + data$climate_resp1mean + data$climate_resp2mean
                       + data$trust1mean + data$trust2mean + data$age + data$gender + data$Q20,
              family = binomial(link="logit"), data = NETdata)
summary(DAC_offshorereg)
DAC_offshore_or <- odds.ratio(DAC_offshorereg, level=0.95)
DAC_offshore_or
write.csv(DAC_offshore_or, file="DAC_offshore_or.csv")
  
DAC_onshorereg <- glm(formula = as.factor(data$DAC_onshore) ~ data$needNETs1mean + data$needNETs2mean + data$risk_l.a 
                      + data$risk_l.b + data$governancemean + data$climate_resp1mean + data$climate_resp2mean
                      + data$trust1mean + data$trust2mean + data$age + data$gender + data$Q20,
                       family = binomial(link="logit"), data = NETdata)
summary(DAC_onshorereg)
DAC_onshore_or <- odds.ratio(DAC_onshorereg, level=0.95)
DAC_onshore_or
write.csv(DAC_onshore_or, file="DAC_onshore_or.csv")

alkalinreg <- glm(formula = as.factor(data$alkalin) ~ data$needNETs1mean + data$needNETs2mean + data$risk_l.a 
                  + data$risk_l.b + data$governancemean + data$climate_resp1mean + data$climate_resp2mean
                  + data$trust1mean + data$trust2mean + data$age + data$gender + data$Q20,
                      family = binomial(link="logit"), data = NETdata)
summary(alkalinreg)
alkalin_or <- odds.ratio(alkalinreg, level=0.95)
alkalin_or
write.csv(alkalin_or, file="alkalin_or.csv")

weatheringreg <- glm(formula = as.factor(data$weathering) ~ data$needNETs1mean + data$needNETs2mean + data$risk_l.a 
                     + data$risk_l.b + data$governancemean + data$climate_resp1mean + data$climate_resp2mean
                     + data$trust1mean + data$trust2mean + data$age + data$gender + data$Q20,
                      family = binomial(link="logit"), data = NETdata)
summary(weatheringreg)
weathering_or <- odds.ratio(weatheringreg, level=0.95)
weathering_or
write.csv(weathering_or, file="weathering_or.csv") 

fertilreg <- glm(formula = as.factor(data$fertil) ~ data$needNETs1mean + data$needNETs2mean + data$risk_l.a 
                 + data$risk_l.b + data$governancemean + data$climate_resp1mean + data$climate_resp2mean
                 + data$trust1mean + data$trust2mean + data$age + data$gender + data$Q20,
                         family = binomial(link="logit"), data = NETdata)
summary(fertilreg)
fertil_or <- odds.ratio(fertilreg, level=0.95)
fertil_or
write.csv(fertil_or, file="fertil_or.csv")

coast_rest_reg <- glm(formula = as.factor(data$coast_rest) ~ data$needNETs1mean + data$needNETs2mean + data$risk_l.a 
                      + data$risk_l.b + data$governancemean + data$climate_resp1mean + data$climate_resp2mean
                      + data$trust1mean + data$trust2mean + data$age + data$gender + data$Q20,
                 family = binomial(link="logit"), data = NETdata)
summary(coast_rest_reg)
coast_rest_or <- odds.ratio(coast_rest_reg, level=0.95)
coast_rest_or
write.csv(coast_rest_or, file="coast_rest_or.csv")

afforest_reg <- glm(formula = as.factor(data$afforest) ~ data$needNETs1mean + data$needNETs2mean + data$risk_l.a 
                    + data$risk_l.b + data$governancemean + data$climate_resp1mean + data$climate_resp2mean
                    + data$trust1mean + data$trust2mean + data$age + data$gender + data$Q20,
                      family = binomial(link="logit"), data = NETdata)
summary(afforest_reg)
afforest_or <- odds.ratio(afforest_reg, level=0.95)
afforest_or
write.csv(afforest_or, file="afforest_or.csv")

biochar_reg <- glm(formula = as.factor(data$biochar) ~ data$needNETs1mean + data$needNETs2mean + data$risk_l.a 
                   + data$risk_l.b + data$governancemean + data$climate_resp1mean + data$climate_resp2mean
                   + data$trust1mean + data$trust2mean + data$age + data$gender + data$Q20,
                    family = binomial(link="logit"), data = NETdata)
summary(biochar_reg)
biochar_or <- odds.ratio(biochar_reg, level=0.95)
biochar_or
write.csv(biochar_or, file="biochar_or.csv")


soil_reg <- glm(formula = as.factor(data$soil) ~ data$needNETs1mean + data$needNETs2mean + data$risk_l.a 
                + data$risk_l.b + data$governancemean + data$climate_resp1mean + data$climate_resp2mean
                + data$trust1mean + data$trust2mean + data$age + data$gender + data$Q20,
                   family = binomial(link="logit"), data = NETdata)
summary(soil_reg)
soil_or <- odds.ratio(soil_reg, level=0.95)
soil_or 
write.csv(soil_or, file="soil_or.csv")


##### Regression support on EVs ####
supportreg <- glm(formula = as.factor(data$support) ~ data$needNETs1mean + data$needNETs2mean + data$risk_l.a 
                  + data$risk_l.b + data$governancemean + data$climate_resp1mean + data$climate_resp2mean
                  + data$trust1mean + data$trust2mean + data$age + data$gender + data$Q20,
              family = binomial(link="logit"), data = NETdata)
summary(supportreg)
support_or <- odds.ratio(supportreg, level=0.95)
support_or
write.csv(support_or, file="support_or.csv")




support_plot <- plot_model(supportreg, title = "Likelihood of supporting SC", show.values = TRUE, dot.size = 3, value.offset=0.3,
                       vline.color = "grey", colors= c("blue", "red"), geom.label.color = "black") +
  scale_x_discrete(labels=list(
    data$needNETs1mean = "Need for NETs scale 1",
    data$needNETs2mean = "Need for NETs scale 2",
    data$risk_lmean = "Perceived likelihood of risks",
    data$benefit_lmean = "Perceived likelihood of benefits",
    data$climate_resp1mean = "Climate responsibility scale 1",
    data$climate_resp2mean = "Climate responsibility scale 2"
  ))+
  theme_sjplot(base_size = 16, base_family = "Times")+
  theme(axis.title.x=element_blank())+
  theme(plot.margin = unit(c(1, 0.3, 1, 0.7), "cm"))
support_plot

