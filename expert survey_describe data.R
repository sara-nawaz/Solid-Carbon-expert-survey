library(tidyverse)
library(dplyr)
library(ggplot2)
theme_set(
  theme_light() + theme(legend.position = "top")
)
library(tidyr)
library(scales)  # for percentage scales
library(psych)
library(reshape2)
library(likert)
library(ggpubr)
library(rstatix)
library(networkD3)
library(easyalluvial)

setwd("~/Desktop/Stuff/Postdoc/Solid carbon/Expert survey/data and analysis/R scripts and data")
data <- read.csv("expert survey_cleaned.csv", sep= ',', stringsAsFactors=FALSE, header = TRUE)
str(data) 

##### number of respondents, group 1 vs. group 2 #####
table(data$expert)


##### Test group for normality ####
data %>%
  group_by(expert) %>%
  summarise('W Stat' = shapiro.test(soil)$statistic, p.value=shapiro.test(soil)$p.value)

#Perform Mann-Whitney U test
mDAC<-wilcox.test(DAC ~ expert, data=data, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(mDAC)
mDAC_offshore<-wilcox.test(DAC_offshore ~ expert, data=data, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(mDAC_offshore)
mDAC_onshore<-wilcox.test(DAC_onshore ~ expert, data=data, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(mDAC_onshore)
mafforest<-wilcox.test(afforest ~ expert, data=data, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(mafforest) # 10% significant
mbiochar<-wilcox.test(biochar ~ expert, data=data, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(mbiochar) # 5% signficant
mcoast_rest<-wilcox.test(coast_rest ~ expert, data=data, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(mcoast_rest) #5% signficant
mweathering<-wilcox.test(weathering ~ expert, data=data, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(mweathering) #10% significant
malkalin <-wilcox.test(alkalin ~ expert, data=data, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(malkalin) 
mfertil <-wilcox.test(fertil ~ expert, data=data, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(mfertil) 
msoil <-wilcox.test(soil ~ expert, data=data, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(msoil) 



##### Correlation matrices #####
data2 <-data.frame(data$gender, data$political, data$trust1mean, data$trust2mean,
              data$governancemean, data$nature1mean, data$nature2mean, 
              data$climate_resp1mean, data$climate_resp2mean) #matrix of IVs

library(expss)
colnames(data2) <- c("Gender", "Political", "Trust scale 1", 
                     "Trust scale 2", "Governance scale", "Nature values scale 1",
                     "Nature values scale 2", "Climate responsibility scale 1",
                     "Climate responsibility scale 2")

library(corrplot)
corrplot(cor(data2, method="pearson", use="complete.obs"), 
         tl.col = "black", tl.srt = 45, method = "number",  order = "FPC")

correlation <- corrplot(cor(data2, method="pearson", use="complete.obs"), 
                        tl.col = "black", tl.srt = 45, method = "circle",  order = "FPC")

risks <- data.frame(data$Q8a_1, data$Q8a_2, data$Q8a_3, data$Q8a_4, data$Q8a_5, data$Q8a_6, 
                    data$Q8a_7, data$Q8a_8, data$Q8a_9, data$Q8a_10, data$Q8a_11, data$Q8b_1, 
                    data$Q8b_2, data$Q8b_3, data$Q8b_4, data$Q8b_5, 
                    data$Q8b_6, data$Q8b_7, data$Q8b_8, data$Q8b_9, data$Q8b_10, data$Q8b_11) #matrix of risk likelihood with risk severity

colnames(risks) <- c("induced earthquakes--likelihood", "seismicity earthquakes--likelihood", "explosive CO2--likelihood", 
                     "deep ocean ecosystems--likelihood", "human health--likelihood", "CO2 leakage--likelihood", 
                     "ocean pollution--likelihood", "diversion of funds--likelihood", "fossil fuel dependence--likelihood", 
                     "cost overruns--likelihood", "reduced renewable dev--likelihood", 
                     "induced earthquakes--severity", "seismicity earthquakes--severity", "explosive CO2--severity", 
                     "deep ocean ecosystems--severity", "human health--severity", "CO2 leakage--severity", 
                     "ocean pollution--severity", "diversion of funds--severity", "fossil fuel dependence--severity", 
                     "cost overruns--severity", "reduced renewable dev--severity")
correlation <- corrplot(cor(risks, method="pearson", use="complete.obs"), 
                        tl.col = "black", tl.srt = 45, method = "circle",  order = "FPC")

benefits<- data.frame(data$Q9a_1, data$Q9a_2, data$Q9a_3, data$Q9a_4, data$Q9a_5, data$Q9a_6, 
                    data$Q9a_7, data$Q9a_8, data$Q9a_9, data$Q9a_10, data$Q9b_1, 
                    data$Q9b_2, data$Q9b_3, data$Q9b_4, data$Q9b_5, 
                    data$Q9b_6, data$Q9b_7, data$Q9b_8, data$Q9b_9, data$Q9b_10)#matrix of benefit likelihood with benefit importance
colnames(benefits) <- c("safe solution for CC--likelihood", "reduces CO2 concentration--likelihood", "good use carbon taxes--likelihood",
                     "employment--likelihood", "benefit future generations--likelihood", "removes atmospheric CO2--likelihood", 
                     "cost effective--likelihood", "component of green economy--likelihood", "addresses diminishing land--likelihood", 
                     "independent of ff industry--likelihood", "safe solution for CC--importance", "reduces CO2 concentration--importance", "good use carbon taxes--likelihood",
                     "employment--importance", "benefit future generations--importance", "removes atmospheric CO2--importance", 
                     "cost effective--importance", "component of green economy--importance", "addresses diminishing land--importance", 
                     "independent of ff industry--importance")
correlation <- corrplot(cor(benefits, method="pearson", use="complete.obs"), 
                        tl.col = "black", tl.srt = 45, method = "circle",  order = "FPC")




##### Support for Solid Carbon #####
data$support <- as.factor(data$support)
levels(data$support) <- c("-2" = "Strongly oppose",
                          "-1" = "Oppose",
                          "0" = "Neither oppose nor support",
                          "1" = "Support",
                          "2" = "Strongly support")

support.data <- data %>% 
  filter(!is.na(support)) %>%
  count(support) %>%
  mutate(percent = n / nrow(data)*100)

#broken down by experts...
expert.labs <- c("1" = "Geophysical experts", "2" = "Marine experts")
support.expert <- data %>% 
  filter(!is.na(support)) %>%
  group_by(expert) %>%
  count(support) %>%
  mutate(percent = n / nrow(data)*100)
support.expert$support <- as.factor(support.expert$support)
support.expert$expert <- as.factor(support.expert$expert) 

##Plot of support... ignore others unless wanting expert breakdown?
support_plot <- ggplot(data=data, aes(support)) + 
  geom_bar() +
  labs(title="Support/opposition for Solid Carbon", 
       x="Strongly oppose (-2), Oppose (-1), Neither (0), Support (+1), Strongly support (+2)") 
support_plot

support_ <- data[c("support")]

names(support_) <- c(support = "support vs. oppose")



data$support <- as.factor(support)
levels(data$support) <- c("-2" = "Strongly oppose",
                          "-1" = "Oppose",
                          "0" = "Neither oppose nor support",
                          "1" = "Support",
                          "2" = "Strongly support")

support.expert.plot <- ggplot(data, aes(as.factor(support), group = expert)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  ylab("percent") +
  scale_x_discrete(labels = c('Strongly oppose', 
                              'Oppose', 
                              'Neither', 
                              'Support', 
                              'Strongly support'))+
  theme(axis.title.x=element_blank())+
  facet_grid(~expert)

support.expert.plot





##### Make categorical variables of scales ####

#Make categorical for Question 2 item 2
data <- data %>% mutate(Q2_2cat = ifelse(Q2_2 >=4, "Agree",
                                         ifelse(Q2_2<=2, "Disagree",
                                                ifelse(Q2_2==3, "Neither", NA))))
data$Q2_2cat

#Make binary for Question 2 item 2
data <- data %>% mutate(Q2_2cat2 = ifelse(Q2_2 >=4, "Agree",
                                         ifelse(Q2_2<=2, "Disagree", NA)))
data$Q2_2cat2

#Make categorical for Question 2 item 3
data <- data %>% mutate(Q2_3cat = ifelse(Q2_3 >=4, "Agree",
                                         ifelse(Q2_3<=2, "Disagree",
                                                ifelse(Q2_3==3, "Neither", NA))))
data$Q2_3cat

#Make binary for Question 2 item 3
data <- data %>% mutate(Q2_3cat2 = ifelse(Q2_3 >=4, "Agree",
                                          ifelse(Q2_3<=2, "Disagree", NA)))
data$Q2_3cat2

#Make categorical for Question 2 item 4
data <- data %>% mutate(Q2_4cat = ifelse(Q2_4 >=4, "Agree",
                                          ifelse(Q2_4<=2, "Disagree", 
                                                 ifelse(Q2_3==3, "Neither", NA))))
data$Q2_4cat

#Make BINARY for Question 2 item 4
data <- data %>% mutate(Q2_4cat2 = ifelse(Q2_4 >=4, "Agree",
                                         ifelse(Q2_4<=2, "Disagree", NA)))
data$Q2_4cat2

#create categorical for climate factor 1
data$climate_resp1mean=rowMeans(cbind(data$Q14_6, data$Q14_1, data$Q14_5, data$Q14_4, data$Q14_3),na.rm=TRUE)
data <- data %>% mutate(climate_cat_1 = ifelse(climate_resp1mean >=1, "Agree",
                                               ifelse(climate_resp1mean<=-1, "Disagree","Neither")))
data$climate_cat_1

#create categorical for climate factor 2
data$climate_resp2mean=rowMeans(cbind(data$Q14_7, data$Q14_9, data$Q14_8),na.rm=TRUE)
data <- data %>% mutate(climate_cat_2 = ifelse(climate_resp2mean >=1, "Agree",
                                               ifelse(climate_resp2mean<=-1, "Disagree","Neither")))
data$climate_cat_2

#create BINARY for climate factor 1
data$climate_resp1mean=rowMeans(cbind(data$Q14_6, data$Q14_1, data$Q14_5, data$Q14_4, data$Q14_3),na.rm=TRUE)
data <- data %>% mutate(climate_bi_1 = ifelse(climate_resp1mean >=1, "Agree",
                                               ifelse(climate_resp1mean<=-1, "Disagree",NA)))
data$climate_bi_1
data$climate_bi_1 <- as.factor(data$climate_bi_1)

#create BINARY for climate factor 2
data$climate_resp2mean=rowMeans(cbind(data$Q14_7, data$Q14_9, data$Q14_8),na.rm=TRUE)
data <- data %>% mutate(climate_bi_2 = ifelse(climate_resp2mean >=1, "Agree",
                                               ifelse(climate_resp2mean<=-1, "Disagree",NA)))
data$climate_bi_2


#create categorical for climate item 2 "developed countries should accept climate refugees"
data <- data %>% mutate(climate_item2_cat = ifelse(Q14_2 >=1, "Agree",
                                                   ifelse(Q14_2<=-1, "Disagree","Neither")))
data$climate_item2_cat

#create categorical for climate item 3 "cease all dev of fossil fuels"
data <- data %>% mutate(climate_item3_cat = ifelse(Q14_3 >=1, "Agree",
                                                   ifelse(Q14_3<=-1, "Disagree","Neither")))
data$climate_item3_cat

#create categorical for climate item 7 "carbon taxes"
data <- data %>% mutate(climate_item7_cat = ifelse(Q14_7 >=1, "Agree",
                                                   ifelse(Q14_7<=-1, "Disagree","Neither")))
data$climate_item7_cat

#create categorical for climate item 9 "contributions should be proportional to GDP"
data <- data %>% mutate(climate_item9_cat = ifelse(Q14_9 >=1, "Agree",
                                                   ifelse(Q14_9<=-1, "Disagree","Neither")))
data$climate_item9_cat


#create BINARY for climate item 2 "developed countries should accept climate refugees"
data <- data %>% mutate(climate_item2_bi = ifelse(Q14_2 >=1, "Agree",
                                                   ifelse(Q14_2<=-1, "Disagree",NA)))
data$climate_item2_bi

#create BINARY for climate item 3 "cease all dev of fossil fuels"
data <- data %>% mutate(climate_item3_bi = ifelse(Q14_3 >=1, "Agree",
                                                   ifelse(Q14_3<=-1, "Disagree",NA)))
data$climate_item3_bi

#create BINARY for climate item 7 "carbon taxes"
data <- data %>% mutate(climate_item7_bi = ifelse(Q14_7 >=1, "Agree",
                                                   ifelse(Q14_7<=-1, "Disagree",NA)))
data$climate_item7_bi

#create BINARY for climate item 9 "contributions should be proportional to GDP"
data <- data %>% mutate(climate_item9_bi = ifelse(Q14_9 >=1, "Agree",
                                                   ifelse(Q14_9<=-1, "Disagree",NA)))
data$climate_item9_bi



#Trust scale 1 categorical
data$trust1mean=rowMeans(cbind(data$Q12_6, data$Q12_5, data$Q12_5, data$Q12_9, data$Q12_10, 
                               data$Q12_8, data$Q12_11, data$Q12_7),na.rm=TRUE)
data <- data %>% mutate(trust_cat_1 = ifelse(trust1mean >=1, "Agree",
                                             ifelse(trust1mean<=-1, "Disagree","Neither")))
data$trust_cat_1

##make categorical variable for trust factor 2
data$trust2mean=rowMeans(cbind(data$Q12_1, data$Q12_3, data$Q12_2) ,na.rm=TRUE)
data <- data %>% mutate(trust_cat_2 = ifelse(trust2mean >=1, "Agree",
                                             ifelse(trust2mean<=-1, "Disagree","Neither")))
data$trust_cat_2



#Trust scale 1 categorical
data$trust1mean=rowMeans(cbind(data$Q12_6, data$Q12_5, data$Q12_5, data$Q12_9, data$Q12_10, 
                               data$Q12_8, data$Q12_11, data$Q12_7),na.rm=TRUE)
data <- data %>% mutate(trust_bi_1 = ifelse(trust1mean >=1, "Agree",
                                             ifelse(trust1mean<=-1, "Disagree",NA)))
data$trust_bi_1

##make categorical variable for trust factor 2
data$trust2mean=rowMeans(cbind(data$Q12_1, data$Q12_3, data$Q12_2) ,na.rm=TRUE)
data <- data %>% mutate(trust_bi_2 = ifelse(trust2mean >=1, "Agree",
                                             ifelse(trust2mean<=-1, "Disagree",NA)))
data$trust_bi_2




##make categorical variable for governance

data$governancemean=rowMeans(cbind(data$Q13_1, data$Q13_2, data$Q13_3, data$Q13_4, data$Q13_5),na.rm=TRUE)
data <- data %>% mutate(governance_cat_1 = ifelse(governancemean >=1, "Agree",
                                                  ifelse(governancemean<=-1, "Disagree","Neither")))
data$governance_cat_1

##make categorical variable for components factor 1
data$components1mean=rowMeans(cbind(data$Q7_5, data$Q7_4, data$Q7_6),na.rm=TRUE)
data <- data %>% mutate(comp_cat_1 = ifelse(components1mean >=4, "Agree",
                                            ifelse(components1mean<=2, "Disagree","Neither")))
data$comp_cat_1

##make categorical variable for components factor 2
data$components2mean=rowMeans(cbind(data$Q7_2, data$Q7_3, data$Q7_1) ,na.rm=TRUE)
data <- data %>% mutate(comp_cat_2 = ifelse(components2mean >=4, "Agree",
                                            ifelse(components2mean<=2, "Disagree","Neither")))
data$comp_cat_2

##make BINARY variable for components factor 1
data$components1mean=rowMeans(cbind(data$Q7_5, data$Q7_4, data$Q7_6),na.rm=TRUE)
data <- data %>% mutate(comp_bi_1 = ifelse(components1mean >=4, "Agree",
                                            ifelse(components1mean<=2, "Disagree",NA)))
data$comp_bi_1

##make BINARY variable for components factor 2
data$components2mean=rowMeans(cbind(data$Q7_2, data$Q7_3, data$Q7_1) ,na.rm=TRUE)
data <- data %>% mutate(comp_bi_2 = ifelse(components2mean >=4, "Agree",
                                            ifelse(components2mean<=2, "Disagree",NA)))
data$comp_bi_2

##make categorical variable for risk likelihood factor A
data$risk_l.a <- rowMeans(cbind(data$Q8a_4, data$Q8a_3, data$Q8a_7, data$Q8a_6, data$Q8a_5))
data <- data %>% mutate(risk_l.a_cat = ifelse(risk_l.a >=4, "Risks likely",
                                              ifelse(risk_l.a<=2, "Risks unlikely","Neither")))
data$risk_l.a_cat

##make categorical variable for risk likelihood factor B
data$risk_l.b <- rowMeans(cbind(data$Q8a_11, data$Q8a_9, data$Q8a_8, data$Q8a_10))
data <- data %>% mutate(risk_l.b_cat = ifelse(risk_l.b >=4, "Risks likely",
                                              ifelse(risk_l.b<=2, "Risks unlikely","Neither")))
data$risk_l.b_cat

##make categorical variables for risk likelihood "induced earthquake" item
data <- data %>% mutate(risk_l.c_cat = ifelse(Q8a_1 >=4, "Risks likely",
                                              ifelse(Q8a_1 <=2, "Risks unlikely","Neither")))
data$risk_l.c_cat


##make BINARY variable for risk likelihood factor A
data$risk_l.a <- rowMeans(cbind(data$Q8a_4, data$Q8a_3, data$Q8a_7, data$Q8a_6, data$Q8a_5))
data <- data %>% mutate(risk_l.a_bi = ifelse(risk_l.a >=4, "Risks likely",
                                              ifelse(risk_l.a<=2, "Risks unlikely",NA)))
data$risk_l.a_bi

##make BINARY variable for risk likelihood factor B
data$risk_l.b <- rowMeans(cbind(data$Q8a_11, data$Q8a_9, data$Q8a_8, data$Q8a_10))
data <- data %>% mutate(risk_l.b_bi = ifelse(risk_l.b >=4, "Risks likely",
                                              ifelse(risk_l.b<=2, "Risks unlikely",NA)))
data$risk_l.b_bi

##make BINARY variables for risk likelihood "induced earthquake" item
data <- data %>% mutate(risk_l.c_bi = ifelse(Q8a_1 >=4, "Risk likely",
                                              ifelse(Q8a_1 <=2, "Risk unlikely",NA)))
data$risk_l.c_bi

## make categoricals for risk severity factor A

data$risk_s.a <- rowMeans(cbind(data$Q8b_2, data$Q8b_1, data$Q8b_3, data$Q8b_4, data$Q8b_5,
                                data$Q8b_7, data$Q8b_6))
data <- data %>% mutate(risk_s.a_cat = ifelse(risk_s.a >=4, "Risks severe",
                                              ifelse(risk_s.a<=2, "Risks not severe","Neither")))
data$risk_s.a_cat         

# make categoricals for risk severity factor B
data$risk_s.b <- rowMeans(cbind(data$Q8b_11, data$Q8b_9, data$Q8b_10, data$Q8b_8))
data <- data %>% mutate(risk_s.b_cat = ifelse(risk_s.b >=4, "Risks severe",
                                              ifelse(risk_s.b<=2, "Risks not severe","Neither")))
data$risk_s.b_cat  


## make BINARY for risk severity factor A

data$risk_s.a <- rowMeans(cbind(data$Q8b_2, data$Q8b_1, data$Q8b_3, data$Q8b_4, data$Q8b_5,
                                data$Q8b_7, data$Q8b_6))
data <- data %>% mutate(risk_s.a_bi = ifelse(risk_s.a >=4, "Risks severe",
                                              ifelse(risk_s.a<=2, "Risks not severe",NA)))
data$risk_s.a_bi         

# make BINARY for risk severity factor A
data$risk_s.b <- rowMeans(cbind(data$Q8b_11, data$Q8b_9, data$Q8b_10, data$Q8b_8))
data <- data %>% mutate(risk_s.b_bi = ifelse(risk_s.b >=4, "Risks severe",
                                              ifelse(risk_s.b<=2, "Risks not severe",NA)))
data$risk_s.b_bi  


#### Likert style plot of support, by expert ####
likert_recode <- function(x) {
  y <- ifelse(is.na(x), NA,
              ifelse(x == -2, "Strongly oppose",
                     ifelse(x == -1, "Oppose",
                            ifelse(x == 0, "Neither oppose nor support",
                                   ifelse(x == 1, "Support", "Strongly support")))))
  
  y <- factor(y, levels = c("Strongly oppose", "Oppose", 
                            "Neither oppose nor support", 
                            "Support",
                            "Strongly support"))
  
  return(y)
}
support_ <- cbind(support_, data$expert)

#NETs_grp1 <- NETs_new %>%
#filter(data$expert==2) #MODIFY GROUPING-EXPERT AFFILIATION

support_likert <- support_[,1] %>%
  mutate_all(likert_recode) %>%
  likert(grouping = data$expert)

#NETs_likert <- NETs %>%
#mutate_all(likert_recode) %>%
#likert(grouping = data$expert)
#likert()

plot(support_likert, as.percent = TRUE, panel.strip.color="dark grey", centered = TRUE, positive.order = TRUE)









##### Likert style plot of worthiness of NETS #####

NETs <- data[c("alkalin", "fertil", "weathering", "biochar", "DAC_offshore", 
               "DAC_onshore", "DAC", "afforest", "soil", "coast_rest")]

names(NETs) <- c(alkalin = "Ocean alkalinization",
                 fertil = "Ocean fertilization",
                 weathering = "Enhanced weathering",
                 biochar = "Biochar",
                 DAC_offshore = "DAC offshore",
                 DAC_onshore = "DAC onshore",
                 DAC = "Direct air capture (DAC)",
                 afforest = "Afforestation",
                 soil = "Soil carbon sequestration",
                 coast_rest = "Coastal restoration")



likert_recode <- function(x) {
  y <- ifelse(is.na(x), "Don't know/not sure",
              ifelse(x == -2, "Not at all worthy of consideration",
                     ifelse(x == -1, "Somewhat worthy of consideration",
                            ifelse(x == 0, "Moderately worthy of consideration",
                                   ifelse(x == 1, "Very worthy of consideration", "Absolutely worthy of consideration")))))
  
  y <- factor(y, levels = c("Don't know/not sure", "Not at all worthy of consideration", 
                            "Somewhat worthy of consideration", 
                            "Moderately worthy of consideration",
                            "Very worthy of consideration",
                            "Absolutely worthy of consideration"))
  
  return(y)
}
#NETs_new <- cbind(NETs, data$expert)

#NETs_grp1 <- NETs_new %>%
  #filter(data$expert==2) #MODIFY GROUPING-EXPERT AFFILIATION

#NETs_likert2 <- NETs_new2[,c(1:10)] %>%
 # mutate_all(likert_recode) %>%
  # likert(grouping = data$expert)

NETs_likert <- NETs %>%
  mutate_all(likert_recode) %>%
  #likert(grouping = data$expert)
  likert()

plot(NETs_likert, as.percent = TRUE, panel.strip.color="dark grey", centered = TRUE, positive.order = TRUE)


##### Likert-style plot of Need for NETs#####


needNETs <- data[c("Q2_1", "Q2_2", "Q2_3", "Q2_4")]

names(needNETs) <- c("Q2_1" = "Climate modelling indicates that 
                 changes in what humans consume can stabilize 
                 warming at 1.5 degrees by 2050", "Q2_2" = 
                     "Climate modelling indicates that removal of 
                 atmospheric carbon through Negative Emissions 
                 Technologies (NETs) is necessary to stabilize 
                 warming at 1.5 degrees by 2050", "Q2_3" = "If 
                 Negative Emissions Technologies (NETs) are needed, 
                 those based on natural processes will be sufficient 
                 to stabilize warming at 1.5 degrees by 2050", "Q2_4" 
                   = "Negative Emissions Technologies (NETs) based on 
                 technologically-engineered processes will also be 
                 needed to stabilize warming at 1.5 degrees by 2050")



likert_recode <- function(x) {
  y <- ifelse(is.na(x), NA,
              ifelse(x == 1, "Strongly disagree",
                     ifelse(x == 2, "Disagree",
                            ifelse(x == 3, "Neither agree nor disagree",
                                   ifelse(x == 4, "Agree", "Strongly agree")))))
  
  y <- factor(y, levels = c("Strongly disagree", 
                            "Disagree", 
                            "Neither agree nor disagree",
                            "Agree",
                            "Strongly agree"))
  
  return(y)
}

#needNETs_new <- cbind(needNETs, data$expert) #use this is breaking down by expert group

#needNETs_likert <- needNETs[,c(1:4)] %>% # ditto above
needNETs_likert <- needNETs %>%
  mutate_all(likert_recode) %>%
  likert() 

plot(needNETs_likert, as.percent = TRUE, panel.strip.color="dark grey")


##### Likert style plot of risks likelihood #####

risks_l <- data[c("Q8a_1", "Q8a_2", "Q8a_3", "Q8a_4", "Q8a_5", "Q8a_6", "Q8a_7", "Q8a_8", "Q8a_9", "Q8a_10", "Q8a_11")]

names(risks_l) <- c("Q8a_1" = "Induced earthquakes", "Q8a_2" = "Occurence of earthquakes
                  from site seismicity", "Q8a_3" = "Explosive discharge of CO2
                  from solid rock", "Q8a_4" = "Adverse consequences to deep ocean systems",
                  "Q8a_5" = "Negative consequences for human health",
                  "Q8a_6" = "Unnoticed leakage of CO2 from porous rock to the surface", 
                  "Q8a_7" = "Ocean pollution", "Q8a_8" = "Diversion of funds that could be 
                  used on better solutions", "Q8a_9" = "Continued dependence on fossil
                  fuels", "Q8a_10" = "High cost overruns", "Q8a_11" = "Reduced development 
                  of renewable energy")



likert_recode <- function(x) {
  y <- ifelse(is.na(x), NA,
              ifelse(x == 1, "Extremely unlikely",
                     ifelse(x == 2, "Unlikely",
                            ifelse(x == 3, "Equally unlikely/likely",
                                   ifelse(x == 4, "Likely", "Extremely likely")))))
  
  y <- factor(y, levels = c("Extremely unlikely", 
                            "Unlikely", 
                            "Equally unlikely/likely",
                            "Likely",
                            "Extremely likely"))
  
  return(y)
}
risks_l<- cbind(risks_l, data$expert)

risks_l_likert <- risks_l[,c(1:11)]  %>%
  mutate_all(likert_recode) %>%
  #likert()
likert(grouping = data$expert) 

risk_l_plot <- plot(risks_l_likert, as.percent = TRUE, ordered = TRUE, centered = TRUE, panel.strip.color="dark grey")



##### Likert style plot of risk severity #####

risks_s <- data[c("Q8b_1", "Q8b_2", "Q8b_3", "Q8b_4", "Q8b_5", "Q8b_6", 
                  "Q8b_7", "Q8b_8", "Q8b_9", "Q8b_10", "Q8b_11")]

names(risks_s) <- c("Q8b_1" = "Induced earthquakes", "Q8b_2" = "Occurence of earthquakes
                  from site seismicity", "Q8b_3" = "Explosive discharge of CO2
                  from solid rock", "Q8b_4" = "Adverse consequences to deep ocean systems",
                  "Q8b_5" = "Negative consequences for human health",
                  "Q8b_6" = "Unnoticed leakage of CO2 from porous rock to the surface", 
                  "Q8a_7" = "Ocean pollution", "Q8b_8" = "Diversion of funds that could be 
                  used on better solutions", "Q8b_9" = "Continued dependence on fossil
                  fuels", "Q8b_10" = "High cost overruns", "Q8b_11" = "Reduced development 
                  of renewable energy")



likert_recode <- function(x) {
  y <- ifelse(is.na(x), NA,
              ifelse(x == 1, "Extremely unlikely",
                     ifelse(x == 2, "Unlikely",
                            ifelse(x == 3, "Equally unlikely/likely",
                                   ifelse(x == 4, "Likely", "Extremely likely")))))
  
  y <- factor(y, levels = c("Extremely unlikely", 
                            "Unlikely", 
                            "Equally unlikely/likely",
                            "Likely",
                            "Extremely likely"))
  
  return(y)
}

risks_s <- cbind(risks_s, data$expert)

risks_s_likert <- risks_s[,c(1:11)] %>%
  mutate_all(likert_recode) %>%
  #likert()
likert(grouping = data$expert) 

plot(risks_s_likert, as.percent = TRUE, ordered = TRUE, centered = TRUE, panel.strip.color="dark grey")


##### Likert style plot of benefit likelihood #####

benefits_l <- data[c("Q9a_1", "Q9a_2", "Q9a_3", "Q9a_4", "Q9a_5", "Q9a_6", 
                     "Q9a_7", "Q9a_8", "Q9a_9", "Q9a_10")]

names(benefits_l) <- c("Q9a_1" = "Solid Carbon turns out to be a safe solution for addressing climate change", 
                     "Q9a_2" = "Solid Carbon turns out to be effective for reducing the concentration of CO2 in the atmosphere",
                     "Q9a_3" = "Solid Carbon turns out to be a good use of revenues from carbon taxes",
                     "Q9a_4" = "Solid Carbon turns out to provide skilled employment for people formerly employed in the oil and gas industry",
                  "Q9a_5" = "Solid Carbon turns out to benefit future generations",
                  "Q9a_6" = "Solid Carbon turns out to be successful at removing atmospheric CO2", 
                  "Q9a_7" = "Solid Carbon turns out to be a cost-effective means of addressing climate change", 
                     "Q9a_8" = "Solid Carbon turns out to be an important component of a ‘green’ economy", 
                     "Q9a_9" = "Solid Carbon turns out to address diminishing availability of lands suitable for afforestation", 
                     "Q9a_10" = "Solid Carbon turns out to enable CO2 removal independent of the fossil fuel industry")



likert_recode <- function(x) {
  y <- ifelse(is.na(x), NA,
              ifelse(x == 1, "Extremely unlikely",
                     ifelse(x == 2, "Unlikely",
                            ifelse(x == 3, "Equally unlikely/likely",
                                   ifelse(x == 4, "Likely", "Extremely likely")))))
  
  y <- factor(y, levels = c("Extremely unlikely", 
                            "Unlikely", 
                            "Equally unlikely/likely",
                            "Likely",
                            "Extremely likely"))
  
  return(y)
}

benefits_l <- cbind(benefits_l, data$expert)

benefits_l_likert <- benefits_l[,c(1:10)] %>%
  mutate_all(likert_recode) %>%
  #likert()
likert(grouping = data$expert) 

plot(benefits_l_likert, as.percent = TRUE, ordered = TRUE, centered = TRUE, panel.strip.color="dark grey")

##### Likert style plot of Importance of benefits #####

benefits_i <- data[c("Q9b_1", "Q9b_2", "Q9b_3", "Q9b_4", "Q9b_5", "Q9b_6", 
                     "Q9b_7", "Q9b_8", "Q9b_9", "Q9b_10")]

names(benefits_i) <- c("Q9b_1" = "Solid Carbon turns out to be a safe solution for addressing climate change", 
                       "Q9b_2" = "Solid Carbon turns out to be effective for reducing the concentration of CO2 in the atmosphere",
                       "Q9b_3" = "Solid Carbon turns out to be a good use of revenues from carbon taxes",
                       "Q9b_4" = "Solid Carbon turns out to provide skilled employment for people formerly employed in the oil and gas industry",
                       "Q9b_5" = "Solid Carbon turns out to benefit future generations",
                       "Q9b_6" = "Solid Carbon turns out to be successful at removing atmospheric CO2", 
                       "Q9b_7" = "Solid Carbon turns out to be a cost-effective means of addressing climate change", 
                       "Q9b_8" = "Solid Carbon turns out to be an important component of a ‘green’ economy", 
                       "Q9b_9" = "Solid Carbon turns out to address diminishing availability of lands suitable for afforestation", 
                       "Q9b_10" = "Solid Carbon turns out to enable CO2 removal independent of the fossil fuel industry")



likert_recode <- function(x) {
  y <- ifelse(is.na(x), NA,
              ifelse(x == 1, "Extremely unlikely",
                     ifelse(x == 2, "Unlikely",
                            ifelse(x == 3, "Equally unlikely/likely",
                                   ifelse(x == 4, "Likely", "Extremely likely")))))
  
  y <- factor(y, levels = c("Extremely unlikely", 
                            "Unlikely", 
                            "Equally unlikely/likely",
                            "Likely",
                            "Extremely likely"))
  
  return(y)
}

benefits_i <- cbind(benefits_i, data$expert)

benefits_i_likert <- benefits_i[,c(1:10)] %>%
  mutate_all(likert_recode) %>%
  #likert()
likert(grouping = data$expert) 

plot(benefits_i_likert, as.percent = TRUE, ordered = TRUE, centered = TRUE, panel.strip.color="dark grey")


##### Likert style plot of Climate responsibility ####
climate_resp <- data[c("Q14_1", "Q14_2", "Q14_3", "Q14_4", "Q14_5", 
                       "Q14_6", "Q14_7", "Q14_8", "Q14_9")]
names(climate_resp) <- c("Q14_1" = "We shouldn't expect high carbon-emitting countries to 
                         cover the climate mitigation costs of lesser emitting countries (REVERSE)", 
                       "Q14_2" = "Developed countries should be responsible for accepting 
                       climate refugees from parts of the world undergoing severe drought, 
                       salinization of irrigated lands, or other climate effects that may 
                       render these regions uninhabitable",
                       "Q14_3" = "It is unrealistic to expect wealthy countries to cease 
                       all fossil fuel development by 2050 (REVERSE)",
                       "Q14_4" = "Morally speaking, the onus for bearing the costs of 
                       climate change should rest primarily with individual consumers as 
                       opposed to government and industry (REVERSE)",
                       "Q14_5" = "Carbon emission reduction and/or removal technologies 
                       should be designed to match the volume of emissions produced in each 
                       region (e.g., state or province)",
                       "Q14_6" = "No one country should be expected to reduce or remove 
                       carbon beyond their own national carbon footprint (REVERSE)", 
                       "Q14_7" = "Carbon taxes in wealthier countries should be used to 
                       finance the climate mitigation needs of less wealthy countries", 
                       "Q14_8" = "Coastal areas of least-developed nations should be off 
                       limits for ocean-based climate solutions unless the benefits to the 
                       host nation are significant", 
                       "Q14_9" = "Nations should contribute to climate mitigation in a manner 
                       that is proportional to their per capita GDP")


likert_recode <- function(x) {
  y <- ifelse(is.na(x), NA,
              ifelse(x == 1, "Strongly disagree",
                     ifelse(x == 2, "Disagree",
                            ifelse(x == 3, "Neither agree nor disagree",
                                   ifelse(x == 4, "Agree", "Strongly agree")))))
  
  y <- factor(y, levels = c("Strongly disagree", 
                            "Disagree", 
                            "Neither agree nor disagree",
                            "Agree",
                            "Strongly agree"))
  
  return(y)
}

climate_resp_likert <- climate_resp %>%
  mutate_all(likert_recode) %>%
  likert()

plot(climate_resp_likert, as.percent = TRUE, ordered = TRUE, centered = TRUE, panel.strip.color="dark grey")




##### ?? Make bar charts of descriptive stats ####


### HOW TO MAKE THIS WORK???
NETmeans <- data %>%
  summarise(mean_DAC = mean(DAC, na.rm=TRUE), mean_DAC_offshore = mean(DAC_offshore, na.rm=TRUE),
            mean_DAC_onshore = mean(DAC_onshore, na.rm=TRUE), mean_alkalin = mean(alkalin, na.rm=TRUE),
            mean_weathering = mean(weathering, na.rm=TRUE), mean_fertil = mean(fertil, na.rm=TRUE),
            mean_coast_rest = mean(coast_rest, na.rm=TRUE), mean_afforest = mean(afforest, na.rm=TRUE),
            mean_biochar = mean(biochar, na.rm=TRUE), mean_soil = mean(soil, na.rm=TRUE)) %>%
  gather()
NETse <- data %>%
  summarise(se_DAC = sd(data$DAC, na.rm=TRUE)/sqrt(length(data$DAC)), 
            se_DAC_offshore = sd(data$DAC_offshore, na.rm=TRUE)/sqrt(length(data$DAC_offshore)),
            se_DAC_onshore = sd(data$DAC_onshore, na.rm=TRUE)/sqrt(length(data$DAC_onshore)),
            se_alkalin = sd(data$alkalin, na.rm=TRUE)/sqrt(length(data$alkalin)),
            se_weathering = sd(data$weathering, na.rm=TRUE)/sqrt(length(data$weathering)),
            se_fertil = sd(data$fertil, na.rm=TRUE)/sqrt(length(data$fertil)),
            se_coast_rest = sd(data$coast_rest, na.rm=TRUE)/sqrt(length(data$coast_rest)),
            se_afforest = sd(data$afforest, na.rm=TRUE)/sqrt(length(data$afforest)),
            se_biochar = sd(data$biochar, na.rm=TRUE)/sqrt(length(data$biochar)),
            se_soil = sd(data$soil, na.rm=TRUE)/sqrt(length(data$soil))) %>%
  gather ()
###using the non-dplyr way that I know how...

#### Risk mean scores ####

risk <- c("Induced earthquakes", "Occurrence of earthquakes from site seismicity", 
                     "Explosive discharge of CO2 from solid rock", "Adverse consequences to deep ocean systems",
                     "Negative consequences for human health", "Unnoticed leakage of CO2 from the porous rock to the surface",
                     "Ocean pollution", "Diversion of funds that could be used on better solutions",
                     "Continued dependence on fossil fules", "High cost overruns",
                     "Reduced development of renewable energy")
risk_l_mean <- c(mean(data$Q8a_1, na.rm=TRUE), mean(data$Q8a_2, na.rm=TRUE),
             mean(data$Q8a_3, na.rm=TRUE), mean(data$Q8a_4, na.rm=TRUE),
             mean(data$Q8a_5, na.rm=TRUE), mean(data$Q8a_6, na.rm=TRUE),
             mean(data$Q8a_7, na.rm=TRUE), mean(data$Q8a_8, na.rm=TRUE),
             mean(data$Q8a_9, na.rm=TRUE), mean(data$Q8a_10, na.rm=TRUE),
             mean(data$Q8a_11, na.rm=TRUE))
risk_s_mean <- c(mean(data$Q8b_1, na.rm=TRUE), mean(data$Q8b_2, na.rm=TRUE),
                 mean(data$Q8b_3, na.rm=TRUE), mean(data$Q8b_4, na.rm=TRUE),
                 mean(data$Q8b_5, na.rm=TRUE), mean(data$Q8b_6, na.rm=TRUE),
                 mean(data$Q8b_7, na.rm=TRUE), mean(data$Q8b_8, na.rm=TRUE),
                 mean(data$Q8b_9, na.rm=TRUE), mean(data$Q8b_10, na.rm=TRUE),
                 mean(data$Q8b_11, na.rm=TRUE))
risk_l_se <- c(sd(data$Q8a_1, na.rm=TRUE)/sqrt(length(data$Q8a_1)),
           sd(data$Q8a_2, na.rm=TRUE)/sqrt(length(data$Q8a_2)),
           sd(data$Q8a_3, na.rm=TRUE)/sqrt(length(data$Q8a_3)),
           sd(data$Q8a_4, na.rm=TRUE)/sqrt(length(data$Q8a_5)),
           sd(data$Q8a_5, na.rm=TRUE)/sqrt(length(data$Q8a_5)),
           sd(data$Q8a_6, na.rm=TRUE)/sqrt(length(data$Q8a_6)),
           sd(data$Q8a_7, na.rm=TRUE)/sqrt(length(data$Q8a_7)),
           sd(data$Q8a_8, na.rm=TRUE)/sqrt(length(data$Q8a_8)),
           sd(data$Q8a_9, na.rm=TRUE)/sqrt(length(data$Q8a_9)),
           sd(data$Q8a_10, na.rm=TRUE)/sqrt(length(data$Q8a_10)),
           sd(data$Q8a_11, na.rm=TRUE)/sqrt(length(data$Q8a_11)))
risk_s_se <- c(sd(data$Q8b_1, na.rm=TRUE)/sqrt(length(data$Q8b_1)),
               sd(data$Q8b_2, na.rm=TRUE)/sqrt(length(data$Q8b_2)),
               sd(data$Q8b_3, na.rm=TRUE)/sqrt(length(data$Q8b_3)),
               sd(data$Q8b_4, na.rm=TRUE)/sqrt(length(data$Q8b_5)),
               sd(data$Q8b_5, na.rm=TRUE)/sqrt(length(data$Q8b_5)),
               sd(data$Q8b_6, na.rm=TRUE)/sqrt(length(data$Q8b_6)),
               sd(data$Q8b_7, na.rm=TRUE)/sqrt(length(data$Q8b_7)),
               sd(data$Q8b_8, na.rm=TRUE)/sqrt(length(data$Q8b_8)),
               sd(data$Q8b_9, na.rm=TRUE)/sqrt(length(data$Q8b_9)),
               sd(data$Q8b_10, na.rm=TRUE)/sqrt(length(data$Q8b_10)),
               sd(data$Q8b_11, na.rm=TRUE)/sqrt(length(data$Q8b_11)))


Risk_summary <- data.frame(risk, risk_l_mean, risk_s_mean, risk_l_se, risk_s_se)

kruskal.test(risk_l)
kruskal.test(risk_s)
dunn_test()

#convert to long format for plotting

Risk_summary <- gather(Risk_summary, type, risk_mean, risk_l_mean:risk_s_mean)
Risk_summary

Risk_summary <- gather(Risk_summary, se_type, risk_se, risk_l_se:risk_s_se)
Risk_summary

Risk_summary_plot <- ggplot(data=Risk_summary, 
                          aes(x=reorder(risk, -risk_mean), 
                              y=risk_mean, 
                              ymin=(risk_mean-risk_se), 
                              ymax=(risk_mean+risk_se), 
                              fill=type)) +
  geom_bar(stat="identity", position='dodge') +
  geom_errorbar(width=.5, position= position_dodge(.9)) +
  labs(y= "Average response (Likelihood: Extremely unlikely to Extremely Likely,
       Severity: Not at all severe to extremely severe)",
       x= "") +
  scale_fill_discrete(name="", labels = c("Likelihood", "Severity")) +
  coord_flip()

Risk_summary_plot



##### Benefit mean scores ####
benefit <- c("Solid Carbon turns out to be a safe solution for addressing climate change",
             "Solid Carbon turns out to be effective for reducing the concentration of CO2 in the atmosphere",
             "Solid Carbon turns out to be a good use of revenues from carbon taxes",
             "Solid Carbon turns out to provide skilled employment for people formerly employed in the oil and gas industry",
             "Solid Carbon turns out to benefit future generations",
             "Solid Carbon turns out to be successful at removing atmospheric CO2",
             "Solid Carbon turns out to be a cost-effective means of addressing climate change",
             "Solid Carbon turns out to be an important component of a ‘green’ economy'",
             "Solid Carbon turns out to address diminishing availability of lands suitable for afforestation",
             "Solid Carbon turns out to enable CO2 removal independent of the fossil fuel industry")
benefit_l_mean <- c(mean(data$Q9a_1, na.rm=TRUE), mean(data$Q9a_2, na.rm=TRUE),
                 mean(data$Q9a_3, na.rm=TRUE), mean(data$Q9a_4, na.rm=TRUE),
                 mean(data$Q9a_5, na.rm=TRUE), mean(data$Q9a_6, na.rm=TRUE),
                 mean(data$Q9a_7, na.rm=TRUE), mean(data$Q9a_8, na.rm=TRUE),
                 mean(data$Q9a_9, na.rm=TRUE), mean(data$Q9a_10, na.rm=TRUE))
benefit_i_mean <- c(mean(data$Q9b_1, na.rm=TRUE), mean(data$Q9b_2, na.rm=TRUE),
                 mean(data$Q9b_3, na.rm=TRUE), mean(data$Q9b_4, na.rm=TRUE),
                 mean(data$Q9b_5, na.rm=TRUE), mean(data$Q9b_6, na.rm=TRUE),
                 mean(data$Q9b_7, na.rm=TRUE), mean(data$Q9b_8, na.rm=TRUE),
                 mean(data$Q9b_9, na.rm=TRUE), mean(data$Q9b_10, na.rm=TRUE))
benefit_l_se <- c(sd(data$Q9a_1, na.rm=TRUE)/sqrt(length(data$Q9a_1)),
               sd(data$Q9a_2, na.rm=TRUE)/sqrt(length(data$Q9a_2)),
               sd(data$Q9a_3, na.rm=TRUE)/sqrt(length(data$Q9a_3)),
               sd(data$Q9a_4, na.rm=TRUE)/sqrt(length(data$Q9a_5)),
               sd(data$Q9a_5, na.rm=TRUE)/sqrt(length(data$Q9a_5)),
               sd(data$Q9a_6, na.rm=TRUE)/sqrt(length(data$Q9a_6)),
               sd(data$Q9a_7, na.rm=TRUE)/sqrt(length(data$Q9a_7)),
               sd(data$Q9a_8, na.rm=TRUE)/sqrt(length(data$Q9a_8)),
               sd(data$Q9a_9, na.rm=TRUE)/sqrt(length(data$Q9a_9)),
               sd(data$Q9a_10, na.rm=TRUE)/sqrt(length(data$Q9a_10)))
benefit_i_se <- c(sd(data$Q9b_1, na.rm=TRUE)/sqrt(length(data$Q9b_1)),
               sd(data$Q9b_2, na.rm=TRUE)/sqrt(length(data$Q9b_2)),
               sd(data$Q9b_3, na.rm=TRUE)/sqrt(length(data$Q9b_3)),
               sd(data$Q9b_4, na.rm=TRUE)/sqrt(length(data$Q9b_5)),
               sd(data$Q9b_5, na.rm=TRUE)/sqrt(length(data$Q9b_5)),
               sd(data$Q9b_6, na.rm=TRUE)/sqrt(length(data$Q9b_6)),
               sd(data$Q9b_7, na.rm=TRUE)/sqrt(length(data$Q9b_7)),
               sd(data$Q9b_8, na.rm=TRUE)/sqrt(length(data$Q9b_8)),
               sd(data$Q9b_9, na.rm=TRUE)/sqrt(length(data$Q9b_9)),
               sd(data$Q9b_10, na.rm=TRUE)/sqrt(length(data$Q9b_10)))


Benefit_summary <- data.frame(benefit, benefit_l_mean, benefit_i_mean, benefit_l_se, benefit_i_se)

#convert to long format for plotting

Benefit_summary <- gather(Benefit_summary, type, benefit_mean, benefit_l_mean:benefit_i_mean)
Benefit_summary

Benefit_summary <- gather(Benefit_summary, se_type, benefit_se, benefit_l_se:benefit_i_se)
Benefit_summary

Benefit_summary_plot <- ggplot(data=Benefit_summary, 
                            aes(x=reorder(benefit, -benefit_mean), 
                                y=benefit_mean, 
                                ymin=(benefit_mean-benefit_se), 
                                ymax=(benefit_mean+benefit_se), 
                                fill=type)) +
  geom_bar(stat="identity", position='dodge') +
  geom_errorbar(width=.5, position= position_dodge(.9)) +
  labs(y= "Average response (Likelihood: Extremely unlikely to Extremely Likely,
       Importance: Not at all important for society to extremely important for society)",
       x= "") +
  scale_fill_discrete(name="", labels = c("Likelihood", "Importance")) +
  coord_flip()

Benefit_summary_plot




##### Risks, by Q2_3 ####
Risk_Q2_3 <- data %>% 
  group_by(Q2_3cat) %>%
  summarize(risk_l_1 = mean(Q8a_1, na.rm=TRUE), 
            risk_l_2 = mean(Q8a_2, na.rm=TRUE),
            risk_l_3 = mean(Q8a_3, na.rm=TRUE),
            risk_l_4 = mean(Q8a_4, na.rm=TRUE), 
            risk_l_5 = mean(Q8a_5, na.rm=TRUE),
            risk_l_6 = mean(Q8a_6, na.rm=TRUE),
            risk_l_7 = mean(Q8a_7, na.rm=TRUE), 
            risk_l_8 = mean(Q8a_8, na.rm=TRUE),
            risk_l_9 = mean(Q8a_9, na.rm=TRUE),
            risk_l_10 = mean(Q8a_10, na.rm=TRUE), 
            risk_l_11 = mean(Q8a_11, na.rm=TRUE)) %>%
  gather("risk_type", "mean_likelihood", risk_l_1:risk_l_11)

Risk_Q2_3


Risk_Q2_3$risk_type <- as.factor(Risk_Q2_3$risk_type)
levels(Risk_Q2_3$risk_type) <- c("Induced earthquakes", "Occurrence of earthquakes from site seismicity", 
                                        "Explosive discharge of CO2 from solid rock", "Adverse consequences to deep ocean systems",
                                        "Negative consequences for human health", "Unnoticed leakage of CO2 from the porous rock to the surface",
                                        "Ocean pollution", "Diversion of funds that could be used on better solutions",
                                        "Continued dependence on fossil fules", "High cost overruns",
                                        "Reduced development of renewable energy")

Risk_Q2_3plot <- ggplot(data=Risk_Q2_3, aes((reorder(risk_type, -mean_likelihood)), mean_likelihood, fill = Q2_3cat)) +
  geom_bar(stat="identity", position="dodge") +
  labs(x = "", y = "Perceived Likelihood of risks (1=Not at all likely, 5=Extremely likely)",
       fill="Are nature-based NETs sufficient?") +
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white") + 
  coord_flip()

Risk_Q2_3plot


#### Risks, by Q2_3 ####

risk_s_Q2_3 <- data %>% 
  group_by(Q2_3cat) %>%
  summarize(risk_s_1 = mean(Q8b_1, na.rm=TRUE),
            risk_s_2 = mean(Q8b_2, na.rm=TRUE),
            risk_s_3 = mean(Q8b_3, na.rm=TRUE),
            risk_s_4 = mean(Q8b_4, na.rm=TRUE),
            risk_s_5 = mean(Q8b_5, na.rm=TRUE),
            risk_s_6 = mean(Q8b_6, na.rm=TRUE),
            risk_s_7 = mean(Q8b_7, na.rm=TRUE),
            risk_s_8 = mean(Q8b_8, na.rm=TRUE),
            risk_s_9 = mean(Q8b_9, na.rm=TRUE),
            risk_s_10 = mean(Q8b_10, na.rm=TRUE),
            risk_s_11 = mean(Q8b_11, na.rm=TRUE)) %>%
  gather("risk_type", "mean_likelihood", risk_s_1:risk_s_11)

risk_s_Q2_3


risk_s_Q2_3$risk_type <- as.factor(risk_s_Q2_3$risk_type)
levels(risk_s_Q2_3$risk_type) <- c("Induced earthquakes", "Occurrence of earthquakes from site seismicity", 
                                 "Explosive discharge of CO2 from solid rock", "Adverse consequences to deep ocean systems",
                                 "Negative consequences for human health", "Unnoticed leakage of CO2 from the porous rock to the surface",
                                 "Ocean pollution", "Diversion of funds that could be used on better solutions",
                                 "Continued dependence on fossil fules", "High cost overruns",
                                 "Reduced development of renewable energy")

risk_s_Q2_3plot <- ggplot(data=risk_s_Q2_3, aes((reorder(risk_type, -mean_likelihood)), mean_likelihood, fill = Q2_3cat)) +
  geom_bar(stat="identity", position="dodge") +
  labs(x = "", y = "Perceived severity of risks (1=Not at all severe, 5=Extremely severe)",
       fill="Are nature-based NETs sufficient?") +
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white") + 
  coord_flip()

risk_s_Q2_3plot











#### Plot benefits broken down by Q2_3

benefit_l_Q2_3 <- data %>% 
  group_by(Q2_3cat) %>%
  summarize(benefit_l_1 = mean(Q9a_1, na.rm=TRUE),
            benefit_l_2 = mean(Q9a_2, na.rm=TRUE),
            benefit_l_3 = mean(Q9a_3, na.rm=TRUE),
            benefit_l_4 = mean(Q9a_4, na.rm=TRUE),
            benefit_l_5 = mean(Q9a_5, na.rm=TRUE),
            benefit_l_6 = mean(Q9a_6, na.rm=TRUE),
            benefit_l_7 = mean(Q9a_7, na.rm=TRUE),
            benefit_l_8 = mean(Q9a_8, na.rm=TRUE),
            benefit_l_9 = mean(Q9a_9, na.rm=TRUE),
            benefit_l_10 = mean(Q9a_10, na.rm=TRUE)) %>%
  gather("benefit_type", "mean_likelihood", benefit_l_1:benefit_l_10)

benefit_l_Q2_3


benefit_l_Q2_3$benefit_type <- as.factor(benefit_l_Q2_3$benefit_type)
levels(benefit_l_Q2_3$benefit_type) <- c("Solid Carbon turns out to be a safe solution for addressing climate change",
                                         "Solid Carbon turns out to be effective for reducing the concentration of CO2 in the atmosphere",
                                         "Solid Carbon turns out to be a good use of revenues from carbon taxes",
                                         "Solid Carbon turns out to provide skilled employment for people formerly employed in the oil and gas industry",
                                         "Solid Carbon turns out to benefit future generations",
                                         "Solid Carbon turns out to be successful at removing atmospheric CO2",
                                         "Solid Carbon turns out to be a cost-effective means of addressing climate change",
                                         "Solid Carbon turns out to be an important component of a ‘green’ economy'",
                                         "Solid Carbon turns out to address diminishing availability of lands suitable for afforestation",
                                         "Solid Carbon turns out to enable CO2 removal independent of the fossil fuel industry")

benefit_l_Q2_3plot <- ggplot(data=benefit_l_Q2_3, aes((reorder(benefit_type, -mean_likelihood)), mean_likelihood, fill = Q2_3cat)) +
  geom_bar(stat="identity", position="dodge") +
  labs(x = "", y = "Perceived likelihood of benefits (1=Not at all likely, 5=Extremely likely)",
       fill="Are nature-based NETs sufficient?") +
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white") + 
  coord_flip()

benefit_l_Q2_3plot



benefit_i_Q2_3 <- data %>% 
  group_by(Q2_3cat) %>%
  summarize(benefit_i_1 = mean(Q9b_1, na.rm=TRUE),
            benefit_i_2 = mean(Q9b_2, na.rm=TRUE),
            benefit_i_3 = mean(Q9b_3, na.rm=TRUE),
            benefit_i_4 = mean(Q9b_4, na.rm=TRUE),
            benefit_i_5 = mean(Q9b_5, na.rm=TRUE),
            benefit_i_6 = mean(Q9b_6, na.rm=TRUE),
            benefit_i_7 = mean(Q9b_7, na.rm=TRUE),
            benefit_i_8 = mean(Q9b_8, na.rm=TRUE),
            benefit_i_9 = mean(Q9b_9, na.rm=TRUE),
            benefit_i_10 = mean(Q9b_10, na.rm=TRUE)) %>%
  gather("benefit_type", "mean_likelihood", benefit_i_1:benefit_i_10)

benefit_i_Q2_3


benefit_i_Q2_3$benefit_type <- as.factor(benefit_i_Q2_3$benefit_type)
levels(benefit_i_Q2_3$benefit_type) <- c("Solid Carbon turns out to be a safe solution for addressing climate change",
                                         "Solid Carbon turns out to be effective for reducing the concentration of CO2 in the atmosphere",
                                         "Solid Carbon turns out to be a good use of revenues from carbon taxes",
                                         "Solid Carbon turns out to provide skilled employment for people formerly employed in the oil and gas industry",
                                         "Solid Carbon turns out to benefit future generations",
                                         "Solid Carbon turns out to be successful at removing atmospheric CO2",
                                         "Solid Carbon turns out to be a cost-effective means of addressing climate change",
                                         "Solid Carbon turns out to be an important component of a ‘green’ economy'",
                                         "Solid Carbon turns out to address diminishing availability of lands suitable for afforestation",
                                         "Solid Carbon turns out to enable CO2 removal independent of the fossil fuel industry")

benefit_i_Q2_3plot <- ggplot(data=benefit_i_Q2_3, aes((reorder(benefit_type, -mean_likelihood)), mean_likelihood, fill = Q2_3cat)) +
  geom_bar(stat="identity", position="dodge") +
  labs(x = "", y = "Perceived importance of benefits (1=Not at all important for society, 5=Extremely important for society)",
       fill="Are nature-based NETs sufficient?") +
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white") + 
  coord_flip()

benefit_i_Q2_3plot

##### NETs mean scores ####
NET <- c("Direct air capture", "Direct air capture onshore", "Direct air capture offshore",
         "Ocean alkalinization", "Enhanced weathering", "Ocean fertilization", 
         "Coastal restoration", "Afforestation", "Biochar", "Soil sequestration")
NETmean <- c(mean(data$DAC, na.rm=TRUE), mean(data$DAC_offshore, na.rm=TRUE),
             mean(data$DAC_onshore, na.rm=TRUE), mean(data$alkalin, na.rm=TRUE),
             mean(data$weathering, na.rm=TRUE), mean(data$fertil, na.rm=TRUE),
             mean(data$coast_rest, na.rm=TRUE), mean(data$afforest, na.rm=TRUE),
             mean(data$biochar, na.rm=TRUE), mean(data$soil, na.rm=TRUE))
NETse <- c(sd(data$DAC, na.rm=TRUE)/sqrt(length(data$DAC)),
           sd(data$DAC_offshore, na.rm=TRUE)/sqrt(length(data$DAC_offshore)),
           sd(data$DAC_onshore, na.rm=TRUE)/sqrt(length(data$DAC_onshore)),
           sd(data$alkalin, na.rm=TRUE)/sqrt(length(data$alkalin)),
           sd(data$weathering, na.rm=TRUE)/sqrt(length(data$weathering)),
           sd(data$fertil, na.rm=TRUE)/sqrt(length(data$fertil)),
           sd(data$coast_rest, na.rm=TRUE)/sqrt(length(data$coast_rest)),
           sd(data$afforest, na.rm=TRUE)/sqrt(length(data$afforest)),
           sd(data$biochar, na.rm=TRUE)/sqrt(length(data$biochar)),
           sd(data$soil, na.rm=TRUE)/sqrt(length(data$soil)))


NETsummary <- data.frame(NET, NETmean, NETse)


kruskal.test(NETs)
dunn_test(NETs)

NETsummary_plot <- ggplot(data=NETsummary, 
                          aes(x=reorder(NET, -NETmean), 
                              y=NETmean, 
                              ymin=(NETmean-NETse), ymax=(NETmean+NETse))) + 
  geom_bar(stat="identity") +
  geom_errorbar(width=0.25) +
  labs(y= "Average response (-2=Not worthy of consideration; 2=Very worthy of consideration)",
       x= "") +
  scale_y_continuous(limit=c(-2,2))+
  coord_flip()

NETsummary_plot





##### NETs mean scores, by responses to Q2_2, Q2_3, Q2_4 ####

NET_Q2_2 <- data %>% 
  group_by(Q2_2cat2) %>%
  summarize(DAC= mean(DAC, na.rm=TRUE),
            DAC_offshore= mean(DAC_offshore, na.rm=TRUE),
            DAC_onshore= mean(DAC_onshore, na.rm=TRUE),
            alkalin= mean(alkalin, na.rm=TRUE),
            weathering= mean(weathering, na.rm=TRUE),
            fertil= mean(fertil, na.rm=TRUE),
            coast_rest= mean(coast_rest, na.rm=TRUE),
            afforest= mean(afforest, na.rm=TRUE),
            biochar= mean(biochar, na.rm=TRUE),
            soil= mean(soil, na.rm=TRUE)) %>%
  gather("NET", "mean", DAC:soil)

NET_Q2_2
NET_Q2_2$NET <- as.factor(NET_Q2_2$NET)
levels(NET_Q2_2$NET) <- c("Afforestation", "Ocean alkalinization", "Biochar", 
                          "Coastal restoration", "Direct air capture (DAC)",
                          "DAC offshore", "DAC onshore", "Ocean fertilization",
                          "Soil enhancement", "Enhanced weathering")
NET_Q2_2plot <- ggplot(data=na.omit(NET_Q2_2), aes((reorder(NET, mean)), mean, group = Q2_2cat2)) +
  geom_line(aes(color=Q2_2cat2)) +
  geom_point() +
  labs(x = "", y = "Worthy of consideration? (MOST to LEAST)") +
  scale_color_manual(name = "Are NETs necessary?", 
                     values = c("Agree" = "darkblue", "Disagree" = "red")) +  coord_flip()

NET_Q2_2plot


NET_Q2_3 <- data %>% 
  group_by(Q2_3cat2) %>%
  summarize(DAC= mean(DAC, na.rm=TRUE),
            DAC_offshore= mean(DAC_offshore, na.rm=TRUE),
            DAC_onshore= mean(DAC_onshore, na.rm=TRUE),
            alkalin= mean(alkalin, na.rm=TRUE),
            weathering= mean(weathering, na.rm=TRUE),
            fertil= mean(fertil, na.rm=TRUE),
            coast_rest= mean(coast_rest, na.rm=TRUE),
            afforest= mean(afforest, na.rm=TRUE),
            biochar= mean(biochar, na.rm=TRUE),
            soil= mean(soil, na.rm=TRUE)) %>%
  gather("NET", "mean", DAC:soil)

NET_Q2_3
NET_Q2_3$NET <- as.factor(NET_Q2_3$NET)
levels(NET_Q2_3$NET) <- c("Afforestation", "Ocean alkalinization", "Biochar", 
                          "Coastal restoration", "Direct air capture (DAC)",
                          "DAC offshore", "DAC onshore", "Ocean fertilization",
                          "Soil enhancement", "Enhanced weathering")
NET_Q2_3plot <- ggplot(data=na.omit(NET_Q2_3), aes((reorder(NET, mean)), mean, group = Q2_3cat2)) +
  geom_line(aes(color=Q2_3cat2)) +
  geom_point() +
  labs(x = "", y = "Worthy of consideration? (MOST to LEAST)") +
  scale_color_manual(name = "Will NETs based in natural processes be sufficient?", 
                     values = c("Agree" = "darkblue", "Disagree" = "red")) +  coord_flip()

NET_Q2_3plot



NET_Q2_4 <- data %>% 
  group_by(Q2_4cat2) %>%
  summarize(DAC= mean(DAC, na.rm=TRUE),
            DAC_offshore= mean(DAC_offshore, na.rm=TRUE),
            DAC_onshore= mean(DAC_onshore, na.rm=TRUE),
            alkalin= mean(alkalin, na.rm=TRUE),
            weathering= mean(weathering, na.rm=TRUE),
            fertil= mean(fertil, na.rm=TRUE),
            coast_rest= mean(coast_rest, na.rm=TRUE),
            afforest= mean(afforest, na.rm=TRUE),
            biochar= mean(biochar, na.rm=TRUE),
            soil= mean(soil, na.rm=TRUE)) %>%
  gather("NET", "mean", DAC:soil)

NET_Q2_4
NET_Q2_4$NET <- as.factor(NET_Q2_4$NET)
levels(NET_Q2_4$NET) <- c("Afforestation", "Ocean alkalinization", "Biochar", 
                          "Coastal restoration", "Direct air capture (DAC)",
                          "DAC offshore", "DAC onshore", "Ocean fertilization",
                          "Soil enhancement", "Enhanced weathering")
NET_Q2_4plot <- ggplot(data=na.omit(NET_Q2_4), aes((reorder(NET, mean)), mean, group = Q2_4cat2)) +
  geom_line(aes(color=Q2_4cat2)) +
  geom_point() +
  labs(x = "", y = "Worthy of consideration? (MOST to LEAST)") +
  scale_color_manual(name = "Will NETs based in technological processes be necessary?", 
                     values = c("Agree" = "darkblue", "Disagree" = "red")) +  
  coord_flip()

NET_Q2_4plot

##### NETs mean scores, by climate scales and individual items ####
NET_climate_1 <- data %>% 
  group_by(climate_bi_1) %>%
  summarize(DAC= mean(DAC, na.rm=TRUE),
            DAC_offshore= mean(DAC_offshore, na.rm=TRUE),
            DAC_onshore= mean(DAC_onshore, na.rm=TRUE),
            alkalin= mean(alkalin, na.rm=TRUE),
            weathering= mean(weathering, na.rm=TRUE),
            fertil= mean(fertil, na.rm=TRUE),
            coast_rest= mean(coast_rest, na.rm=TRUE),
            afforest= mean(afforest, na.rm=TRUE),
            biochar= mean(biochar, na.rm=TRUE),
            soil= mean(soil, na.rm=TRUE)) %>%
  gather("NET", "mean", DAC:soil)

NET_climate_1
NET_climate_1$NET <- as.factor(NET_climate_1$NET)
levels(NET_climate_1$NET) <- c("Afforestation", "Ocean alkalinization", "Biochar", 
                          "Coastal restoration", "Direct air capture (DAC)",
                          "DAC offshore", "DAC onshore", "Ocean fertilization",
                          "Soil enhancement", "Enhanced weathering")
NET_climate_1 <- na.omit(NET_climate_1)
NET_climate_1plot <- ggplot(data=NET_climate_1, aes((reorder(NET, -mean)), mean, group = climate_bi_1)) +
  geom_line(aes(color=climate_bi_1)) +
  geom_point() +
  labs(x = "", y = "Worthy of consideration? Least = 1, Most = 5") +
  scale_color_manual(name = "Agreement with climate responsibility scale 1", 
                     values = c("Agree" = "darkblue", "Disagree" = "red")) +
  coord_flip()

NET_climate_1plot




NET_climate_2 <- data %>% 
  group_by(climate_bi_2) %>%
  summarize(DAC= mean(DAC, na.rm=TRUE),
            DAC_offshore= mean(DAC_offshore, na.rm=TRUE),
            DAC_onshore= mean(DAC_onshore, na.rm=TRUE),
            alkalin= mean(alkalin, na.rm=TRUE),
            weathering= mean(weathering, na.rm=TRUE),
            fertil= mean(fertil, na.rm=TRUE),
            coast_rest= mean(coast_rest, na.rm=TRUE),
            afforest= mean(afforest, na.rm=TRUE),
            biochar= mean(biochar, na.rm=TRUE),
            soil= mean(soil, na.rm=TRUE)) %>%
  gather("NET", "mean", DAC:soil)

NET_climate_2
NET_climate_2$NET <- as.factor(NET_climate_2$NET)
levels(NET_climate_2$NET) <- c("Afforestation", "Ocean alkalinization", "Biochar", 
                               "Coastal restoration", "Direct air capture (DAC)",
                               "DAC offshore", "DAC onshore", "Ocean fertilization",
                               "Soil enhancement", "Enhanced weathering")
NET_climate_2 <- na.omit(NET_climate_2)
NET_climate_2plot <- ggplot(data=NET_climate_2, aes((reorder(NET, mean)), mean, group = climate_bi_2)) +
  geom_line(aes(color=climate_bi_2)) +
  geom_point() +
  labs(x = "", y = "Worthy of consideration? (MOST to LEAST)") +
  scale_color_manual(name = "Agreement with climate responsibility scale 2", 
                     values = c("Agree" = "darkblue", "Disagree" = "red")) +
  coord_flip()

NET_climate_2plot



NET_climate_item2 <- data %>% 
  group_by(climate_item2_bi) %>%
  summarize(DAC= mean(DAC, na.rm=TRUE),
            DAC_offshore= mean(DAC_offshore, na.rm=TRUE),
            DAC_onshore= mean(DAC_onshore, na.rm=TRUE),
            alkalin= mean(alkalin, na.rm=TRUE),
            weathering= mean(weathering, na.rm=TRUE),
            fertil= mean(fertil, na.rm=TRUE),
            coast_rest= mean(coast_rest, na.rm=TRUE),
            afforest= mean(afforest, na.rm=TRUE),
            biochar= mean(biochar, na.rm=TRUE),
            soil= mean(soil, na.rm=TRUE)) %>%
  gather("NET", "mean", DAC:soil)

NET_climate_item2
NET_climate_item2$NET <- as.factor(NET_climate_item2$NET)
levels(NET_climate_item2$NET) <- c("Afforestation", "Ocean alkalinization", "Biochar", 
                               "Coastal restoration", "Direct air capture (DAC)",
                               "DAC offshore", "DAC onshore", "Ocean fertilization",
                               "Soil enhancement", "Enhanced weathering")
NET_climate_item2 <- na.omit(NET_climate_item2)
NET_climate_item2plot <- ggplot(data=NET_climate_item2, aes((reorder(NET, -mean)), mean, group = climate_item2_bi)) +
  geom_line(aes(color=climate_item2_bi)) +
  geom_point() +
  labs(x = "", y = "Worth of consideration? Least = 1, Most = 5") +
  scale_color_manual(name = "'Developed countries should be responsible for accepting climate refugees'", 
                     values = c("Agree" = "darkblue", "Disagree" = "red"))+
  coord_flip()

NET_climate_item2plot


NET_climate_item3 <- data %>% 
  group_by(climate_item3_bi) %>%
  summarize(DAC= mean(DAC, na.rm=TRUE),
            DAC_offshore= mean(DAC_offshore, na.rm=TRUE),
            DAC_onshore= mean(DAC_onshore, na.rm=TRUE),
            alkalin= mean(alkalin, na.rm=TRUE),
            weathering= mean(weathering, na.rm=TRUE),
            fertil= mean(fertil, na.rm=TRUE),
            coast_rest= mean(coast_rest, na.rm=TRUE),
            afforest= mean(afforest, na.rm=TRUE),
            biochar= mean(biochar, na.rm=TRUE),
            soil= mean(soil, na.rm=TRUE)) %>%
  gather("NET", "mean", DAC:soil)

NET_climate_item3
NET_climate_item3$NET <- as.factor(NET_climate_item3$NET)
levels(NET_climate_item3$NET) <- c("Afforestation", "Ocean alkalinization", "Biochar", 
                                   "Coastal restoration", "Direct air capture (DAC)",
                                   "DAC offshore", "DAC onshore", "Ocean fertilization",
                                   "Soil enhancement", "Enhanced weathering")
NET_climate_item3 <- na.omit(NET_climate_item3)

NET_climate_item3plot <- ggplot(data=NET_climate_item3, aes((reorder(NET, mean)), mean, group = climate_item3_bi)) +
  geom_line(aes(color=climate_item3_bi)) +
  geom_point() +
  labs(x = "", y = "Worth of consideration? (MOST to LEAST)") +
  scale_color_manual(name = "Wealthy countries should cease all fossil fuel development", 
                     values = c("Agree" = "darkblue", "Disagree" = "red"))+
  coord_flip()

NET_climate_item3plot



NET_climate_item7 <- data %>% 
  group_by(climate_item7_bi) %>%
  summarize(DAC= mean(DAC, na.rm=TRUE),
            DAC_offshore= mean(DAC_offshore, na.rm=TRUE),
            DAC_onshore= mean(DAC_onshore, na.rm=TRUE),
            alkalin= mean(alkalin, na.rm=TRUE),
            weathering= mean(weathering, na.rm=TRUE),
            fertil= mean(fertil, na.rm=TRUE),
            coast_rest= mean(coast_rest, na.rm=TRUE),
            afforest= mean(afforest, na.rm=TRUE),
            biochar= mean(biochar, na.rm=TRUE),
            soil= mean(soil, na.rm=TRUE)) %>%
  gather("NET", "mean", DAC:soil)

NET_climate_item7
NET_climate_item7$NET <- as.factor(NET_climate_item7$NET)
levels(NET_climate_item7$NET) <- c("Afforestation", "Ocean alkalinization", "Biochar", 
                                   "Coastal restoration", "Direct air capture (DAC)",
                                   "DAC offshore", "DAC onshore", "Ocean fertilization",
                                   "Soil enhancement", "Enhanced weathering")
NET_climate_item7 <- na.omit(NET_climate_item7)

NET_climate_item7plot <- ggplot(data=NET_climate_item7, aes((reorder(NET, -mean)), mean, group = climate_item7_bi)) +
  geom_line(aes(color=climate_item7_bi)) +
  geom_point() +
  labs(x = "", y = "Worth of consideration? Least = 1, Most = 5") +
  scale_color_manual(name = "'Carbon taxes should finance mitigation of less wealthy countries'", 
                     values = c("Agree" = "darkblue", "Disagree" = "red"))+
  coord_flip()

NET_climate_item7plot



NET_climate_item9 <- data %>% 
  group_by(climate_item9_bi) %>%
  summarize(DAC= mean(DAC, na.rm=TRUE),
            DAC_offshore= mean(DAC_offshore, na.rm=TRUE),
            DAC_onshore= mean(DAC_onshore, na.rm=TRUE),
            alkalin= mean(alkalin, na.rm=TRUE),
            weathering= mean(weathering, na.rm=TRUE),
            fertil= mean(fertil, na.rm=TRUE),
            coast_rest= mean(coast_rest, na.rm=TRUE),
            afforest= mean(afforest, na.rm=TRUE),
            biochar= mean(biochar, na.rm=TRUE),
            soil= mean(soil, na.rm=TRUE)) %>%
  gather("NET", "mean", DAC:soil)

NET_climate_item9
NET_climate_item9$NET <- as.factor(NET_climate_item9$NET)
levels(NET_climate_item9$NET) <- c("Afforestation", "Ocean alkalinization", "Biochar", 
                                   "Coastal restoration", "Direct air capture (DAC)",
                                   "DAC offshore", "DAC onshore", "Ocean fertilization",
                                   "Soil enhancement", "Enhanced weathering")
NET_climate_item9 <- na.omit(NET_climate_item9)
NET_climate_item9plot <- ggplot(data=NET_climate_item9, aes((reorder(NET, mean)), mean, group = climate_item9_bi)) +
  geom_line(aes(color=climate_item9_bi)) +
  geom_point() +
  labs(x = "", y = "Worth of consideration? (MOST to LEAST)") +
  scale_color_manual(name = "'Climate mitigation should be proportionate to per capita GDP'", 
                     values = c("Agree" = "darkblue", "Disagree" = "red"))+
  coord_flip()

NET_climate_item9plot



##### NETs mean scores, by governance ####
NET_governance_1 <- data %>% 
  group_by(governance_cat_1) %>%
  summarize(DAC= mean(DAC, na.rm=TRUE),
            DAC_offshore= mean(DAC_offshore, na.rm=TRUE),
            DAC_onshore= mean(DAC_onshore, na.rm=TRUE),
            alkalin= mean(alkalin, na.rm=TRUE),
            weathering= mean(weathering, na.rm=TRUE),
            fertil= mean(fertil, na.rm=TRUE),
            coast_rest= mean(coast_rest, na.rm=TRUE),
            afforest= mean(afforest, na.rm=TRUE),
            biochar= mean(biochar, na.rm=TRUE),
            soil= mean(soil, na.rm=TRUE)) %>%
  gather("NET", "mean", DAC:soil)

NET_governance_1
NET_governance_1$NET <- as.factor(NET_governance_1$NET)
levels(NET_governance_1$NET) <- c("Afforestation", "Ocean alkalinization", "Biochar", 
                               "Coastal restoration", "Direct air capture (DAC)",
                               "DAC offshore", "DAC onshore", "Ocean fertilization",
                               "Soil enhancement", "Enhanced weathering")

NET_governance_1plot <- ggplot(data=NET_governance_1, aes((reorder(NET, -mean)), mean, fill = governance_cat_1)) +
  geom_bar(stat="identity", position="dodge") +
  labs(x = "", y = "Average score (-2=not at all worthy of consideration, 2=Absolutely worthy of consideration)",
       fill="Agreement with governance scale") +
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white") + 
  coord_flip()

NET_governance_1plot


##### NETs mean scores, by trust ####


NET_trust_bi_1 <- data %>% 
  group_by(trust_bi_1) %>%
  summarize(DAC= mean(DAC, na.rm=TRUE),
            DAC_offshore= mean(DAC_offshore, na.rm=TRUE),
            DAC_onshore= mean(DAC_onshore, na.rm=TRUE),
            alkalin= mean(alkalin, na.rm=TRUE),
            weathering= mean(weathering, na.rm=TRUE),
            fertil= mean(fertil, na.rm=TRUE),
            coast_rest= mean(coast_rest, na.rm=TRUE),
            afforest= mean(afforest, na.rm=TRUE),
            biochar= mean(biochar, na.rm=TRUE),
            soil= mean(soil, na.rm=TRUE)) %>%
  gather("NET", "mean", DAC:soil)

NET_trust_bi_1
NET_trust_bi_1$NET <- as.factor(NET_trust_bi_1$NET)
levels(NET_trust_bi_1$NET) <- c("Afforestation", "Ocean alkalinization", "Biochar", 
                                   "Coastal restoration", "Direct air capture (DAC)",
                                   "DAC offshore", "DAC onshore", "Ocean fertilization",
                                   "Soil enhancement", "Enhanced weathering")
NET_trust_bi_1 <- na.omit(NET_trust_bi_1)
NET_trust_bi_1plot <- ggplot(data=NET_trust_bi_1, aes((reorder(NET, mean)), mean, group = trust_bi_1)) +
  geom_line(aes(color=trust_bi_1)) +
  geom_point() +
  labs(x = "", y = "Worth of consideration? (MOST to LEAST)") +
  scale_color_manual(name = "Agreement with trust scale 1", 
                          values = c("Agree" = "darkblue", "Disagree" = "red")) +
  coord_flip()

NET_trust_bi_1plot





NET_trust_bi_2 <- data %>% 
  group_by(trust_bi_2) %>%
  summarize(DAC= mean(DAC, na.rm=TRUE),
            DAC_offshore= mean(DAC_offshore, na.rm=TRUE),
            DAC_onshore= mean(DAC_onshore, na.rm=TRUE),
            alkalin= mean(alkalin, na.rm=TRUE),
            weathering= mean(weathering, na.rm=TRUE),
            fertil= mean(fertil, na.rm=TRUE),
            coast_rest= mean(coast_rest, na.rm=TRUE),
            afforest= mean(afforest, na.rm=TRUE),
            biochar= mean(biochar, na.rm=TRUE),
            soil= mean(soil, na.rm=TRUE)) %>%
  gather("NET", "mean", DAC:soil)

NET_trust_bi_2
NET_trust_bi_2$NET <- as.factor(NET_trust_bi_2$NET)
levels(NET_trust_bi_2$NET) <- c("Afforestation", "Ocean alkalinization", "Biochar", 
                                "Coastal restoration", "Direct air capture (DAC)",
                                "DAC offshore", "DAC onshore", "Ocean fertilization",
                                "Soil enhancement", "Enhanced weathering")
NET_trust_bi_2 <- na.omit(NET_trust_bi_2)
NET_trust_bi_2plot <- ggplot(data=NET_trust_bi_2, aes((reorder(NET, mean)), mean, group = trust_bi_2)) +
  geom_line(aes(color=trust_bi_2)) +
  geom_point() +
  labs(x = "", y = "Worth of consideration? (MOST to LEAST)") +
  scale_color_manual(name = "Agreement with trust scale 2", 
                     values = c("Agree" = "darkblue", "Disagree" = "red")) +
  coord_flip()

NET_trust_bi_2plot

##### NETs mean scores, by components ####
NET_comp_bi_1 <- data %>% 
  group_by(comp_bi_1) %>%
  summarize(DAC= mean(DAC, na.rm=TRUE),
            DAC_offshore= mean(DAC_offshore, na.rm=TRUE),
            DAC_onshore= mean(DAC_onshore, na.rm=TRUE),
            alkalin= mean(alkalin, na.rm=TRUE),
            weathering= mean(weathering, na.rm=TRUE),
            fertil= mean(fertil, na.rm=TRUE),
            coast_rest= mean(coast_rest, na.rm=TRUE),
            afforest= mean(afforest, na.rm=TRUE),
            biochar= mean(biochar, na.rm=TRUE),
            soil= mean(soil, na.rm=TRUE)) %>%
  gather("NET", "mean", DAC:soil)

NET_comp_bi_1
NET_comp_bi_1$NET <- as.factor(NET_comp_bi_1$NET)
levels(NET_comp_bi_1$NET) <- c("Afforestation", "Ocean alkalinization", "Biochar", 
                               "Coastal restoration", "Direct air capture (DAC)",
                               "DAC offshore", "DAC onshore", "Ocean fertilization",
                               "Soil enhancement", "Enhanced weathering")
NET_comp_bi_1 <- na.omit(NET_comp_bi_1)
NET_comp_bi_1plot <- ggplot(data=NET_comp_bi_1, aes((reorder(NET, mean)), mean, group = comp_bi_1)) +
  geom_line(aes(color=comp_bi_1)) +
  geom_point() +
  labs(x = "", y = "Worthy of consideration? (MOST to LEAST)") +
  scale_color_manual(name = "Concerned with sub-seafloor components of Solid Carbon", 
                     values = c("Agree" = "darkblue", "Disagree" = "red")) +
  coord_flip()

NET_comp_bi_1plot





NET_comp_bi_2 <- data %>% 
  group_by(comp_bi_2) %>%
  summarize(DAC= mean(DAC, na.rm=TRUE),
            DAC_offshore= mean(DAC_offshore, na.rm=TRUE),
            DAC_onshore= mean(DAC_onshore, na.rm=TRUE),
            alkalin= mean(alkalin, na.rm=TRUE),
            weathering= mean(weathering, na.rm=TRUE),
            fertil= mean(fertil, na.rm=TRUE),
            coast_rest= mean(coast_rest, na.rm=TRUE),
            afforest= mean(afforest, na.rm=TRUE),
            biochar= mean(biochar, na.rm=TRUE),
            soil= mean(soil, na.rm=TRUE)) %>%
  gather("NET", "mean", DAC:soil)

NET_comp_bi_2
NET_comp_bi_2$NET <- as.factor(NET_comp_bi_2$NET)
levels(NET_comp_bi_2$NET) <- c("Afforestation", "Ocean alkalinization", "Biochar", 
                                "Coastal restoration", "Direct air capture (DAC)",
                                "DAC offshore", "DAC onshore", "Ocean fertilization",
                                "Soil enhancement", "Enhanced weathering")
NET_comp_bi_2 <- na.omit(NET_comp_bi_2)
NET_comp_bi_2plot <- ggplot(data=NET_comp_bi_2, aes((reorder(NET, mean)), mean, group = comp_bi_2)) +
  geom_line(aes(color=comp_bi_2)) +
  geom_point() +
  labs(x = "", y = "Worthy of consideration? (MOST to LEAST)") +
  scale_color_manual(name = "Concerned with above-ocean components of Solid Carbon", 
                     values = c("Agree" = "darkblue", "Disagree" = "red")) +
  coord_flip()

NET_comp_bi_2plot


NET_comp_2 <- data %>% 
  group_by(comp_cat_2) %>%
  summarize(DAC= mean(DAC, na.rm=TRUE),
            DAC_offshore= mean(DAC_offshore, na.rm=TRUE),
            DAC_onshore= mean(DAC_onshore, na.rm=TRUE),
            alkalin= mean(alkalin, na.rm=TRUE),
            weathering= mean(weathering, na.rm=TRUE),
            fertil= mean(fertil, na.rm=TRUE),
            coast_rest= mean(coast_rest, na.rm=TRUE),
            afforest= mean(afforest, na.rm=TRUE),
            biochar= mean(biochar, na.rm=TRUE),
            soil= mean(soil, na.rm=TRUE)) %>%
  gather("NET", "mean", DAC:soil)

NET_comp_2
NET_comp_2$NET <- as.factor(NET_comp_2$NET)
levels(NET_comp_2$NET) <- c("Afforestation", "Ocean alkalinization", "Biochar", 
                            "Coastal restoration", "Direct air capture (DAC)",
                            "DAC offshore", "DAC onshore", "Ocean fertilization",
                            "Soil enhancement", "Enhanced weathering")

NET_comp_2plot <- ggplot(data=NET_comp_2, aes((reorder(NET, -mean)), mean, fill = comp_cat_2)) +
  geom_bar(stat="identity", position="dodge") +
  labs(x = "", y = "Average score (1=not at all concerned, 5=extremely concerned)",
       fill="Level of concern about above-ocean components of Solid Carbon") +
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white") + 
  coord_flip()

NET_comp_2plot


##### NETs mean scores, by risk likelihood ####


NET_risk_l_bi_1 <- data %>% 
  group_by(risk_l.a_bi) %>%
  summarize(DAC= mean(DAC, na.rm=TRUE),
            DAC_offshore= mean(DAC_offshore, na.rm=TRUE),
            DAC_onshore= mean(DAC_onshore, na.rm=TRUE),
            alkalin= mean(alkalin, na.rm=TRUE),
            weathering= mean(weathering, na.rm=TRUE),
            fertil= mean(fertil, na.rm=TRUE),
            coast_rest= mean(coast_rest, na.rm=TRUE),
            afforest= mean(afforest, na.rm=TRUE),
            biochar= mean(biochar, na.rm=TRUE),
            soil= mean(soil, na.rm=TRUE)) %>%
  gather("NET", "mean", DAC:soil)

NET_risk_l_bi_1
NET_risk_l_bi_1$NET <- as.factor(NET_risk_l_bi_1$NET)
levels(NET_risk_l_bi_1$NET) <- c("Afforestation", "Ocean alkalinization", "Biochar", 
                               "Coastal restoration", "Direct air capture (DAC)",
                               "DAC offshore", "DAC onshore", "Ocean fertilization",
                               "Soil enhancement", "Enhanced weathering")
NET_risk_l_bi_1 <- na.omit(NET_risk_l_bi_1)
NET_risk_l_bi_1plot <- ggplot(data=NET_risk_l_bi_1, aes((reorder(NET, mean)), mean, group = risk_l.a_bi)) +
  geom_line(aes(color=risk_l.a_bi)) +
  geom_point() +
  labs(x = "", y = "Worthy of consideration? (MOST to LEAST)") +
  scale_color_manual(name = "Likelihood of physical risks", 
                     values = c("Risks likely" = "Red", "Risks unlikely" = "Darkblue")) +
  coord_flip()

NET_risk_l_bi_1plot


NET_risk_l_bi_2 <- data %>% 
  group_by(risk_l.b_bi) %>%
  summarize(DAC= mean(DAC, na.rm=TRUE),
            DAC_offshore= mean(DAC_offshore, na.rm=TRUE),
            DAC_onshore= mean(DAC_onshore, na.rm=TRUE),
            alkalin= mean(alkalin, na.rm=TRUE),
            weathering= mean(weathering, na.rm=TRUE),
            fertil= mean(fertil, na.rm=TRUE),
            coast_rest= mean(coast_rest, na.rm=TRUE),
            afforest= mean(afforest, na.rm=TRUE),
            biochar= mean(biochar, na.rm=TRUE),
            soil= mean(soil, na.rm=TRUE)) %>%
  gather("NET", "mean", DAC:soil)

NET_risk_l_bi_2
NET_risk_l_bi_2$NET <- as.factor(NET_risk_l_bi_2$NET)
levels(NET_risk_l_bi_2$NET) <- c("Afforestation", "Ocean alkalinization", "Biochar", 
                                 "Coastal restoration", "Direct air capture (DAC)",
                                 "DAC offshore", "DAC onshore", "Ocean fertilization",
                                 "Soil enhancement", "Enhanced weathering")
NET_risk_l_bi_2 <- na.omit(NET_risk_l_bi_2)
NET_risk_l_bi_2plot <- ggplot(data=NET_risk_l_bi_2, aes((reorder(NET, mean)), mean, group = risk_l.b_bi)) +
  geom_line(aes(color=risk_l.b_bi)) +
  geom_point() +
  labs(x = "", y = "Worthy of consideration? (MOST to LEAST)") +
  scale_color_manual(name = "Likelihood of economic and moral hazard risks", 
                     values = c("Risks likely" = "Red", "Risks unlikely" = "Darkblue")) +
  coord_flip()

NET_risk_l_bi_2plot


NET_risk_l_bi_3 <- data %>% 
  group_by(risk_l.c_bi) %>%
  summarize(DAC= mean(DAC, na.rm=TRUE),
            DAC_offshore= mean(DAC_offshore, na.rm=TRUE),
            DAC_onshore= mean(DAC_onshore, na.rm=TRUE),
            alkalin= mean(alkalin, na.rm=TRUE),
            weathering= mean(weathering, na.rm=TRUE),
            fertil= mean(fertil, na.rm=TRUE),
            coast_rest= mean(coast_rest, na.rm=TRUE),
            afforest= mean(afforest, na.rm=TRUE),
            biochar= mean(biochar, na.rm=TRUE),
            soil= mean(soil, na.rm=TRUE)) %>%
  gather("NET", "mean", DAC:soil)

NET_risk_l_bi_3
NET_risk_l_bi_3$NET <- as.factor(NET_risk_l_bi_3$NET)
levels(NET_risk_l_bi_3$NET) <- c("Afforestation", "Ocean alkalinization", "Biochar", 
                                 "Coastal restoration", "Direct air capture (DAC)",
                                 "DAC offshore", "DAC onshore", "Ocean fertilization",
                                 "Soil enhancement", "Enhanced weathering")
NET_risk_l_bi_3 <- na.omit(NET_risk_l_bi_3)
NET_risk_l_bi_3plot <- ggplot(data=NET_risk_l_bi_3, aes((reorder(NET, mean)), mean, group = risk_l.c_bi)) +
  geom_line(aes(color=risk_l.c_bi)) +
  geom_point() +
  labs(x = "", y = "Worthy of consideration? (MOST to LEAST)") +
  scale_color_manual(name = "Likelihood of induced earthquakes", 
                     values = c("Risk likely" = "Red", "Risk unlikely" = "Darkblue")) +
  coord_flip()

NET_risk_l_bi_3plot





NET_risk_s_bi_1 <- data %>% 
  group_by(risk_s.a_bi) %>%
  summarize(DAC= mean(DAC, na.rm=TRUE),
            DAC_offshore= mean(DAC_offshore, na.rm=TRUE),
            DAC_onshore= mean(DAC_onshore, na.rm=TRUE),
            alkalin= mean(alkalin, na.rm=TRUE),
            weathering= mean(weathering, na.rm=TRUE),
            fertil= mean(fertil, na.rm=TRUE),
            coast_rest= mean(coast_rest, na.rm=TRUE),
            afforest= mean(afforest, na.rm=TRUE),
            biochar= mean(biochar, na.rm=TRUE),
            soil= mean(soil, na.rm=TRUE)) %>%
  gather("NET", "mean", DAC:soil)

NET_risk_s_bi_1
NET_risk_s_bi_1$NET <- as.factor(NET_risk_s_bi_1$NET)
levels(NET_risk_s_bi_1$NET) <- c("Afforestation", "Ocean alkalinization", "Biochar", 
                                 "Coastal restoration", "Direct air capture (DAC)",
                                 "DAC offshore", "DAC onshore", "Ocean fertilization",
                                 "Soil enhancement", "Enhanced weathering")
NET_risk_s_bi_1 <- na.omit(NET_risk_s_bi_1)
NET_risk_s_bi_1plot <- ggplot(data=NET_risk_s_bi_1, aes((reorder(NET, mean)), mean, group = risk_s.a_bi)) +
  geom_line(aes(color=risk_s.a_bi)) +
  geom_point() +
  labs(x = "", y = "Worthy of consideration? (MOST to LEAST)") +
  scale_color_manual(name = "Severity of physical risks (non-seismic)", 
                     values = c("Risks not severe" = "Darkblue", "Risks severe" = "Red")) +
  coord_flip()

NET_risk_s_bi_1plot


NET_risk_s_bi_2 <- data %>% 
  group_by(risk_s.b_bi) %>%
  summarize(DAC= mean(DAC, na.rm=TRUE),
            DAC_offshore= mean(DAC_offshore, na.rm=TRUE),
            DAC_onshore= mean(DAC_onshore, na.rm=TRUE),
            alkalin= mean(alkalin, na.rm=TRUE),
            weathering= mean(weathering, na.rm=TRUE),
            fertil= mean(fertil, na.rm=TRUE),
            coast_rest= mean(coast_rest, na.rm=TRUE),
            afforest= mean(afforest, na.rm=TRUE),
            biochar= mean(biochar, na.rm=TRUE),
            soil= mean(soil, na.rm=TRUE)) %>%
  gather("NET", "mean", DAC:soil)

NET_risk_s_bi_2
NET_risk_s_bi_2$NET <- as.factor(NET_risk_s_bi_2$NET)
levels(NET_risk_s_bi_2$NET) <- c("Afforestation", "Ocean alkalinization", "Biochar", 
                                 "Coastal restoration", "Direct air capture (DAC)",
                                 "DAC offshore", "DAC onshore", "Ocean fertilization",
                                 "Soil enhancement", "Enhanced weathering")
NET_risk_s_bi_2 <- na.omit(NET_risk_s_bi_2)
NET_risk_s_bi_2plot <- ggplot(data=NET_risk_s_bi_2, aes((reorder(NET, mean)), mean, group = risk_s.b_bi)) +
  geom_line(aes(color=risk_s.b_bi)) +
  geom_point() +
  labs(x = "", y = "Worthy of consideration? (MOST to LEAST)") +
  scale_color_manual(name = "Severity of economic and moral hazard risks", 
                     values = c("Risks not severe" = "Darkblue", "Risks severe" = "Red")) +
  coord_flip()

NET_risk_s_bi_2plot

##### Support for SC, by Q2.2, Q2.3, Q2.4 ####

support_Q2_2plot <- ggplot(data, aes(x=support, fill=Q2_2cat)) +
  geom_bar(position="dodge") +
  labs(x = "") +
  theme(legend.title = element_blank())+
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white")

support_Q2_2plot

#support, according to response to Q2.3
support_Q2_3plot <- ggplot(data, aes(x=support, fill=Q2_3cat)) +
  geom_bar(position="dodge") +
  labs(x = "") +
  theme(legend.title = element_blank())+
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white")

support_Q2_3plot

#support, according to response to Q2.4
support_Q2_4plot <- ggplot(data, aes(x=support, fill=Q2_4cat)) +
  geom_bar(position="dodge") +
  labs(x = "") +
  theme(legend.title = element_blank())+
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white")

support_Q2_4plot



##### Support, by climate scales and individual items ####

support_climate_1_plot <- ggplot(data, aes(x=support, fill=climate_cat_1)) +
  geom_bar(position="dodge") +
  theme(legend.title = element_blank())+
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white")

support_climate_1_plot

support_climate_2_plot <- ggplot(data, aes(x=support, fill=climate_cat_2)) +
  geom_bar(position="dodge") +
  theme(legend.title = element_blank())+
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white")

support_climate_2_plot


support_climate_item2_plot <- ggplot(data, aes(x=support, fill=climate_item2_cat)) +
  geom_bar(position="dodge") +
  theme(legend.title = element_blank())+
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white")

support_climate_item2_plot

support_climate_item3_plot <- ggplot(data, aes(x=support, fill=climate_item3_cat)) +
  geom_bar(position="dodge") +
  theme(legend.title = element_blank())+
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white")

support_climate_item3_plot

support_climate_item7_plot <- ggplot(data, aes(x=support, fill=climate_item7_cat)) +
  geom_bar(position="dodge") +
  theme(legend.title = element_blank())+
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white")

support_climate_item7_plot

support_climate_item9_plot <- ggplot(data, aes(x=support, fill=climate_item9_cat)) +
  geom_bar(position="dodge") +
  theme(legend.title = element_blank())+
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white")

support_climate_item9_plot


##### Support, by governance ####

support_governance_plot <- ggplot(data, aes(x=support, fill=governance_cat_1)) +
  geom_bar(position="dodge") +
  theme(legend.title = element_blank())+
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white")

support_governance_plot



##### Support, by trust ####

support_trust_1_plot <- ggplot(data, aes(x=support, fill=trust_cat_1)) +
  geom_bar(position="dodge") +
  theme(legend.title = element_blank())+
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white")

support_trust_1_plot

support_trust_2_plot <- ggplot(data, aes(x=support, fill=trust_cat_2)) +
  geom_bar(position="dodge") +
  theme(legend.title = element_blank())+
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white")

support_trust_2_plot



##### Support, by components ####

support_comp_1_plot <- ggplot(data, aes(x=support, fill=comp_cat_1)) +
  geom_bar(position="dodge") +
  theme(legend.title = element_blank())+
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white")

support_comp_1_plot

support_comp_2_plot <- ggplot(data, aes(x=support, fill=comp_cat_2)) +
  geom_bar(position="dodge") +
  theme(legend.title = element_blank())+
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white")

support_comp_2_plot



##### Support, by risk likelihood ####
##Plot support for SC by each of these risk likelihood factors

support_risk_a_plot <- ggplot(data, aes(x=support, fill=risk_l.a_cat)) +
  geom_bar(position="dodge") +
  theme(legend.title = element_blank())+
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white")

support_risk_a_plot

support_risk_b_plot <- ggplot(data, aes(x=support, fill=risk_l.b_cat)) +
  geom_bar(position="dodge") +
  theme(legend.title = element_blank())+
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white")

support_risk_b_plot

support_risk_c_plot <- ggplot(data, aes(x=support, fill=risk_l.c_cat)) +
  geom_bar(position="dodge") +
  theme(legend.title = element_blank())+
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white")

support_risk_c_plot


##### Support, by risk severity ####
###plot support for SC by risk severity factors 
support_risk_a.s_plot <- ggplot(data, aes(x=support, fill=risk_s.a_cat)) +
  geom_bar(position="dodge") +
  theme(legend.title = element_blank())+
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white")

support_risk_a.s_plot

support_risk_b.s_plot <- ggplot(data, aes(x=support, fill=risk_s.b_cat)) +
  geom_bar(position="dodge") +
  theme(legend.title = element_blank())+
  scale_fill_manual(values=c("#619CFF", "#F8766D", "grey"), na.value="white")

support_risk_b.s_plot







##### Sankey diagram ####

Q2_2cat<- as.factor(data$Q2_2cat)
Q2_3cat<- as.factor(data$Q2_3cat)
Q2_4cat<- as.factor(data$Q2_4cat)
sankey_data <- data.frame(Q2_2cat, Q2_3cat, Q2_4cat)
sankey_data <-rename(sankey_data, "Are NETs necessary?" = Q2_2cat,
    "Are NETs based in natural processes sufficient?" = Q2_3cat,
    "Are NETs based in technologically engineered processes required?" = Q2_4cat)
sankey_data <- na.omit(sankey_data)
alluvial_wide(sankey_data, col_vector_value =  RColorBrewer::brewer.pal(9, 'Greys')[c(4,7,5,8,6)])
