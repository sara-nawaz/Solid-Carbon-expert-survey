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

setwd("~/Desktop/Stuff/Postdoc/Solid carbon/Expert survey/data and analysis/R scripts and data")
data <- read.csv("expert survey_cleaned.csv", sep= ',', stringsAsFactors=FALSE, header = TRUE)
str(data) 

##### number of respondents, group 1 vs. group 2 #####
table(data$expert)


##### Very basic plots #####
#Level of agreement with question 2, on climate modelling and NETs
hist(data$Q2_1)
hist(data$Q2_2)
hist(data$Q2_3)

hist(data$DAC)
mean(data$DAC, na.rm=TRUE)

hist(data$DAC_offshore)
mean(data$DAC_offshore, na.rm=TRUE)

hist(data$DAC_onshore)
mean(data$DAC_onshore, na.rm=TRUE)

hist(data$alkalin)
mean(data$alkalin, na.rm=TRUE)

hist(data$weathering)
mean(data$weathering, na.rm=TRUE)

hist(data$fertil)
mean(data$fertil, na.rm=TRUE)

hist(data$coast_rest)
mean(data$coast_rest, na.rm=TRUE)

hist(data$afforest)
mean(data$afforest, na.rm=TRUE)

hist(data$biochar)
mean(data$biochar, na.rm=TRUE)

hist(data$soil)
mean(data$soil, na.rm=TRUE)


#Test group for normality
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



hist(data$support)
hist(data$Q6)



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




##### Plot of support #####
data$support <- as.factor(support)
levels(data$support) <- c("-2" = "Strongly oppose",
                          "-1" = "Oppose",
                          "0" = "Neither oppose nor support",
                          "1" = "Support",
                          "2" = "Strongly support")


expert.labs <- c("1" = "Geophysical experts", "2" = "Marine experts")
support.expert <- data %>% 
  filter(!is.na(support)) %>%
  group_by(expert) %>%
  count(support) %>%
  mutate(percent = n / nrow(data)*100)
support.expert$support <- as.factor(support.expert$support)
support.expert$expert <- as.factor(support.expert$expert) 

support.expert.plot <-ggplot(support.expert, mapping = aes(x = as.factor(support), y = percent, fill=as.factor(expert)) +
                               geom_bar(stat="identity", position=position_dodge())+
                               theme_minimal()+
                               coord_flip()+
                               scale_x_discrete(labels = c("Strongly oppose", 
                                                           "Oppose", 
                                                           "Neither", 
                                                           "Support", 
                                                           "Strongly support"))+
                               scale_fill_discrete(labels=c("Group 1", "Group 2")))

support.expert.plot

support_plot <- ggplot(data=data, aes(support)) + 
  geom_bar() +
  labs(title="Support/opposition for Solid Carbon", 
       x="Strongly oppose (-2), Oppose (-1), Neither (0), Support (+1), Strongly support (+2)") + 
  facet_wrap(~ expert, labeller=labeller(expert = expert.labs))
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









##### Likert plot--NETS #####

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
NETs_new <- cbind(NETs, data$expert)

#NETs_grp1 <- NETs_new %>%
  #filter(data$expert==2) #MODIFY GROUPING-EXPERT AFFILIATION

NETs_likert2 <- NETs_new2[,c(1:10)] %>%
  mutate_all(likert_recode) %>%
  likert(grouping = data$expert)

#NETs_likert <- NETs %>%
  #mutate_all(likert_recode) %>%
  #likert(grouping = data$expert)
  #likert()

plot(NETs_likert2, as.percent = TRUE, panel.strip.color="dark grey", centered = TRUE, positive.order = TRUE) + 


##### Need for NETs, broken down by expert group #####


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

needNETs_new <- cbind(needNETs, data$expert)

needNETs_likert <- needNETs[,c(1:4)] %>%
#needNETs_likert <- needNETs %>%
  mutate_all(likert_recode) %>%
  likert(grouping = data$expert) 

plot(needNETs_likert, as.percent = TRUE, panel.strip.color="dark grey")


##### risks and benefits #####

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



##### Risk severity #####

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


##### Benefit likelihood #####

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

##### Importance of benefits #####

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

