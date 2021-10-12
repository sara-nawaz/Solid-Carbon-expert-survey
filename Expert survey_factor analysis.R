library(tidyverse)
library(dplyr)
library(ggplot2)
theme_set(
  theme_light() + theme(legend.position = "top")
)
library(tidyr)
library(scales)  # for percentage scales
library(psych)

setwd("~/Desktop/Stuff/Postdoc/Solid carbon/Expert survey/data and analysis/R scripts and data")
data <- read.csv("expert survey_cleaned.csv", sep= ',', stringsAsFactors=FALSE, header = TRUE)



###FACTOR ANALYSIS

needNETs <- data[c("Q2_1", "Q2_2", "Q2_3", "Q2_4")]
scree(needNETs)
needNETs.fa <- fa(needNETs, nfactors=2, scores=TRUE,rotate="varimax", fm="pa")
print.psych(needNETs.fa, cut = 0.3, sort = TRUE)

needNETs.a <- data.frame(data$Q2_4, data$Q2_2)
psych::alpha(needNETs.a)

needNETs.b <- data.frame(data$Q2_3, data$Q2_1)
psych::alpha(needNETs.b)


NETs <- data[c("DAC", "DAC_offshore", "DAC_onshore", "alkalin", "weathering", 
               "fertil", "coast_rest", "afforest", "biochar", "soil")]

scree(NETs)
NETs.fa <- fa(NETs, nfactors=2, scores=TRUE,rotate="varimax", fm="pa")
print.psych(NETs.fa, cut = 0.3, sort = TRUE)

NETs.a <- data.frame(data$DAC_offshore, data$DAC_onshore, data$weathering,
                     data$alkalin, data$fertil, data$biochar, data$DAC)
psych::alpha(NETs.a)

NETs.b <- data.frame(data$coast_rest, data$afforest, data$soil)
psych::alpha(NETs.b)
#probably should drop afforestation--so we end up with just coast_rest and soil

components <- data[c("Q7_1", "Q7_2", "Q7_3", "Q7_4", "Q7_5", "Q7_6")]
scree(components)
components.fa <- fa(components, nfactors=2, scores=TRUE,rotate="varimax", fm="pa")
print.psych(components.fa, cut = 0.3, sort = TRUE)

components.a <- data.frame(data$Q7_5, data$Q7_4, data$Q7_6)
psych::alpha(components.a)

components.b <- data.frame(data$Q7_2, data$Q7_3, data$Q7_1)
psych::alpha(components.b)
##seems to be a distinction between drilling, injecting, and liquid co2--versus DAC/ocean platform.

risk_l <- data[c("Q8a_1", "Q8a_2", "Q8a_3", "Q8a_4", "Q8a_5", "Q8a_6", "Q8a_7",
                 "Q8a_8", "Q8a_9", "Q8a_10", "Q8a_11")]
scree(risk_l)
risk_l.fa <- fa(risk_l, nfactors=3, scores=TRUE,rotate="varimax", fm="pa")
print.psych(risk_l.fa, cut = 0.3, sort = TRUE)
data$risk_l <- risk_l

risk_l.a <- data.frame(data$Q8a_4, data$Q8a_3, data$Q8a_7, data$Q8a_6, data$Q8a_5)
psych::alpha(risk_l.a)

risk_l.b <- data.frame(data$Q8a_11, data$Q8a_9, data$Q8a_8, data$Q8a_10)
psych::alpha(risk_l.b)

risk_l.c <- data.frame(data$Q8a_2, data$Q8a_1)
psych::alpha(risk_l.c)
##appears to be a distinciton between earthquake/seismicity; various environmental and health risks; 
## and broader socio-political/economic/intangible risks

risk_s <- data[c("Q8b_1", "Q8b_2", "Q8b_3", "Q8b_4", "Q8b_5", "Q8b_6", "Q8b_7",
                 "Q8b_8", "Q8b_9", "Q8b_10", "Q8b_11")]
scree(risk_s)
risk_s.fa <- fa(risk_s, nfactors=2, scores=TRUE,rotate="varimax", fm="pa")
print.psych(risk_s.fa, cut = 0.3, sort = TRUE)
data$risk_s <- risk_s

risk_s.a <- data.frame(data$Q8b_2, data$Q8b_1, data$Q8b_3, data$Q8b_4, data$Q8b_5,
                       data$Q8b_7, data$Q8b_6)
psych::alpha(risk_s.a)

risk_s.b <- data.frame(data$Q8b_11, data$Q8b_9, data$Q8b_10, data$Q8b_8)
psych::alpha(risk_s.b)

##there are only two factors it seems, for severity, as opposed to 3 for likelihood. groupings are:
## earthquakes/seismicity/explosive discrarge/leakage/health, vs., all the socioeconomic. pollution, 
## interestingly, is in the second group


benefit_l <- data[c("Q9a_1", "Q9a_2", "Q9a_3", "Q9a_4", "Q9a_5", "Q9a_6", "Q9a_7",
                    "Q9a_8", "Q9a_9", "Q9a_10")]
scree(benefit_l)
benefit_l.fa <- fa(benefit_l, nfactors=1, scores=TRUE,rotate="varimax", fm="pa")
print.psych(benefit_l.fa, cut = 0.3, sort = TRUE)
data$benefit_l <- benefit_l

psych::alpha(benefit_l)

benefit_s <- data[c("Q9b_1", "Q9b_2", "Q9b_3", "Q9b_4", "Q9b_5", "Q9b_6", "Q9b_7",
                    "Q9b_8", "Q9b_9", "Q9b_10")]
scree(benefit_s)
benefit_s.fa <- fa(benefit_s, nfactors=1, scores=TRUE,rotate="varimax", fm="pa")
print.psych(benefit_s.fa, cut = 0.3, sort = TRUE)
data$benefit_s <- benefit_s

psych::alpha(benefit_s)








trust <- data[c("Q12_1", "Q12_2", "Q12_3", "Q12_4", "Q12_5", "Q12_6", "Q12_7",
                "Q12_8", "Q12_9", "Q12_10", "Q12_11")]
data$trust <-trust

scree(trust)
trust.fa <- fa(trust, nfactors=2, scores=TRUE,rotate="varimax", fm="pa")
print.psych(trust.fa, cut = 0.3, sort = TRUE)


trust.a <- data.frame(data$Q12_6, data$Q12_4, data$Q12_5, data$Q12_9, data$Q12_10, 
                      data$Q12_8, data$Q12_11, data$Q12_7)
psych::alpha(trust.a)

trust.b <- data.frame(data$Q12_1, data$Q12_3, data$Q12_2)
psych::alpha(trust.b)


data$trust1mean=rowMeans(cbind(data$Q12_6, data$Q12_5, data$Q12_5, data$Q12_9, data$Q12_10, 
                               data$Q12_8, data$Q12_11, data$Q12_7),na.rm=TRUE)
data$trust2mean=rowMeans(cbind(data$Q12_1, data$Q12_3, data$Q12_2) ,na.rm=TRUE)


#decide what to do with trust--is it really a scale...?

governance <- data[c("Q13_1", "Q13_2", "Q13_3", "Q13_4", "Q13_5")]
data$governance <- governance
scree(governance)
governance.fa <- fa(governance, nfactors=1, scores=TRUE,rotate="varimax", fm="pa")
print.psych(governance.fa, cut = 0.3, sort = TRUE)

governance.a <- data.frame(data$Q13_1, data$Q13_2, data$Q13_3, data$Q13_4)
#keys.governance.a <- c(1, 1, 1, 1)
#reverse.code(keys.governance.a, governance.a)
psych::alpha(governance.a)

data$governancemean=rowMeans(cbind(data$Q13_1, data$Q13_2, data$Q13_3, data$Q13_4, data$Q13_5),na.rm=TRUE)


climate_resp <- data[c("Q14_1", "Q14_2", "Q14_3", "Q14_4", "Q14_5", 
                       "Q14_6", "Q14_7", "Q14_8", "Q14_9")]
data$climate_resp <- climate_resp
scree(climate_resp)
climate_resp.fa <- fa(climate_resp, nfactors=2, scores=TRUE,rotate="varimax", fm="pa")
print.psych(climate_resp.fa, cut= 0.3, sort = TRUE)

climate_resp.a <- data.frame(data$Q14_6, data$Q14_1, data$Q14_5, data$Q14_4, data$Q14_3)
alpha(climate_resp.a)
climate_resp.b <- data.frame(data$Q14_7, data$Q14_2, data$Q14_9, data$Q14_8)
alpha(climate_resp.b)

data$climate_resp1mean=rowMeans(cbind(data$Q14_6, data$Q14_1, data$Q14_5, data$Q14_4, data$Q14_3),na.rm=TRUE)
data$climate_resp2mean=rowMeans(cbind(data$Q14_7, data$Q14_9, data$Q14_8),na.rm=TRUE)




nature <- data[c("Q15_1", "Q15_2", "Q15_3", "Q15_4", "Q15_5", "Q15_6", "Q15_7", 
                 "Q15_8", "Q15_9", "Q15_10")]
data$nature <- nature
scree(nature)
nature.fa <- fa(nature, nfactors=3, scores=TRUE,rotate="varimax", fm="pa")
print.psych(nature.fa, cut= 0.3, sort = TRUE)

nature.a <- data.frame(data$Q15_3, data$Q15_5, data$Q15_7, data$Q15_6)
alpha(nature.a) 

nature.b <- data.frame(data$Q15_8, data$Q15_1)
alpha(nature.b)

nature.c <- data.frame(data$Q15_9, data$Q15_4)
alpha(nature.c)

data$nature1mean=rowMeans(cbind(data$Q15_4, data$Q15_3, data$Q15_9, data$Q15_5),na.rm=TRUE)
data$nature2mean=rowMeans(cbind(data$Q15_8, data$Q15_1, data$Q15_2),na.rm=TRUE)



