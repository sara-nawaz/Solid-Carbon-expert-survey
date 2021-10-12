rm(list = ls())
library(tidyverse)

data <- read.csv("Solid carbon_Expert survey_August 23, 2021_10.58_NUMERIC.csv", sep= ',', stringsAsFactors=FALSE, header = TRUE)

#clean data
data$expert <- data$Q1 
as.numeric(data$expert)

data$Q2_1 <-dplyr::na_if(data$Q2_1, 6) #remove na's
data$Q2_2 <-dplyr::na_if(data$Q2_2, 6) #remove na's
data$Q2_3 <-dplyr::na_if(data$Q2_3, 6) #remove na's
data$Q2_4 <-dplyr::na_if(data$Q2_4, 6) #remove na's

#CHANGED CODE back ...
data$DAC <- recode(data$Q3.1, "46" = "1",
       "47" = "2",
       "48" = "3",
       "49" = "4",
       "50" = "5")
data$DAC <-dplyr::na_if(data$DAC, 51) #remove na's
as.numeric(data$DAC)

data$DAC_offshore <- recode(data$Q3.2, "1" = "1",
                   "2" = "2",
                   "3" = "3",
                   "4" = "4",
                   "5" = "5")
data$DAC_offshore <-dplyr::na_if(data$DAC_offshore, 6) #remove na's
as.numeric(data$DAC_offshore)

data$DAC_onshore <- recode(data$Q3.3, "1" = "1",
                           "2" = "2",
                           "3" = "3",
                           "4" = "4",
                           "5" = "5")
data$DAC_onshore <-dplyr::na_if(data$DAC_onshore, 6) #remove na's
as.numeric(data$DAC_onshore)

data$alkalin <- recode(data$Q3.4, "1" = "1",
                       "2" = "2",
                       "3" = "3",
                       "4" = "4",
                       "5" = "5")
data$alkalin <-dplyr::na_if(data$alkalin, 6) #remove na's
as.numeric(data$alkalin)


data$weathering <- recode(data$Q3.5, "1" = "1",
                          "2" = "2",
                          "3" = "3",
                          "4" = "4",
                          "5" = "5")
data$weathering <-dplyr::na_if(data$weathering, 6) #remove na's
as.numeric(data$weathering)

data$fertil <- recode(data$Q3.6, "1" = "1",
                      "2" = "2",
                      "3" = "3",
                      "4" = "4",
                      "5" = "5")
data$fertil <-dplyr::na_if(data$fertil, 6) #remove na's
as.numeric(data$fertil)

data$coast_rest <- recode(data$Q3.7, "1" = "1",
                          "2" = "2",
                          "3" = "3",
                          "4" = "4",
                          "5" = "5")
data$coast_rest <-dplyr::na_if(data$coast_rest, 6) #remove na's
as.numeric(data$coast_rest)

data$afforest <- recode(data$Q3.8, "1" = "1",
                        "2" = "2",
                        "3" = "3",
                        "4" = "4",
                        "5" = "5")
data$afforest <-dplyr::na_if(data$afforest, 6) #remove na's
as.numeric(data$afforest)

data$biochar <- recode(data$Q3.9,"1" = "1",
                       "2" = "2",
                       "3" = "3",
                       "4" = "4",
                       "5" = "5")
data$biochar <-dplyr::na_if(data$biochar, 6) #remove na's
as.numeric(data$biochar)


data$soil <- recode(data$Q3.10, "1" = "1",
                    "2" = "2",
                    "3" = "3",
                    "4" = "4",
                    "5" = "5")
data$soil <-dplyr::na_if(data$soil, 6) #remove na's
as.numeric(data$soil)


data$Q4 <-dplyr::na_if(data$Q4, 6) #remove na's
data$Q4 <- recode(data$Q4, "5" = "3")
as.numeric(data$Q4)

data$Q5 <- recode(data$Q5, "1" = "1",
                  "2" = "2",
                  "3" = "3",
                  "4" = "4",
                  "5" = "5")
data$Q5 <-dplyr::na_if(data$Q5, 6) #remove na's
as.numeric(data$Q5)

data$Q4 <- recode(data$Q4, "5" = "3")
data$Q6 <-dplyr::na_if(data$Q6, 6) #remove na's
data$Q6 <-dplyr::na_if(data$Q6, 9) #remove na's
as.numeric(data$Q6)

data$Q7_1 <-dplyr::na_if(data$Q7_1, 6) #remove na's
as.numeric(data$Q7_1)

data$Q7_2 <-dplyr::na_if(data$Q7_2, 6) #remove na's
as.numeric(data$Q7_2)

data$Q7_3 <-dplyr::na_if(data$Q7_3, 6) #remove na's
as.numeric(data$Q7_3)

data$Q7_4 <-dplyr::na_if(data$Q7_4, 6) #remove na's
as.numeric(data$Q7_4)

data$Q7_5 <-dplyr::na_if(data$Q7_5, 6) #remove na's
as.numeric(data$Q7_5)

data$Q7_6 <-dplyr::na_if(data$Q7_6, 6) #remove na's
as.numeric(data$Q7_6)


data$Q8a_1 <-dplyr::na_if(data$Q8a_1, 6) #remove na's
as.numeric(data$Q8a_1)

data$Q8a_2 <-dplyr::na_if(data$Q8a_2, 6) #remove na's
as.numeric(data$Q8a_2)

data$Q8a_3 <-dplyr::na_if(data$Q8a_3, 6) #remove na's
as.numeric(data$Q8a_3)

data$Q8a_4 <-dplyr::na_if(data$Q8a_4, 6) #remove na's
as.numeric(data$Q8a_4)

data$Q8a_5 <-dplyr::na_if(data$Q8a_5, 6) #remove na's
as.numeric(data$Q8a_5)

data$Q8a_6 <-dplyr::na_if(data$Q8a_6, 6) #remove na's
as.numeric(data$Q8a_6)

data$Q8a_7 <-dplyr::na_if(data$Q8a_7, 6) #remove na's
as.numeric(data$Q8a_7)

data$Q8a_8 <-dplyr::na_if(data$Q8a_8, 6) #remove na's
as.numeric(data$Q8a_8)

data$Q8a_9 <-dplyr::na_if(data$Q8a_9, 6) #remove na's
as.numeric(data$Q8a_9)

data$Q8a_10 <-dplyr::na_if(data$Q8a_10, 6) #remove na's
as.numeric(data$Q8a_10)

data$Q8a_11 <-dplyr::na_if(data$Q8a_11, 6) #remove na's
as.numeric(data$Q8a_11)



data$Q8b_1 <-dplyr::na_if(data$Q8b_1, 6) #remove na's
as.numeric(data$Q8b_1)

data$Q8b_2 <-dplyr::na_if(data$Q8b_2, 6) #remove na's
as.numeric(data$Q8b_2)

data$Q8b_3 <-dplyr::na_if(data$Q8b_3, 6) #remove na's
as.numeric(data$Q8b_3)

data$Q8b_4 <-dplyr::na_if(data$Q8b_4, 6) #remove na's
as.numeric(data$Q8b_4)

data$Q8b_5 <-dplyr::na_if(data$Q8b_5, 6) #remove na's
as.numeric(data$Q8b_5)

data$Q8b_6 <-dplyr::na_if(data$Q8b_6, 6) #remove na's
as.numeric(data$Q8b_6)

data$Q8b_7 <-dplyr::na_if(data$Q8b_7, 6) #remove na's
as.numeric(data$Q8b_7)

data$Q8b_8 <-dplyr::na_if(data$Q8a_8, 6) #remove na's
as.numeric(data$Q8b_8)

data$Q8b_9 <-dplyr::na_if(data$Q8b_9, 6) #remove na's
as.numeric(data$Q8b_9)

data$Q8b_10 <-dplyr::na_if(data$Q8b_10, 6) #remove na's
as.numeric(data$Q8b_10)

data$Q8b_11 <-dplyr::na_if(data$Q8b_11, 6) #remove na's
as.numeric(data$Q8b_11)


data$Q9a_1 <-dplyr::na_if(data$Q9a_1, 6) #remove na's
as.numeric(data$Q9a_1)

data$Q9a_2 <-dplyr::na_if(data$Q9a_2, 6) #remove na's
as.numeric(data$Q9a_2)

data$Q9a_3 <-dplyr::na_if(data$Q9a_3, 6) #remove na's
as.numeric(data$Q9_3)

data$Q9a_4 <-dplyr::na_if(data$Q9a_4, 6) #remove na's
as.numeric(data$Q9a_4)

data$Q9a_5 <-dplyr::na_if(data$Q9a_5, 6) #remove na's
as.numeric(data$Q9a_5)

data$Q9a_6 <-dplyr::na_if(data$Q9a_6, 6) #remove na's
as.numeric(data$Q9a_6)

data$Q9a_7 <-dplyr::na_if(data$Q9a_7, 6) #remove na's
as.numeric(data$Q9a_7)

data$Q9a_8 <-dplyr::na_if(data$Q9a_8, 6) #remove na's
as.numeric(data$Q9a_8)

data$Q9a_9 <-dplyr::na_if(data$Q9a_9, 6) #remove na's
as.numeric(data$Q9a_9)

data$Q9a_10 <-dplyr::na_if(data$Q9a_10, 6) #remove na's
as.numeric(data$Q9a_10)


data$Q9b_1 <-dplyr::na_if(data$Q9b_1, 6) #remove na's
as.numeric(data$Q9b_1)

data$Q9b_2 <-dplyr::na_if(data$Q9b_2, 6) #remove na's
as.numeric(data$Q9b_2)

data$Q9b_3 <-dplyr::na_if(data$Q9b_3, 6) #remove na's
as.numeric(data$Q9b_3)

data$Q9b_4 <-dplyr::na_if(data$Q9b_4, 6) #remove na's
as.numeric(data$Q9b_4)

data$Q9b_5 <-dplyr::na_if(data$Q9b_5, 6) #remove na's
as.numeric(data$Q9b_5)

data$Q9b_6 <-dplyr::na_if(data$Q9b_6, 6) #remove na's
as.numeric(data$Q9b_6)

data$Q9b_7 <-dplyr::na_if(data$Q9b_7, 6) #remove na's
as.numeric(data$Q9b_7)

data$Q9b_8 <-dplyr::na_if(data$Q9b_8, 6) #remove na's
as.numeric(data$Q9b_8)

data$Q9b_9 <-dplyr::na_if(data$Q9b_9, 6) #remove na's
as.numeric(data$Q9b_9)

data$Q9b_10 <-dplyr::na_if(data$Q9b_10, 6) #remove na's
as.numeric(data$Q9b_10)




data$Q10 <- recode(data$Q10, "1" = "-2",
                    "2" = "-1",
                    "3" = "0",
                    "4" = "1",
                    "5" = "2")
data$Q10 <-dplyr::na_if(data$Q10, 6) #remove na's
as.numeric(data$Q10)
data$support <- data$Q10


data$Q12_1 <- recode(data$Q12_1, "1" = "-2",
                   "2" = "-1",
                   "3" = "0",
                   "4" = "1",
                   "5" = "2")
data$Q12_1 <-dplyr::na_if(data$Q12_1, 6) #remove na's
as.numeric(data$Q12_1)

data$Q12_2 <- recode(data$Q12_2, "1" = "-2",
                     "2" = "-1",
                     "3" = "0",
                     "4" = "1",
                     "5" = "2")
data$Q12_2 <-dplyr::na_if(data$Q12_2, 6) #remove na's
as.numeric(data$Q12_2)


data$Q12_3 <- recode(data$Q12_3, "1" = "-2",
                     "2" = "-1",
                     "3" = "0",
                     "4" = "1",
                     "5" = "2")
data$Q12_3 <-dplyr::na_if(data$Q12_3, 6) #remove na's
as.numeric(data$Q12_3)

data$Q12_4 <- recode(data$Q12_4, "1" = "-2",
                     "2" = "-1",
                     "3" = "0",
                     "4" = "1",
                     "5" = "2")
data$Q12_4 <-dplyr::na_if(data$Q12_4, 6) #remove na's
as.numeric(data$Q12_4)

data$Q12_5 <- recode(data$Q12_5, "1" = "-2",
                     "2" = "-1",
                     "3" = "0",
                     "4" = "1",
                     "5" = "2")
data$Q12_5 <-dplyr::na_if(data$Q12_5, 6) #remove na's
as.numeric(data$Q12_5)

data$Q12_6 <- recode(data$Q12_6, "1" = "-2",
                     "2" = "-1",
                     "3" = "0",
                     "4" = "1",
                     "5" = "2")
data$Q12_6 <-dplyr::na_if(data$Q12_6, 6) #remove na's
as.numeric(data$Q12_6)

data$Q12_7 <- recode(data$Q12_7, "1" = "-2",
                     "2" = "-1",
                     "3" = "0",
                     "4" = "1",
                     "5" = "2")
data$Q12_7 <-dplyr::na_if(data$Q12_7, 6) #remove na's
as.numeric(data$Q12_7)

data$Q12_8 <- recode(data$Q12_8, "1" = "-2",
                     "2" = "-1",
                     "3" = "0",
                     "4" = "1",
                     "5" = "2")
data$Q12_8 <-dplyr::na_if(data$Q12_8, 6) #remove na's
as.numeric(data$Q12_8)

data$Q12_9 <- recode(data$Q12_9, "1" = "-2",
                     "2" = "-1",
                     "3" = "0",
                     "4" = "1",
                     "5" = "2")
data$Q12_9 <-dplyr::na_if(data$Q12_9, 6) #remove na's
as.numeric(data$Q12_9)


data$Q12_10 <- recode(data$Q12_10, "1" = "-2",
                     "2" = "-1",
                     "3" = "0",
                     "4" = "1",
                     "5" = "2")
data$Q12_10 <-dplyr::na_if(data$Q12_10, 6) #remove na's
as.numeric(data$Q12_10)

data$Q12_11 <- recode(data$Q12_11, "1" = "-2",
                      "2" = "-1",
                      "3" = "0",
                      "4" = "1",
                      "5" = "2")
data$Q12_11 <-dplyr::na_if(data$Q12_11, 6) #remove na's
as.numeric(data$Q12_11)


data$Q13_1 <- recode(data$Q13_1, "1" = "-2",
                      "2" = "-1",
                      "3" = "0",
                      "4" = "1",
                      "5" = "2")
data$Q13_1 <-dplyr::na_if(data$Q13_1, 6) #remove na's
as.numeric(data$Q13_1)


data$Q13_2 <- recode(data$Q13_2, "1" = "2",
                     "2" = "1",
                     "3" = "0",
                     "4" = "-1",
                     "5" = "-2") # REVERSE CODED
data$Q13_2 <-dplyr::na_if(data$Q13_2, 6) #remove na's
as.numeric(data$Q13_2)

data$Q13_3 <- recode(data$Q13_3, "1" = "-2",
                     "2" = "-1",
                     "3" = "0",
                     "4" = "1",
                     "5" = "2")
data$Q13_3 <-dplyr::na_if(data$Q13_3, 6) #remove na's
as.numeric(data$Q13_3)

data$Q13_4 <- recode(data$Q13_4, "1" = "-2",
                     "2" = "-1",
                     "3" = "0",
                     "4" = "1",
                     "5" = "2")
data$Q13_4 <-dplyr::na_if(data$Q13_4, 6) #remove na's
as.numeric(data$Q13_4)

data$Q13_5 <- recode(data$Q13_5, "1" = "-2",
                     "2" = "-1",
                     "3" = "0",
                     "4" = "1",
                     "5" = "2")
data$Q13_5 <-dplyr::na_if(data$Q13_5, 6) #remove na's
as.numeric(data$Q13_5)



data$Q14_1 <- recode(data$Q14_1, "1" = "2",
                     "2" = "1",
                     "3" = "0",
                     "4" = "-1",
                     "5" = "-2") # reverse coded
data$Q14_1 <-dplyr::na_if(data$Q14_1, 6) #remove na's
as.numeric(data$Q14_1)

data$Q14_2 <- recode(data$Q14_2, "1" = "-2",
                     "2" = "-1",
                     "3" = "0",
                     "4" = "1",
                     "5" = "2")
data$Q14_2 <-dplyr::na_if(data$Q14_2, 6) #remove na's
as.numeric(data$Q14_2)

data$Q14_3 <- recode(data$Q14_3, "1" = "2",
                     "2" = "1",
                     "3" = "0",
                     "4" = "-1",
                     "5" = "-2") #reverse coded
data$Q14_3 <-dplyr::na_if(data$Q14_3, 6) #remove na's
as.numeric(data$Q14_3)

data$Q14_4 <- recode(data$Q14_4, "1" = "2",
                     "2" = "1",
                     "3" = "0",
                     "4" = "-1",
                     "5" = "-2") #reverse coded
data$Q14_4 <-dplyr::na_if(data$Q14_4, 6) #remove na's
as.numeric(data$Q14_4)

data$Q14_5 <- recode(data$Q14_5, "1" = "2",
                     "2" = "1",
                     "3" = "0",
                     "4" = "-1",
                     "5" = "-2") #reversed
data$Q14_5 <-dplyr::na_if(data$Q14_5, 6) #remove na's
as.numeric(data$Q14_5)

data$Q14_6 <- recode(data$Q14_6, "1" = "2",
                     "2" = "1",
                     "3" = "0",
                     "4" = "-1",
                     "5" = "-2") #reverse coded
data$Q14_6 <-dplyr::na_if(data$Q14_6, 6) #remove na's
as.numeric(data$Q14_6)

data$Q14_7 <- recode(data$Q14_7, "1" = "-2",
                     "2" = "-1",
                     "3" = "0",
                     "4" = "1",
                     "5" = "2")
data$Q14_7 <-dplyr::na_if(data$Q14_7, 6) #remove na's
as.numeric(data$Q14_7)

data$Q14_8 <- recode(data$Q14_8, "1" = "-2",
                     "2" = "-1",
                     "3" = "0",
                     "4" = "1",
                     "5" = "2")
data$Q14_8 <-dplyr::na_if(data$Q14_8, 6) #remove na's
as.numeric(data$Q14_8)

data$Q14_9 <- recode(data$Q14_9, "1" = "-2",
                     "2" = "-1",
                     "3" = "0",
                     "4" = "1",
                     "5" = "2")
data$Q14_9 <-dplyr::na_if(data$Q14_9, 6) #remove na's
as.numeric(data$Q14_9)



data$Q15_1 <- recode(data$Q15_1, "1" = "2",
                     "2" = "1",
                     "3" = "0",
                     "4" = "-1",
                     "5" = "-2") #reverse coded
data$Q15_1 <-dplyr::na_if(data$Q15_1, 6) #remove na's
as.numeric(data$Q15_1)

data$Q15_2 <- recode(data$Q15_2, "1" = "2",
                     "2" = "1",
                     "3" = "0",
                     "4" = "-1",
                     "5" = "-2") #reverse coded
data$Q15_2 <-dplyr::na_if(data$Q15_2, 6) #remove na's
as.numeric(data$Q15_2)

data$Q15_2 <- recode(data$Q15_2, "1" = "-2",
                     "2" = "-1",
                     "3" = "0",
                     "4" = "1",
                     "5" = "2")
data$Q15_3 <-dplyr::na_if(data$Q15_3, 6) #remove na's
as.numeric(data$Q15_3)

data$Q15_4 <- recode(data$Q15_4, "1" = "-2",
                     "2" = "-1",
                     "3" = "0",
                     "4" = "1",
                     "5" = "2")
data$Q15_4 <-dplyr::na_if(data$Q15_4, 6) #remove na's
as.numeric(data$Q15_4)

data$Q15_5 <- recode(data$Q15_5, "1" = "-2",
                     "2" = "-1",
                     "3" = "0",
                     "4" = "1",
                     "5" = "2")
data$Q15_5 <-dplyr::na_if(data$Q15_5, 6) #remove na's
as.numeric(data$Q15_5)

data$Q15_6 <- recode(data$Q15_6, "1" = "-2",
                     "2" = "-1",
                     "3" = "0",
                     "4" = "1",
                     "5" = "2")
data$Q15_6 <-dplyr::na_if(data$Q15_6, 6) #remove na's
as.numeric(data$Q15_6)

data$Q15_7 <- recode(data$Q15_7, "1" = "-2",
                     "2" = "-1",
                     "3" = "0",
                     "4" = "1",
                     "5" = "2")
data$Q15_7 <-dplyr::na_if(data$Q15_7, 6) #remove na's
as.numeric(data$Q15_7)

data$Q15_8 <- recode(data$Q15_8, "1" = "2",
                     "2" = "1",
                     "3" = "0",
                     "4" = "-1",
                     "5" = "-2")
data$Q15_8 <-dplyr::na_if(data$Q15_8, 6) #remove na's
as.numeric(data$Q15_8)


data$Q15_9 <- recode(data$Q15_9, "1" = "-2",
                     "2" = "-1",
                     "3" = "0",
                     "4" = "1",
                     "5" = "2")
data$Q15_9 <-dplyr::na_if(data$Q15_9, 6) #remove na's
as.numeric(data$Q15_9)

data$Q15_10 <- recode(data$Q15_10, "1" = "-2",
                     "2" = "-1",
                     "3" = "0",
                     "4" = "1",
                     "5" = "2")
data$Q15_10 <-dplyr::na_if(data$Q15_10, 6) #remove na's
as.numeric(data$Q15_10)

data$gender <-dplyr::na_if(data$Q17, 3) #remove na's
data$gender <-dplyr::na_if(data$Qgender, 4) #remove na's
as.numeric(data$gender)

data$age <- data$Q18

data$political <-dplyr::na_if(data$Q19, 6) #remove na's
as.numeric(data$political)


data$degree <-dplyr::na_if(data$Q20, 9) #remove na's
data$degree <- recode(data$degree,
                   "6" = "Master's",
                   "7" = "Doctoral",
                   "5" = "Bachelor's")

write_csv(data, file = "expert survey_cleaned.csv")