setwd("C:/Users/a02339097/Box/Rivers Lab/Erin Rivers/DWQ TMDL Prioritization/Survey/Data and Analysis")
library(tidyverse);library(xlsx);library(openxlsx)

dat_wide <- read_csv("wqsurvey_results_11_14_23.csv")
dat_wide <- dat_wide[,-c(40:47)]

dat <- dat_wide %>% gather(question,value,Q1_drinking_water:Q10_knowledge,factor_key=TRUE)
dat <- dat[!is.na(dat$value),]

all <- dat %>% 
  group_by(question) %>% 
  summarize(mean=mean(value))

basin <- dat %>% 
  group_by(Basin,question) %>% 
  summarize(mean=mean(value))

residence <- dat %>% 
  group_by(Residence,question) %>% 
  summarize(mean=mean(value))

age <- dat %>% 
  group_by(Age,question) %>% 
  summarize(mean=mean(value))

education <- dat %>% 
  group_by(Education,question) %>% 
  summarize(mean=mean(value))

gender <- dat %>% 
  group_by(Gender,question) %>% 
  summarize(mean=mean(value))

race <- dat %>% 
  group_by(Race,Ethnicity,question) %>% 
  summarize(mean=mean(value))

results <- list("All" =all, "Basin" = basin, "Residence" = residence, 
                "Age" = age, "Education" = education, 
                "Gender" = gender, "Race" = race)
write.xlsx(results, file = "results_summary_111423.xlsx")

