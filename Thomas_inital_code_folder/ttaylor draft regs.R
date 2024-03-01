library(readr)

library(fixest)

library(tidyverse)
'
apr22pub <- read_csv("apr22pub.csv")

jan20pub <- read_csv("jan20pub.csv")


filt_empl22 <- apr22pub %>% filter(pemlr == 1)

filt_empl20 <- jan20pub %>% filter(pemlr == 1)

filt_empl22$hefaminc <- factor(filt_empl22$hefaminc)

filt_empl22$peeduca <- factor(filt_empl22$ peeduca)

filt_empl22$prwernal

vtable(filt_empl22)
'
library(ipumsr)

ddi <- read_ipums_ddi("Downloads/cps_00001.xml")
data <- read_ipums_micro(ddi)

#factor years, 
data <- data %>% mutate(data, yf = factor(data$YEAR))

#precovid and post covid for diff in diff approaches
data <- data %>% mutate(data, pndmc = case_when( YEAR < 2020 | (YEAR == 2020 & MONTH < 4) ~ "PreCovid",
                                                 TRUE ~ "PostCovid"))




#Industry remapping to make workable, took 280 different indusrty codes and condensed into 16
data <- data %>% mutate(data, indclust = case_when( IND < 1 ~ "NIU",
                                                    IND < 300 ~ "AG & Forestry",
                                                    IND < 1000 ~ "Mining, Utilities, and Construction",
                                                    IND < 1400 ~ "Secondary Food and Tobacco",
                                                    IND < 4000 ~ "Manufacturing",
                                                    IND < 4600 ~ "Wholesale merchant",
                                                    IND < 4700 | (IND > 8769  &  IND < 8781) ~ "AUTO",
                                                    IND < 4900 ~ "Furniture Hardware and Electronics+  Appliances",
                                                    IND < 5000 ~ "Food Sellers",
                                                    IND < 5085 | IND == 7480 | (IND > 7900 & IND < 8300) ~ "Healthcare",
                                                    IND < 6000 ~ "Gas stations and various retail stores",
                                                    IND < 6400 ~ "Transportation and warehousing",
                                                    IND < 6800 | (IND > 8560 & IND < 8600) ~ "Information and entertainment",
                                                    IND < 7075 | (IND > 8659 & IND < 8671) ~ "Finance Insurance and Real estate",
                                                    (IND > 7800 & IND < 7900) ~ "Education",
                                                    TRUE ~ "Other"
                                                    
                                                    ))




data <- data %>% mutate(data, inf = factor(data$IND))


labdata <- data %>% filter(data$LABFORCE > 0)


labdata <- labdata %>% mutate(labdata, labf2 = case_when(LABFORCE == 1 ~ 0,
                                                         TRUE ~ 1))


summary(data)



incdata <- data %>% filter(!is.na(INCTOT))

inc_feols <- feols(FTOTVAL ~ yf + SEX + AGE + factor(EDUC),    data = incdata)

inc2 <- feols(FTOTVAL ~ indclust, data = incdata)

f2 <- feols(labf2 ~ yf + SEX + log(AGE), data = labdata)
f2_2 <- feols(labf2 ~ pndmc + SEX + log(AGE), data = labdata)




f5 <- feols(UHRSWORKT ~ yf + LABFORCE + yf|LABFORCE, data = labdata)

f6 <- feols(HHINCOME ~ yf+  SEX +  factor(LABFORCE) + factor(EDUC),  data = labdata)

#Laborfrce participation rate based on industry cluster
f7 <- feols(labf2 ~ yf + indclust, data = labdata)

f7_2 <- feols(labf2 ~ pndmc + indclust, data = labdata)


#this diff in diff approach probably best bet for main regression


f7_3 <- feols(labf2 ~ pndmc + indclust + (indclust*pndmc), data = labdata)


