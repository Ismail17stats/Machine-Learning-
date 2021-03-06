###Blackbox for the code


getwd()
library(readxl)
library(tidyverse)
library(Stat2Data)
library(skimr)
GSS_data <- read_excel("GSS Clean Final Version Ismail.xlsx")


GSS_data <- GSS_data %>% 
  mutate(age_numeric = parse_number(age_of_respondent))


GSS_data$highest_year_school_completed_father <- as.numeric(GSS_data$highest_year_school_completed_father)
#GSS_data$age_numeric <- as.numeric(GSS_data$age_numeric)
#GSS_data$highest_year_of_school_completed <- as.numeric(GSS_data$highest_year_of_school_completed)

ggplot(data = GSS_data) + 

geom_point(aes(y = highest_year_of_school_completed, 
               x = highest_year_school_completed_father ), alpha =0.06)


#geom_point(aes(y = jitter(highest_year_of_school_completed), 
#               x = jitter(high_scool_father)))


model1 <- lm(highest_year_of_school_completed ~ highest_year_school_completed_father , data = GSS_data)


summary(model1)

HYSC <- as.numeric(GSS_data$highest_year_of_school_completed)
model2 <- lm(HYSC ~ age_numeric, data = GSS_data)
summary(model2)

model3 <- lm(highest_year_of_school_completed ~ highest_year_school_completed_father + born_in_us, data = GSS_data)

summary(model3)

model4 <- lm(HYSC ~ age_numeric + born_in_us, data = GSS_data)
summary(model4)


model5 <- lm(highest_year_of_school_completed ~ highest_year_school_completed_father + age_numeric, data = GSS_data)
summary(model5)


GSS_data %>%
  filter(age_numeric==45, highest_year_school_completed_father == 12)%>% 
  slice(1) %>%
  glimpse()

