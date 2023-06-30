#libraries
library(dplyr)
library(tidyr)
library(here)
library(janitor)

#load data
data <- read.csv(here("Documents", "developmental.csv")) %>% 
  clean_names()

#filter academic interest devo 1st and faculty devo 1st
primary_devo <- data %>% 
  filter(academic_interest_1 == "Developmental Psychology") %>% 
  filter(faculty1 %in% c("Gweon, Hyowon",
                        "Frank, Michael",
                        "Ellis, Cameron",
                        "Markman, Ellen",
                        "Dweck, Carol",
                        "Roberts, Steven")) %>% 
mutate(ethnicity_african = if_else(ipeds_classification == "Black or African American", "Y",""),
       ethnicity_american_indian_alaskan_native = if_else(ipeds_classification == "American Indian or Alaska Native", "Y",""),
       ethnicity_asian = if_else(ipeds_classification == "Asian", "Y",""),
       ethnicity_hawaiian_pacific_islander = if_else(ipeds_classification == "Hawaiian or Pacific Islander", "Y",""),
       ethnicity_white = if_else(ipeds_classification == "White", "Y",""))

whospopular <- primary_devo %>% 
  count(faculty1)

View(whospopular)

#filter academic interest devo 1st or 2nd or faculty devo 1st
secondary_devo <- data %>% 
  filter(academic_interest_1 == "Developmental Psychology"|
           academic_interest_2 == "Developmental Psychology"|
           faculty1 %in% c("Gweon, Hyowon",
                         "Frank, Michael",
                         "Ellis, Cameron",
                         "Markman, Ellen",
                         "Dweck, Carol",
                         "Roberts, Steven")) %>% 
  subset(!(applicant_id %in% primary_devo$applicant_id)) %>% 
  mutate(ethnicity_african = if_else(ipeds_classification == "Black or African American", "Y",""),
         ethnicity_american_indian_alaskan_native = if_else(ipeds_classification == "American Indian or Alaska Native", "Y",""),
         ethnicity_asian = if_else(ipeds_classification == "Asian", "Y",""),
         ethnicity_hawaiian_pacific_islander = if_else(ipeds_classification == "Hawaiian or Pacific Islander", "Y",""),
         ethnicity_white = if_else(ipeds_classification == "White", "Y",""))

#filter academic interest devo 3rd or faculty devo 2nd or 3rd
tertiary_devo <- data %>% 
  filter(academic_interest_3 == "Developmental Psychology"|
           faculty2 %in% c("Gweon, Hyowon",
                           "Frank, Michael",
                           "Ellis, Cameron",
                           "Markman, Ellen",
                           "Dweck, Carol",
                           "Roberts, Steven") |
           faculty3 %in% c("Gweon, Hyowon",
                           "Frank, Michael",
                           "Ellis, Cameron",
                           "Markman, Ellen",
                           "Dweck, Carol",
                           "Roberts, Steven")) %>% 
  subset(!(applicant_id %in% primary_devo$applicant_id)) %>% 
  subset(!(applicant_id %in% secondary_devo$applicant_id)) %>% 
  mutate(ethnicity_african = if_else(ipeds_classification == "Black or African American", "Y",""),
         ethnicity_american_indian_alaskan_native = if_else(ipeds_classification == "American Indian or Alaska Native", "Y",""),
         ethnicity_asian = if_else(ipeds_classification == "Asian", "Y",""),
         ethnicity_hawaiian_pacific_islander = if_else(ipeds_classification == "Hawaiian or Pacific Islander", "Y",""),
         ethnicity_white = if_else(ipeds_classification == "White", "Y",""))

write.csv(primary_devo, "primary_devo.csv", row.names=FALSE)
write.csv(secondary_devo, "secondary_devo.csv", row.names=FALSE)
write.csv(tertiary_devo, "tertiary_devo.csv", row.names=FALSE)
