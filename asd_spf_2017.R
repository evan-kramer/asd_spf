# ACT Highest Score File
# Evan Kramer
# 9/21/2017

library(tidyverse)
library(lubridate)
library(haven)
library(stringr)

# Load data
pools = read_csv("K:/ORP_accountability/data/2017_final_accountability_files/school_base_2017_JW.csv") %>% 
    filter(system == 985 & subgroup == "All Students" & grade %in% c("9th through 12th", "All Grades")) %>% 
    group_by(system, school, subject) %>% 
    summarize(grad_cohort = sum(as.numeric(grad_cohort), na.rm = T)) %>% 
    ungroup() %>% 
    spread(subject, grad_cohort) %>% 
    transmute(system, school, pool = case_when(
        is.na(pools$`Graduation Rate`) | pools$`Graduation Rate` < 30 ~ "K8",
        pools$`Graduation Rate` >= 30 ~ "HS"))

base = read_csv("K:/ORP_accountability/data/2017_final_accountability_files/school_base_2017_JW.csv") %>% 
    filter(system == 985 & !grade %in% c("9th through 12th", "All Grades") &
               !subject %in% c("ACT Math", "ACT Reading", "US History")) 

base = base %>% 
    mutate(subject = case_when(
        base$subject %in% c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III") ~ "Math",
        str_detect(base$subject, "English") == T ~ "ELA",
        base$subject %in% c("Biology I", "Chemistry") ~ "Science")) %>% 
    group_by(year, system, subgroup)
    

# K2
## Amy to take care of this

# K8
## School Progress - best of two subject-specific TVAAS and percentile rank on math, ELA, science success rate 
