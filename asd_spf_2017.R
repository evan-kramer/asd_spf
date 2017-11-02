# ASD SPF 2017
# Evan Kramer
# 9/21/2017

library(tidyverse)
library(lubridate)
library(haven)
library(stringr)
library(readxl)

data = T
spf = T

# Load data
if(data == T) {
    # Achievement
    student_level = read_dta("K:/ORP_accountability/projects/2017_student_level_file/state_student_level_2017_JP_final_10192017.dta") %>% 
        filter(system == 985 & subject != "US History") %>% 
        transmute(system, school, subject, grade, bhn = bhn_group, ed = economically_disadvantaged,
                  el = as.numeric(ell == 1 | ell_t1t2 == 1), swd = special_ed, valid_tests = valid_test, 
                  n_below = as.numeric(str_detect(performance_level, "1.") == T),
                  n_approaching = as.numeric(str_detect(performance_level, "2.") == T),
                  n_on_track = as.numeric(str_detect(performance_level, "3.") == T),
                  n_mastered = as.numeric(str_detect(performance_level, "4.") == T)) 
    
    # Success Rates
    school_summary = read_csv("K:/ORP_accountability/projects/2017_school_accountability/school_summary_file.csv")
    
    # Lexile
    map = read_excel("K:/ORP_accountability/projects/Evan/ASD 2017 SPF/16-17 ASD Lexile Goal Results.xlsx", sheet = 2) %>% 
        mutate(system = 985, school = ifelse(str_detect(`School Name`, "Corning") == T, 5, NA),
               school = ifelse(str_detect(`School Name`, "Westside") == T, 10, school),
               school = ifelse(str_detect(`School Name`, "Frayser Ach") == T, 15, school),
               school = ifelse(str_detect(`School Name`, "Whitney") == T, 20, school),
               school = ifelse(str_detect(`School Name`, "Georgian") == T, 25, school),
               school = ifelse(str_detect(`School Name`, "Grad") == T, 35, school),
               school = ifelse(str_detect(`School Name`, "Pathways F") == T, 45, school),
               school = ifelse(str_detect(`School Name`, "Whitehaven") == T, 50, school),
               school = ifelse(str_detect(`School Name`, "Brick") == T, 8005, school),
               school = ifelse(str_detect(`School Name`, "- L") == T, 8010, school),
               school = ifelse(str_detect(`School Name`, "Humes") == T, 8015, school),
               school = ifelse(str_detect(`School Name`, "KIPP Memphis P") == T, 8020, school),
               school = ifelse(str_detect(`School Name`, "Aspire Hanley 1") == T, 8024, school),
               school = ifelse(str_detect(`School Name`, "Aspire Hanley 2") == T, 8025, school),
               school = ifelse(str_detect(`School Name`, "Klondike") == T, 8035, school),
               school = ifelse(str_detect(`School Name`, "KIPP Memphis A") == T, 8040, school),
               school = ifelse(str_detect(`School Name`, "Aspire Coleman") == T, 8050, school),
               school = ifelse(str_detect(`School Name`, "Fairley") == T, 8055, school),
               school = ifelse(str_detect(`School Name`, "Promise") == T, 8060, school),
               school = ifelse(str_detect(`School Name`, "MLK") == T, 8065, school),
               school = ifelse(str_detect(`School Name`, "Lester P") == T, 8070, school),
               school = ifelse(str_detect(`School Name`, "Freedom") == T, 8075, school),
               school = ifelse(str_detect(`School Name`, "University") == T, 8080, school),
               school = ifelse(str_detect(`School Name`, "Neely") == T, 8090, school),
               school = ifelse(str_detect(`School Name`, "Wood") == T, 8105, school),
               school = ifelse(str_detect(`School Name`, "Denver") == T, 8115, school),
               school = ifelse(str_detect(`School Name`, "Kansas") == T, 8120, school),
               school = ifelse(str_detect(`School Name`, "Guthrie") == T, 8125, school),
               school = ifelse(str_detect(`School Name`, "Egypt") == T, 8130, school),
               school = ifelse(str_detect(`School Name`, "Kirby") == T, 8135, school),
               school = ifelse(str_detect(`School Name`, "Hillcrest") == T, 8140, school))
    
    # ACT
    
    
    
    
    # focus = read_csv("K:/ORP_accountability/projects/2017_school_accountability/focus_exit_improving.csv")
    # reward = read_csv("K:/ORP_accountability/projects/2017_school_accountability/reward_metrics.csv") 
    # cohort2013 = read_csv("K:/ORP_accountability/data/2017_graduation_rate/student_level_20170830.csv")
    # cohort2012 = read_csv("K:/ORP_accountability/data/2016_graduation_rate/student_level_20161201.csv")
    # cohort2011 = read_dta("K:/ORP_accountability/data/2015_graduation_rate/grad_data_2014-15/2011GradCohort.dta")
    # asd_grads = read_csv("K:/ORP_accountability/projects/Evan/ASD 2017 SPF/asd_grads.csv")
    
    # Determine equity eligibility
    ## All
    all = student_level %>% 
        group_by(system, school) %>% 
        summarize_each(funs(sum(., na.rm = T)), valid_tests, starts_with("n_")) %>% 
        mutate(subgroup = "All Students") %>% 
        ungroup()
    
    ## BHN
    bhn = student_level %>% 
        filter(bhn ==  1) %>% 
        group_by(system, school) %>% 
        summarize_each(funs(sum(., na.rm = T)), valid_tests, starts_with("n_")) %>% 
        mutate(subgroup = "Black/Hispanic/Native American") %>% 
        ungroup()
    
    ## Non-BHN
    non_bhn = student_level %>% 
        filter(bhn == 0) %>% 
        group_by(system, school) %>% 
        summarize_each(funs(sum(., na.rm = T)), valid_tests, starts_with("n_")) %>% 
        mutate(subgroup = "Non-Black/Hispanic/Native American") %>% 
        ungroup()
    
    ## ED
    ed = student_level %>% 
        filter(ed == 1) %>% 
        group_by(system, school) %>% 
        summarize_each(funs(sum(., na.rm = T)), valid_tests, starts_with("n_")) %>% 
        mutate(subgroup = "Economically Disadvantaged") %>% 
        ungroup()
    
    ## Non-ED
    non_ed = student_level %>% 
        filter(ed == 0) %>% 
        group_by(system, school) %>% 
        summarize_each(funs(sum(., na.rm = T)), valid_tests, starts_with("n_")) %>% 
        mutate(subgroup = "Non-Economically Disadvantaged") %>% 
        ungroup()
    
    ## EL
    el = student_level %>% 
        filter(el == 1) %>% 
        group_by(system, school) %>% 
        summarize_each(funs(sum(., na.rm = T)), valid_tests, starts_with("n_")) %>% 
        mutate(subgroup = "English Learners incl. T1/T2") %>% 
        ungroup()
    
    ## Non-EL
    non_el = student_level %>% 
        filter(el == 0) %>% 
        group_by(system, school) %>% 
        summarize_each(funs(sum(., na.rm = T)), valid_tests, starts_with("n_")) %>% 
        mutate(subgroup = "Non-English Learners incl. T1/T2") %>% 
        ungroup()
    
    ## SWD
    swd = student_level %>% 
        filter(swd == 1) %>% 
        group_by(system, school) %>% 
        summarize_each(funs(sum(., na.rm = T)), valid_tests, starts_with("n_")) %>% 
        mutate(subgroup = "Students with Disabilities") %>% 
        ungroup() 
    
    ## Non-SWD
    non_swd = student_level %>% 
        filter(swd == 0) %>% 
        group_by(system, school) %>% 
        summarize_each(funs(sum(., na.rm = T)), valid_tests, starts_with("n_")) %>% 
        mutate(subgroup = "Non-Students with Disabilities") %>% 
        ungroup() 
    
    ## Bind all rows together
    equity_eligible = bind_rows(all, bhn, ed, el, non_bhn, non_ed, non_el, non_swd, swd) %>% 
        arrange(system, school, subgroup) %>% 
        select(system, school, subgroup, valid_tests) %>% 
        filter(valid_tests >= 30) %>% 
        spread(subgroup, valid_tests) %>% 
        mutate(equity_eligible_bhn = as.numeric(`Black/Hispanic/Native American` >= 30 & `Non-Black/Hispanic/Native American` >= 30),
               equity_eligible_ed = as.numeric(`Economically Disadvantaged` >= 30 & `Non-Economically Disadvantaged` >= 30),
               equity_eligible_el = as.numeric(`English Learners incl. T1/T2` >= 30 & `Non-English Learners incl. T1/T2` >= 30),
               equity_eligible_swd = as.numeric(`Students with Disabilities` >= 30 & `Non-Students with Disabilities` >= 30)) %>% 
        mutate_each(funs(ifelse(is.na(.), 0, .)), starts_with("equity_eligible")) %>% 
        mutate(equity_eligible = as.numeric(equity_eligible_bhn == 1 | equity_eligible_ed == 1 | 
                                                equity_eligible_el == 1 | equity_eligible_swd == 1)) %>% 
        rename(valid_tests_all = `All Students`, valid_tests_bhn = `Black/Hispanic/Native American`, 
               valid_tests_ed = `Economically Disadvantaged`, valid_tests_el = `English Learners incl. T1/T2`,
               valid_tests_non_bhn = `Non-Black/Hispanic/Native American`, valid_tests_non_ed = `Non-Economically Disadvantaged`,
               valid_tests_non_el = `Non-English Learners incl. T1/T2`, valid_tests_non_swd = `Non-Students with Disabilities`,
               valid_tests_swd = `Students with Disabilities`)
}

# School Performance Framework
if(spf == T) {
    # Mission
    mission = school_summary %>% 
        filter(system == 985 & subgroup == "All Students") %>% 
        full_join(select(equity_eligible, system, school, equity_eligible), by = c("system", "school")) %>% 
        transmute(system, system_name, school, school_name, subgroup, pool, designation_ineligible, equity_eligible,
                  success_rate_2017, success_rate_2017_pctile, target = 6.5, 
                  pts_possible = ifelse(designation_ineligible == 1, NA, ifelse(equity_eligible == 1, 20, 15)), 
                  pts_earned = ifelse(is.na(pts_possible), NA, 
                                      ifelse(success_rate_2017_pctile >= target, pts_possible, pts_possible * .6)),
                  performance = ifelse(success_rate_2017_pctile >= target, "Exceeds", "Does Not Meet"),
                  category = "Mission")
    
    # School Progress
    
    # Student Progress
    student_progress = school_summary %>% 
        filter(system == 985 & subgroup == "All Students") %>% 
        left_join(read_excel("K:/ORP_accountability/data/2017_TVAAS/2017 School Composites.xlsx") %>% 
                      transmute(system = as.integer(`District Number`), school = as.integer(`School Number`), 
                                tvaas_composite = `School-Wide: Composite`), by = c("system", "school")) %>% 
        transmute(system, system_name, school, school_name, subgroup, pool, designation_ineligible, 
                  tvaas_composite, pts_possible = ifelse(pool == "K8", 30, 20),
                  pts_earned = ifelse(tvaas_composite == 5, pts_possible, 
                               ifelse(tvaas_composite == 4, pts_possible * 0.8, 
                                      ifelse(tvaas_composite == 3, pts_possible * 0.6, pts_possible * 0.4))),
                  performance = ifelse(tvaas_composite == 5, "Exceeds", 
                                       ifelse(tvaas_composite == 4, "Meets",
                                              ifelse(tvaas_composite == 3, "Approaches", "Does Not Meet")))) %>% 
        mutate_each(funs(ifelse(designation_ineligible == 1, NA, .)), starts_with("pts"), performance) %>% 
        mutate(category = "Student Progress")
    
    # College and Career
    ## Lexile Growth
    lexile_growth = map %>% 
        transmute(system, school, id = `Student ID`, 
                  met_lexile_goal = ifelse(`Met Lexile Goal` == "Y", 1, ifelse(`Met Lexile Goal` == "N", 0, NA))) %>% 
        arrange(id, desc(met_lexile_goal)) %>% 
        group_by(id) %>% 
        summarize_all(funs(first(.))) %>% 
        filter(!is.na(met_lexile_goal)) %>% 
        group_by(system, school) %>% 
        summarize(valid_tests = n(), met_lexile_goal = sum(met_lexile_goal, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(pct_met_lexile_goal = round(100 * met_lexile_goal / valid_tests, 1)) %>% 
        full_join(select(mission, system, school, pool, subgroup, designation_ineligible, equity_eligible), by = c("system", "school")) %>% 
        mutate(pts_possible = ifelse(pool == "K8", ifelse(equity_eligible == 1, 15, 20), 15),
               pts_earned = ifelse(pct_met_lexile_goal >= 60, pts_possible, 
                                   ifelse(pct_met_lexile_goal >= 50 & pct_met_lexile_goal < 60, 0.8 * pts_possible, 
                                          ifelse(pct_met_lexile_goal >= 35 & pct_met_lexile_goal < 50, 0.6 * pts_possible, 0.4 * pts_possible))),
               performance = ifelse(pct_met_lexile_goal >= 60, "Exceeds", 
                                    ifelse(pct_met_lexile_goal >= 50 & pct_met_lexile_goal < 60, "Meets", 
                                           ifelse(pct_met_lexile_goal >= 35 & pct_met_lexile_goal < 50, "Approaches", "Does Not Meet")))) %>% 
        mutate_each(funs(ifelse(designation_ineligible == 1, NA, .)), starts_with("pts"), performance) %>% 
        mutate(category = "College and Career - Lexile Growth")
    
    ## ACT - mean composite, junior, HS only
    
    
    ## Extended Graduation Rate - HS only
    
    # Equity
}

# K2
## Amy to take care of this

# K8
## Mission - one-year success rate
# mission = school_summary %>% 
#     filter(system == 985 & subgroup == "All Students") %>% 
#     transmute(system, system_name, school, school_name, subgroup, pool, designation_ineligible,
#               success_rate_2017, success_rate_2017_pctile)
    
## School Progress - best of two subject-specific TVAAS and percentile rank on math, ELA, science success rate 
### Subject-specific TVAAS
### Percentile rank on math, ELA, science success rate
## Student Progress - TVAAS composite
## College and Career - Lexile growth (% at grade level or 1.5 years)
## Equity - subgroup percentile rank on math, ELA, and science SSR

# HS
## Mission - one-year success rate
## School Progress - best of three subject-specific TVAAS and percentile rank on math, ELA, science success rate 
### Subject-specific TVAAS
### Percentile rank on math, ELA, science success rate
### Gains in percent prof/adv
## Student Progress - TVAAS composite
## College and Career - Lexile growth (% at grade level or 1.5 years), average ACT composite (junior day, current year?), extended grad rate gains
## Equity - subgroup percentile rank on math, ELA, and science SSR and subgroup gains in percent prof/adv

# Alt Ed
