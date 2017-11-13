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
    
    # Percent on track/mastered
    school_numeric = read_excel("J:/WEBPAGES/NCLBAppeals/Accountability Web Files/985_SchoolNumericFile_12Oct2017.xlsx") %>% 
        filter(str_detect(subject, "Math") == T | str_detect(subject, "HS") == T | str_detect(subject, "ELA") == T) %>% 
        mutate(subgroup = ifelse(subgroup == "English Learners", "English Learners with T1/T2", subgroup))
    
    # Success Rates
    school_summary = read_csv("K:/ORP_accountability/projects/2017_school_accountability/school_summary_file.csv") %>% 
        mutate(pool = ifelse(system == 985 & school == 8065, "HS", pool),
               pool = ifelse(system == 985 & school == 8140, "HS", pool),
               pool = ifelse(system == 985 & school == 35, "HS", pool))
    
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
    act = read_dta("K:/Assessment_Data Returns/ACT/2016-17/Final Combined/20170907_ACT_JuniorDayResultsCombined_SY2016-17_Whalen_v1.dta") %>% 
        left_join(read_excel("K:/Assessment_Data Returns/ACT/2016-17/Final Combined/TN Crosswalk - Spring 2017.xlsx", 
                             col_types = c(rep("text", 5))) %>% 
                      filter(str_detect(`ACT Organization Code`, "D") == F & !is.na(`ACT Organization Code`)) %>%
                      separate(`Local Site Code`, into = c("system", "school"), sep = " ") %>% 
                      transmute(acthscode = as.numeric(`ACT Organization Code`), system = as.numeric(system), 
                                school = as.numeric(school), school_name = `Organization Name`), 
                  by = "acthscode") %>% 
          mutate(system = ifelse(str_detect(school_name, "HILLCREST") == T, 985, system),
                 school = ifelse(str_detect(school_name, "HILLCREST") == T, 8140, school)) %>% 
          filter(system == 985 & !is.na(act_composite)) %>% 
          group_by(system, school) %>% 
          summarize(avg_composite = round(mean(act_composite, na.rm = T), 1)) %>% 
          ungroup()

    # Graduation
    grad = full_join(read_csv("K:/ORP_accountability/data/2016_graduation_rate/grad_rate_base_EK.csv") %>% 
                         filter(system == 985 & subgroup == "All Students" & school != 0) %>% 
                         select(system, school, grad_rate2016 = grad_rate),
                     read_csv("K:/ORP_accountability/data/2017_graduation_rate/grad_rate_base_EK.csv") %>% 
                         filter(system == 985 & subgroup == "All Students" & school != 0) %>% 
                         select(system, school, grad_rate2017 = grad_rate), 
                     by = c("system", "school"))
    
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
    ## Subject-Specific TVAAS
    ## Percentile Rank of SSR
    ## Gains in Percentage On Track/Mastered
    
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
    act_average = mission %>% 
        select(system, school, pool, subgroup, designation_ineligible, equity_eligible) %>% 
        full_join(act, by = c("system", "school")) %>% 
        mutate(pts_possible = ifelse(is.na(avg_composite), NA, 
                                           ifelse(school %in% c(45, 50), 
                                                  ifelse(equity_eligible == 1, 5, 10),
                                                         ifelse(equity_eligible == 1, 10, 15))),
               pts_earned = ifelse(avg_composite >= 16, pts_possible, 
                                   ifelse(avg_composite >= 15, 0.8 * pts_possible, 
                                          ifelse(avg_composite >= 14, 0.6 * pts_possible, 0.4 * pts_possible))),
               performance = ifelse(avg_composite >= 16, "Exceeds", 
                                    ifelse(avg_composite >= 15, "Meets", 
                                           ifelse(avg_composite >= 14, "Approaches", "Does Not Meet")))) %>% 
        mutate_each(funs(ifelse(designation_ineligible == 1, NA, .)), starts_with("pts"), performance) %>% 
    mutate(category = "College and Career - ACT Composite")

    ## Extended Graduation Rate - HS only
    grad_rates = mission %>% 
        select(system, school, pool, subgroup, designation_ineligible, equity_eligible) %>% 
        full_join(grad, by = c("system", "school")) %>% 
        mutate(pts_possible = ifelse(is.na(grad_rate2016) | is.na(grad_rate2017), NA, 
                                     ifelse(school %in% c(45, 50), 5,
                                            ifelse(equity_eligible == 1, 10, 15))),
               pts_earned = ifelse(grad_rate2017 - grad_rate2016 >= 8, pts_possible,
                                   ifelse(grad_rate2017 - grad_rate2016 >= 7 & grad_rate2017 - grad_rate2016 < 8, 0.8 * pts_possible, 
                                          ifelse(grad_rate2017 - grad_rate2016 >= 6 & grad_rate2017 - grad_rate2016 < 7, 0.6 * pts_possible,
                                                 0.4 * pts_possible))),
               performance = ifelse(grad_rate2017 - grad_rate2016 >= 8, "Exceeds",
                                    ifelse(grad_rate2017 - grad_rate2016 >= 7 & grad_rate2017 - grad_rate2016 < 8, "Meets", 
                                           ifelse(grad_rate2017 - grad_rate2016 >= 6 & grad_rate2017 - grad_rate2016 < 7, "Approaches",
                                                  "Does Not Meet")))) %>% 
        mutate_each(funs(ifelse(designation_ineligible == 1, NA, .)), starts_with("pts"), performance) %>% 
        mutate(category = "College and Career - Graduation Rate Gains")
    
    ## Overall College and Career
    bind_rows(act_average, grad_rates) %>% arrange(system, school, subgroup)
               
    # Equity
    ## Subgroup success rate percentile rank 
    equity_sr = filter(equity_eligible, equity_eligible == 1) %>% 
        select(system, school, contains("eligible_")) %>% 
        gather("subgroup", "equity_eligible", starts_with("equity")) %>% 
        mutate(subgroup = ifelse(str_detect(subgroup, "bhn") == T, "Black/Hispanic/Native American", subgroup),
               subgroup = ifelse(str_detect(subgroup, "ed") == T, "Economically Disadvantaged", subgroup),
               subgroup = ifelse(str_detect(subgroup, "el") == T, "English Learners with T1/T2", subgroup),
               subgroup = ifelse(str_detect(subgroup, "swd") == T, "Students with Disabilities", subgroup)) %>% 
        filter(equity_eligible == 1) %>% 
        left_join(school_summary %>% 
                      group_by(pool, subgroup) %>% 
                      transmute(system, school, success_rate_2017, success_rate_2017_pctile,
                                pctile_new = round(100 * percent_rank(success_rate_2017), 1)), 
                  by = c("system", "school", "subgroup")) %>% 
        filter(!is.na(pctile_new)) %>% 
        arrange(system, school, subgroup) %>% 
        group_by(system, school) %>% 
        mutate(pts_earned = ifelse(sum(pctile_new >= 6.5) / n() > .75, 10, 
                                   ifelse(sum(pctile_new >= 6.5) / n() > .5 & sum(pctile_new >= 6.5) / n() <= .75, 8, 
                                              ifelse(sum(pctile_new >= 6.5) / n() > .25 & sum(pctile_new >= 6.5) / n() <= .5, 6, 4)))) %>% 
        ungroup()
    
    ## Subgroup gains in percent on track/mastered
    equity_gains = filter(equity_eligible, equity_eligible == 1) %>% 
        select(system, school, contains("eligible_")) %>% 
        gather("subgroup", "equity_eligible", starts_with("equity")) %>% 
        mutate(subgroup = str_replace(subgroup, "equity_eligible_", ""),
               subgroup = ifelse(subgroup == "bhn", "Black/Hispanic/Native American", subgroup),
               subgroup = ifelse(subgroup == "ed", "Economically Disadvantaged", subgroup),
               subgroup = ifelse(subgroup == "el", "English Learners with T1/T2", subgroup),
               subgroup = ifelse(subgroup == "swd", "Students with Disabilities", subgroup),
               year = 2017) %>% 
        full_join(school_numeric %>% 
                      group_by(system, school, subgroup, year) %>% 
                      summarize_each(funs(sum(., na.rm = T)), valid_tests, n_on_track, n_mastered) %>% 
                      ungroup() %>% 
                      mutate(pct_on_mastered = round(100 * (n_on_track + n_mastered) / valid_tests, 1)), 
                  by = c("system", "school", "subgroup", "year")) %>% 
        arrange(system, school, subgroup, desc(year)) %>% 
        select(year, system, school, subgroup, pct_on_mastered) %>% 
        spread(year, pct_on_mastered) %>% 
        mutate(pts_possible = 10,
               pts_earned = ifelse(`2017` - `2016` >= 12, pts_possible, 
                                   ifelse(`2017` - `2016` < 12 & `2017` - `2016` >= 10, 0.8 * pts_possible, 
                                          ifelse(`2017` - `2016` < 10 & `2017` - `2016` >= 8, 0.6 * pts_possible, 0.4 * pts_possible)))) 
    
    ## Equity metrics
    equity_metrics = full_join(equity_sr, equity_gains, 
                               by = c("system", "school", "subgroup")) %>%
        filter(equity_eligible == 1) %>% 
        transmute(system, school, subgroup, pool, equity_eligible, success_rate_2017, pctile_new,
                  pct_on_mastered_2016 = `2016`, pct_on_mastered_2017 = `2017`,
                  pts_possible, pts_earned_sr = pts_earned.x, pts_earned_gains = pts_earned.y,
                  pts_earned_max = ifelse(is.na(pts_earned_gains), pts_earned_sr, 
                                          ifelse(pts_earned_sr > pts_earned_gains, pts_earned_sr, pts_earned_gains)),
                  category = "Equity") %>% 
        arrange(system, school, subgroup) %>% 
        group_by(system, school, subgroup) %>% 
        summarize_each(funs(first(.)), pool:category) %>% 
        ungroup()
    
    ## Overall Equity
    equity_overall = equity_metrics %>% 
        group_by(system, school) %>% 
        summarize_each(funs(first(.)), pool, starts_with("pts"), category) %>% 
        full_join(equity_eligible %>% 
                      filter(equity_eligible == 1) %>% 
                      select(system, school, equity_eligible), by = c("system", "school"))

                  
    # Alt Ed
    ## Credit Attainment
    ## Individual Learning Plans (% goals completed)
}

# K2
## Amy to take care of this