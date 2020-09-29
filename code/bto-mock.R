# NOTES
# - explore merTools
# - apply jared's recommendations

# load packages
library(tidyverse)
library(lme4)
source("R/functions.R")

# import data ----
load("data/faketucky.rda")

# temp fun to do all the data preparation ----
data_prep <- function() {
  # Select variables of interest ----
  my_vars <- c("first_dist_code", "first_dist_name", "first_hs_code", 
               "first_hs_name", "chrt_ninth", "male", "race_ethnicity", 
               "frpl_ever_in_hs", "sped_ever_in_hs", "lep_ever_in_hs", 
               "lep_ever_in_hs", "gifted_ever_in_hs", "scale_score_8_math",
               "scale_score_8_read", "scale_score_11_math", "scale_score_11_read")
  
  faketucky <- faketucky_20160923 %>% 
    select(all_of(my_vars))
  
  # Calculate school averages by cohort year ----
  sch_avg_by_cohort <- faketucky %>% 
    # Create indicator for students identifying as white
    mutate(race_white = ifelse(race_ethnicity == "White", 1, 0)) %>% 
    # Group data by high school and cohort year
    group_by(first_hs_code, chrt_ninth) %>% 
    # Calculate school averages
    mutate(sch_male = mean(male, na.rm = TRUE),
           sch_white = mean(race_white, na.rm = TRUE),
           sch_frpl = mean(frpl_ever_in_hs, na.rm = TRUE),
           sch_sped = mean(sped_ever_in_hs, na.rm = TRUE),
           sch_lep = mean(lep_ever_in_hs, na.rm = TRUE),
           sch_gifted = mean(gifted_ever_in_hs, na.rm = TRUE),
           sch_8_math = mean(scale_score_8_math, na.rm = TRUE),
           sch_8_read = mean(scale_score_8_read, na.rm = TRUE)) %>% 
    # Remember to ungroup the data frame
    ungroup()
  
  # Calculate across-year cohort averages
  cohort_avg <- sch_avg_by_cohort %>% 
    # Flag 2010 cohort
    mutate(flag_2010_cohort = ifelse(chrt_ninth == 2010, 1, 0)) %>% 
    # Group by cohort year
    group_by(chrt_ninth) %>%
    # Center prior achievement test scores
    mutate(math_8_center = scale_score_8_math - mean(scale_score_8_math, na.rm = TRUE),
           read_8_center = scale_score_8_read - mean(scale_score_8_read, na.rm = TRUE)) %>% 
    # Calculate averages
    mutate(sch_male_mean_year = mean(sch_male, na.rm = TRUE),
           sch_white_mean_year = mean(sch_white, na.rm = TRUE),
           sch_frpl_mean_year = mean(sch_frpl, na.rm = TRUE),
           sch_sped_mean_year = mean(sch_sped, na.rm = TRUE),
           sch_lep_mean_year = mean(sch_lep, na.rm = TRUE),
           sch_gifted_mean_year = mean(sch_gifted, na.rm = TRUE),
           sch_8_math_mean_year = mean(sch_8_math, na.rm = TRUE),
           sch_8_read_mean_year = mean(sch_8_read, na.rm = TRUE)) %>% 
    # Ungroup data frame
    ungroup()
  
  # Center school-level variables ----
  sch_avg_center <- cohort_avg %>% 
    # Subtract across-year averages from school averages
    mutate(sch_male_center = sch_male - sch_male_mean_year,
           sch_white_center = sch_white - sch_white_mean_year,
           sch_frpl_center = sch_frpl - sch_frpl_mean_year,
           sch_sped_center = sch_sped - sch_sped_mean_year,
           sch_lep_center = sch_lep - sch_lep_mean_year,
           sch_gifted_center = sch_gifted - sch_gifted_mean_year,
           sch_8_math_center = sch_8_math - sch_8_math_mean_year,
           sch_8_read_center = sch_8_read - sch_8_read_mean_year)
  
  return(sch_avg_center)
}

sch_avg_center <- data_prep()

# Fit multilevel models for each subject area ----  
m_math <- lmer(
  # Define model formula
  formula = scale_score_11_math ~ 
    male + race_white + frpl_ever_in_hs + sped_ever_in_hs + 
    lep_ever_in_hs + gifted_ever_in_hs + math_8_center + 
    sch_male_center + sch_white_center + sch_frpl_center +
    sch_sped_center + sch_lep_center + sch_gifted_center +
    sch_8_math_center + flag_2010_cohort + (1|first_hs_code),
  # Call dataframe containing the variables named in formula
  data = sch_avg_center, 
  # Option to use restricted maximum likelihood (REML) estimates
  REML = TRUE
)

m_read <- lmer(
  formula = scale_score_11_read ~ 
    male + race_white + frpl_ever_in_hs + sped_ever_in_hs + 
    lep_ever_in_hs + gifted_ever_in_hs + read_8_center + 
    sch_male_center + sch_white_center + sch_frpl_center +
    sch_sped_center + sch_lep_center + sch_gifted_center +
    sch_8_read_center + flag_2010_cohort + (1|first_hs_code),
  data = sch_avg_center, REML = TRUE
)

# START HERE ----
library(merTools)
plotREsim(REsim(m_math, n.sims = 100), stat = "median", sd = TRUE)

# notes
# - identify placement in guide -> step 3, following summary call
# - https://github.com/jknowles/merTools#marginalizing-random-effects


ranks <- expectedRank(m_math, groupFctr = "first_hs_code")
head(ranks)

ggplot(ranks, aes(pctER)) + geom_histogram(binwidth = 1)
ggplot(ranks, aes(reorder(groupLevel, pctER), pctER)) + geom_point()

# flag top/bottom 5% as over/under performing
new_ranks <- ranks %>% 
  mutate(bto_status = case_when(
    pctER >= 90 ~ "high-performing",
    pctER <= 10 ~ "under-performing"
  ))

new_ranks %>% count(bto_status)
