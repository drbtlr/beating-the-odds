# file:     butler-mock-analysis.R
# author:   Aaron Butler
# date:     14-Apr-2020
# purpose:  Mock analysis 

# NOTES
# - 
# - 

# load packages
library(tidyverse)
library(lme4)

# import data ----
load("data/faketucky.rda")

# select variables of interest
faketucky <- faketucky_20160923 %>% 
  select(-starts_with("enroll"), 
         -starts_with("ihe"))

# step 0: EDA -----

# outcomes: grade 11 math & reading
ggplot(faketucky, aes(scale_score_11_math)) +
  geom_histogram() +
  facet_wrap(~ chrt_ninth)

ggplot(faketucky, aes(scale_score_11_read)) +
  geom_histogram() +
  facet_wrap(~ chrt_ninth)

# covariates: ...
# male
# race_ethnicity -> convert to white
# frpl_ever_in_hs
# sped_ever_in_hs
# lep_ever_in_hs
# gifted_ever_in_hs
# scale_score_8_math
# scale_score_8_read

# step 1: create school averages within year ----
df_sch_avg <- faketucky %>% 
  # conver race_ethnicity to factor
  mutate(race_white = ifelse(race_ethnicity == "White", 1, 0)) %>% 
  # group by high school and cohort year
  group_by(first_hs_code, chrt_ninth) %>% 
  # calc school avgs
  mutate(sch_male = mean(male, na.rm = TRUE),
         sch_white = mean(race_white, na.rm = TRUE),
         sch_frpl = mean(frpl_ever_in_hs, na.rm = TRUE),
         sch_sped = mean(sped_ever_in_hs, na.rm = TRUE),
         sch_lep = mean(lep_ever_in_hs, na.rm = TRUE),
         sch_gifted = mean(gifted_ever_in_hs, na.rm = TRUE)) %>% 
  # remember to ungroup
  ungroup()

# step 2: center variables around grand mean within year ----
df_center <- df_sch_avg %>% 
  # flag 2010 cohort
  mutate(flag_2010_cohort = ifelse(chrt_ninth == 2010, 1, 0)) %>% 
  # group by cohort year
  group_by(chrt_ninth) %>% 
  # center student-level variables
  mutate(scale_score_8_math_center = scale_score_8_math - mean(scale_score_8_math, na.rm = TRUE),
         scale_score_8_read_center = scale_score_8_read - mean(scale_score_8_read, na.rm = TRUE)) %>% 
  # calc within year mean for school-level varialbes
  mutate(sch_male_mean_year = mean(sch_male, na.rm = TRUE),
         sch_white_mean_year = mean(sch_white, na.rm = TRUE),
         sch_frpl_mean_year = mean(sch_frpl, na.rm = TRUE),
         sch_sped_mean_year = mean(sch_sped, na.rm = TRUE),
         sch_lep_mean_year = mean(sch_lep, na.rm = TRUE),
         sch_gifted_mean_year = mean(sch_gifted, na.rm = TRUE)) %>% 
  # remember to ungroup
  ungroup() %>% 
  # center school-level variables
  mutate(sch_male_center = sch_male - sch_male_mean_year,
         sch_white_center = sch_white - sch_white_mean_year,
         sch_frpl_center = sch_frpl - sch_frpl_mean_year,
         sch_sped_center = sch_sped - sch_sped_mean_year,
         sch_lep_center = sch_lep - sch_lep_mean_year,
         sch_gifted_center = sch_gifted - sch_gifted_mean_year)

# step 3: model ----

# fit models
m_math <- lmer(
  scale_score_11_math ~ 
    male + race_white + frpl_ever_in_hs + sped_ever_in_hs + 
    lep_ever_in_hs + gifted_ever_in_hs + scale_score_8_math_center + 
    sch_male_center + sch_white_center + sch_frpl_center +
    sch_sped_center + sch_lep_center + sch_gifted_center +
    flag_2010_cohort + (1|first_hs_code),
  data = df_center, REML = TRUE
)

summary(m_math)

m_read <- lmer(
  scale_score_11_read ~ 
    male + race_white + frpl_ever_in_hs + sped_ever_in_hs + 
    lep_ever_in_hs + gifted_ever_in_hs + scale_score_8_read_center + 
    sch_male_center + sch_white_center + sch_frpl_center +
    sch_sped_center + sch_lep_center + sch_gifted_center +
    flag_2010_cohort + (1|first_hs_code),
  data = df_center, REML = TRUE
)

summary(m_read)

### START HERE ----------------------------------------------------------------------

# access level-2 residuals


# step 4: determine BTO ----

# step 5: plot ----


