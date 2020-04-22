# file:     mock-analysis.R
# author:   Aaron Butler
# date:     14-Apr-2020
# purpose:  Mock analysis 

# NOTES
# - replace missing values?
# - include model checks

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

# access school-level residuals
resids_math <- ranef(m_math)
resids_read <- ranef(m_read)


# step 4: determine BTO ----

# create function to flag BTO schools
calc_bto <- function(.resids) {
  .resids %>%
    # store as a dataframe
    data.frame() %>% 
    # set benchmark at +2 standard deviations
    mutate(pos_bench = condsd * 1.96,
           neg_bench = condsd * -1.96,) %>% 
    # flag schools that perferm above/below benchmark (i.e., BTOs)
    mutate(sig = ifelse(condval >= pos_bench | condval <= neg_bench, "yes", "no"))
}

# execute function for each subject area
bto_math <- calc_bto(resids_math) %>% 
  # keep variables of interest
  # note: select and rename variables at the same time
  select(first_hs_code=grp, resid_math=condval, resid_sd_math=condsd, bto_math=sig)
  
bto_read <- calc_bto(resids_read) %>% 
  select(first_hs_code=grp, resid_read=condval, resid_sd_read=condsd, bto_read=sig)

# merge datsets for plotting

# merge bto datasets
bto_read_math <- left_join(bto_read, bto_math, by = "first_hs_code") %>% 
  # conver to numeric for merge
  mutate(first_hs_code = as.numeric(first_hs_code))

# pull school and district info from original dataset
df_names <- faketucky %>% 
  select(first_dist_code, first_hs_code, first_dist_name, first_hs_name) %>% 
  distinct()

# merge bto + school info datasets
df_bto <- left_join(df_names, bto_read_math, by = "first_hs_code")


# step 5: plot ----

# scatter plt math/read residuals
ggplot(df_bto, aes(resid_math, resid_read)) +
  geom_point(aes(color = bto_math))

# bar plot sm/me/lg change
df_bto %>% 
  ggplot(aes(resid_math)) +
  geom_histogram(aes(fill=bto_math))

summary(df_bto$resid_math)
summary(df_bto$resid_read)

df_cuts <- df_bto %>% 
  # set cut for math
  mutate(perf_math = case_when(
    # no diff
    bto_math == "no" ~ 0,
    # sig positive diff
    bto_math == "yes" & between(resid_math, 0, .5) ~ 1,
    bto_math == "yes" & between(resid_math, .5, 1) ~ 2,
    bto_math == "yes" & resid_math > 1 ~ 3,
    # sig negative diff
    bto_math == "yes" & between(resid_math, -.5, 0) ~ -1,
    bto_math == "yes" & between(resid_math, -1, -.5) ~ -2,
    bto_math == "yes" & resid_math < -1 ~ -3
  )) %>% 
  # set cut for reading
  mutate(perf_read = case_when(
    # no diff
    bto_read == "no" ~ 0,
    # sig positive diff
    bto_read == "yes" & between(resid_read, 0, .5) ~ 1,
    bto_read == "yes" & between(resid_read, .5, 1) ~ 2,
    bto_read == "yes" & resid_read > 1 ~ 3,
    # sig negative diff
    bto_read == "yes" & between(resid_read, -.5, 0) ~ -1,
    bto_read == "yes" & between(resid_read, -1, -.5) ~ -2,
    bto_read == "yes" & resid_read < -1 ~ -3
  ))

df_cuts %>% 
  count(perf_math, perf_read) %>% 
  pivot_longer(-n) %>% 
  ggplot(aes(value, n, fill=name)) +
  geom_col(position = "dodge")

# table sm/med/lg change
df_cuts %>% 
  count(perf_math, perf_read) %>%
  ggplot(aes(perf_math, perf_read)) +
  geom_tile(aes(fill = log(n))) +
  geom_text(aes(label = n)) +
  scale_fill_gradient(low = "#f8d14c", high = "#2c9fd1") 
  
# bar plot perf by stn groups (need to add sch char data)
df_sch_avg %>% 
  filter(chrt_ninth == 2010) %>% 
  glimpse

# mock math
# - make pretty
# - create fun and plot math and reading together
df_bto %>% 
  left_join(df_sch_avg %>% 
              filter(chrt_ninth == 2010), 
            by = "first_hs_code") %>% 
  drop_na(bto_math) %>% 
  group_by(bto_math) %>% 
  summarise(prop_white = mean(sch_white, na.rm = TRUE),
            prop_frpl = mean(sch_frpl, na.rm = TRUE),
            prop_sped = mean(sch_sped, na.rm = TRUE),
            prop_lep = mean(sch_lep, na.rm = TRUE)) %>% 
  pivot_longer(cols = starts_with("prop")) %>% 
  ggplot(aes(name, value, fill = bto_math)) +
  geom_col(position = "dodge")

