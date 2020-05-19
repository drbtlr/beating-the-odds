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

## missting data imputation ----

library(naniar)
library(simputation)

faketucky %>% 
  select(male, race_ethnicity, frpl_ever_in_hs,
         sped_ever_in_hs, lep_ever_in_hs, gifted_ever_in_hs,
         scale_score_8_math, scale_score_8_read,
         scale_score_11_math, scale_score_11_read) %>% 
  miss_var_summary()

x <- faketucky %>% 
  impute_lm(scale_score_8_math ~ male + race_ethnicity + frpl_ever_in_hs + sped_ever_in_hs) %>%
  impute_lm(scale_score_8_read ~ male + race_ethnicity + frpl_ever_in_hs + sped_ever_in_hs) %>%
  impute_lm(scale_score_11_math ~ male + race_ethnicity + frpl_ever_in_hs + sped_ever_in_hs) %>%
  impute_lm(scale_score_11_read ~ male + race_ethnicity + frpl_ever_in_hs + sped_ever_in_hs) 

x %>% 
  select(male, race_ethnicity, frpl_ever_in_hs,
         sped_ever_in_hs, lep_ever_in_hs, gifted_ever_in_hs,
         scale_score_8_math, scale_score_8_read,
         scale_score_11_math, scale_score_11_read) %>% 
  miss_var_summary()

## more eda

# outcome vars
x %>% 
  pivot_longer(cols = c("scale_score_11_math", "scale_score_11_read")) %>%
  ggplot(aes(value, fill = name)) +
  geom_histogram(binwidth = 1, alpha = .6) +
  facet_grid(~chrt_ninth) +
  theme_minimal() +
  theme(legend.position = "bottom")

# school averages
library(gt)

df_avg <- x %>% 
  mutate(race_white = ifelse(race_ethnicity == "White", 1, 0)) %>% 
  group_by(chrt_ninth) %>%
  summarise_at(c("male", "race_white", "frpl_ever_in_hs", "sped_ever_in_hs", 
                 "lep_ever_in_hs", "scale_score_8_math", "scale_score_8_read"),
               mean, na.rm = TRUE) %>% 
  ungroup() %>% 
  pivot_longer(-chrt_ninth,
               names_to = "vars",
               values_to = "avg") %>%
  pivot_wider(names_from = chrt_ninth,
              names_prefix = "avg_",
              values_from = avg)
  
df_sd <- x %>% 
  mutate(race_white = ifelse(race_ethnicity == "White", 1, 0)) %>% 
  group_by(chrt_ninth) %>%
  summarise_at(c("male", "race_white", "frpl_ever_in_hs", "sped_ever_in_hs", 
                 "lep_ever_in_hs", "scale_score_8_math", "scale_score_8_read"),
               sd, na.rm = TRUE) %>% 
  ungroup() %>% 
  pivot_longer(-chrt_ninth,
               names_to = "vars",
               values_to = "sd") %>% 
  pivot_wider(names_from = chrt_ninth,
              names_prefix = "sd_",
              values_from = sd)

gt(data=left_join(df_avg, df_sd)) %>% 
  tab_spanner(label = "2009",
              columns = vars(avg_2009, sd_2009)) %>% 
  tab_spanner(label = "2010",
              columns = vars(avg_2010, sd_2010)) %>% 
  cols_label(vars="School-Level\nCharacteristic", 
             avg_2009="Mean", sd_2009="SD",
             avg_2010="Mean", sd_2010="SD")

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
         sch_gifted = mean(gifted_ever_in_hs, na.rm = TRUE),
         sch_8_math = mean(scale_score_8_math, na.rm = TRUE),
         sch_8_read = mean(scale_score_8_read, na.rm = TRUE)) %>% 
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
         sch_gifted_mean_year = mean(sch_gifted, na.rm = TRUE),
         sch_8_math_mean_year = mean(sch_8_math, na.rm = TRUE),
         sch_8_read_mean_year = mean(sch_8_read, na.rm = TRUE)) %>% 
  # remember to ungroup
  ungroup() %>% 
  # center school-level variables
  mutate(sch_male_center = sch_male - sch_male_mean_year,
         sch_white_center = sch_white - sch_white_mean_year,
         sch_frpl_center = sch_frpl - sch_frpl_mean_year,
         sch_sped_center = sch_sped - sch_sped_mean_year,
         sch_lep_center = sch_lep - sch_lep_mean_year,
         sch_gifted_center = sch_gifted - sch_gifted_mean_year,
         sch_8_math_center = sch_8_math - sch_8_math_mean_year,
         sch_8_read_center = sch_8_read - sch_8_read_mean_year)

# step 3: model ----

# fit models
m_math <- lmer(
  scale_score_11_math ~ 
    male + race_white + frpl_ever_in_hs + sped_ever_in_hs + 
    lep_ever_in_hs + gifted_ever_in_hs + scale_score_8_math_center + 
    sch_male_center + sch_white_center + sch_frpl_center +
    sch_sped_center + sch_lep_center + sch_gifted_center +
    sch_8_math_center + flag_2010_cohort + (1|first_hs_code),
  data = df_center, REML = TRUE
)

summary(m_math)

m_read <- lmer(
  scale_score_11_read ~ 
    male + race_white + frpl_ever_in_hs + sped_ever_in_hs + 
    lep_ever_in_hs + gifted_ever_in_hs + scale_score_8_read_center + 
    sch_male_center + sch_white_center + sch_frpl_center +
    sch_sped_center + sch_lep_center + sch_gifted_center +
    sch_8_read_center + flag_2010_cohort + (1|first_hs_code),
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
df_bto %>% 
  mutate(bto_both = ifelse(bto_math == "yes" & bto_read == "yes", "BTO School", "Not a BTO School")) %>% 
  filter(!is.na(bto_both)) %>% 
  ggplot(aes(resid_math, resid_read, color = bto_both)) +
  geom_point(size = 2, alpha = .6) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#E69F00", "#999999")) +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.position = "bottom",
        legend.text = element_text(size = 11)) +
  labs(x = "Math",
       y = "Reading",
       color = "",
       title = "BTO Schools in Math and Reading",
       subtitle = "Two-year status model (actual - predicted), 2010")

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

df_cuts %>% count(bto_math) %>% mutate(pct = n/sum(n))
df_cuts %>% count(perf_math) %>% mutate(pct = n/sum(n))

# plot sm/med/lg change
df_math <- df_cuts %>% 
  count(perf_math) %>% 
  mutate(subject = "Math") %>% 
  select(subject, cut = perf_math, n)

df_read <- df_cuts %>% 
  count(perf_read) %>% 
  mutate(subject = "Reading") %>% 
  select(subject, cut = perf_read, n)

bind_rows(df_math, df_read) %>% 
  mutate(cut = case_when(
    cut == -3 ~ "Large\nnegative\ndecrease",
    cut == -2 ~ "Medium\nnegative\ndecrease",
    cut == -1 ~ "Small\nnegative\ndecrease",
    cut == 0 ~ "No\nsignificant\ndecrease",
    cut == 1 ~ "Small\npositive\nincrease",
    cut == 2 ~ "Medium\npositive\nincrease",
    cut == 3 ~ "Large\npositive\nincrease"
  )) %>% 
  mutate(cut = fct_relevel(cut, "Large\nnegative\ndecrease", 
                           "Medium\nnegative\ndecrease", "Small\nnegative\ndecrease", 
                           "No\nsignificant\ndecrease", "Small\npositive\nincrease", 
                           "Medium\npositive\nincrease", "Large\npositive\nincrease")) %>% 
  filter(!is.na(cut)) %>% 
  ggplot(aes(cut, n, fill = subject)) +
  geom_col(position = position_dodge(.85), width = .8) +
  geom_text(aes(label = n),
            position = position_dodge(.85), vjust = -.45) +
  scale_fill_manual(values = c("#E69F00", "#999999")) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 11),
        legend.position = "bottom",
        legend.text = element_text(size = 11)) +
  labs(x = "", y = "Number of School", fill = "",
       title = "Distribution of BTO Schools in Math and Reading")

# table sm/med/lg change
df_cuts %>% 
  count(perf_math, perf_read) %>%
  ggplot(aes(perf_math, perf_read)) +
  geom_tile(aes(fill = log(n))) +
  geom_text(aes(label = n)) +
  scale_fill_gradient(low = "#f8d14c", high = "#2c9fd1") 

df_cuts %>%
  mutate(perf_math = case_when(
    perf_math == -3 ~ "Large\nnegative\ndecrease",
    perf_math == -2 ~ "Medium\nnegative\ndecrease",
    perf_math == -1 ~ "Small\nnegative\ndecrease",
    perf_math == 0 ~ "No\nsignificant\ndecrease",
    perf_math == 1 ~ "Small\npositive\nincrease",
    perf_math == 2 ~ "Medium\npositive\nincrease",
    perf_math == 3 ~ "Large\npositive\nincrease"
  )) %>% 
  mutate(perf_math = fct_relevel(perf_math, "Large\nnegative\ndecrease", 
                                 "Medium\nnegative\ndecrease", "Small\nnegative\ndecrease", 
                                 "No\nsignificant\ndecrease", "Small\npositive\nincrease", 
                                 "Medium\npositive\nincrease", "Large\npositive\nincrease")) %>%
  mutate(perf_read = case_when(
    perf_read == -3 ~ "Large\nnegative\ndecrease",
    perf_read == -2 ~ "Medium\nnegative\ndecrease",
    perf_read == -1 ~ "Small\nnegative\ndecrease",
    perf_read == 0 ~ "No\nsignificant\ndecrease",
    perf_read == 1 ~ "Small\npositive\nincrease",
    perf_read == 2 ~ "Medium\npositive\nincrease",
    perf_read == 3 ~ "Large\npositive\nincrease"
  )) %>% 
  mutate(perf_read = fct_relevel(perf_read, "Large\nnegative\ndecrease", 
                                 "Medium\nnegative\ndecrease", "Small\nnegative\ndecrease", 
                                 "No\nsignificant\ndecrease", "Small\npositive\nincrease", 
                                 "Medium\npositive\nincrease", "Large\npositive\nincrease")) %>%
  drop_na(perf_math, perf_read) %>%
  count(perf_math, perf_read) %>%
  ggplot(aes(perf_math, perf_read)) +
  geom_tile(fill = "white") +
  geom_text(aes(label = n)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11)) +
  labs(x = "Math Performance", y = "Reading Performance",
       title = "School Counts by Performance Categories for Math and Reading")

# bar plot perf by stn groups (need to add sch char data)
grp_all <- df_sch_avg %>% 
  filter(chrt_ninth == 2010) %>% 
  summarise(prop_white = mean(sch_white, na.rm = TRUE),
            prop_frpl = mean(sch_frpl, na.rm = TRUE),
            prop_sped = mean(sch_sped, na.rm = TRUE),
            prop_lep = mean(sch_lep, na.rm = TRUE)) %>% 
  mutate(group = "All students") %>% 
  pivot_longer(-group)

grp_math <- df_cuts %>%
  mutate(perf_math_high_low = case_when(
    perf_math > 0 ~ "High performing",
    perf_math < 0 ~ "Low performing",
    perf_math == 0 ~ "Other"
  )) %>%
  left_join(df_sch_avg %>% filter(chrt_ninth == 2010)) %>% 
  drop_na(perf_math_high_low) %>% 
  group_by(perf_math_high_low) %>% 
  summarise(prop_white = mean(sch_white, na.rm = TRUE),
            prop_frpl = mean(sch_frpl, na.rm = TRUE),
            prop_sped = mean(sch_sped, na.rm = TRUE),
            prop_lep = mean(sch_lep, na.rm = TRUE)) %>% 
  pivot_longer(cols = starts_with("prop")) %>% 
  rename(group = perf_math_high_low)

grp_read <- df_cuts %>%
  mutate(perf_read_high_low = case_when(
    perf_read > 0 ~ "High performing",
    perf_read < 0 ~ "Low performing",
    perf_read == 0 ~ "Other"
  )) %>%
  left_join(df_sch_avg %>% filter(chrt_ninth == 2010)) %>% 
  drop_na(perf_read_high_low) %>% 
  group_by(perf_read_high_low) %>% 
  summarise(prop_white = mean(sch_white, na.rm = TRUE),
            prop_frpl = mean(sch_frpl, na.rm = TRUE),
            prop_sped = mean(sch_sped, na.rm = TRUE),
            prop_lep = mean(sch_lep, na.rm = TRUE)) %>% 
  pivot_longer(cols = starts_with("prop")) %>% 
  rename(group = perf_read_high_low) 

# plot math
bind_rows(grp_all, grp_math) %>% 
  mutate(group = fct_relevel(
    group, "All students", "High performing","Low performing", "Other"
    )) %>% 
  mutate(value = round(value * 100), 1) %>% 
  mutate(name = case_when(
    name == "prop_white" ~ "White",
    name == "prop_frpl" ~ "Low Income",
    name == "prop_sped" ~ "Special Education",
    name == "prop_lep" ~ "Limited English"
  )) %>% 
  ggplot(aes(name, value, fill = group)) +
  geom_col(position = position_dodge(.85), width = .8) +
  geom_text(aes(label = round(value, 1)),
            position = position_dodge(.85), vjust = -.45) +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73")) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.position = "top",
        legend.text = element_text(size = 11)) +
  labs(x = "", y = "Percent of Students", fill = "",
       title = "School Demographics by Performance Status in Math")

# plot reading
bind_rows(grp_all, grp_read) %>% 
  mutate(group = fct_relevel(
    group, "All students", "High performing","Low performing", "Other"
  )) %>% 
  mutate(value = round(value * 100), 1) %>% 
  mutate(name = case_when(
    name == "prop_white" ~ "White",
    name == "prop_frpl" ~ "Low Income",
    name == "prop_sped" ~ "Special Education",
    name == "prop_lep" ~ "Limited English"
  )) %>% 
  ggplot(aes(name, value, fill = group)) +
  geom_col(position = position_dodge(.85), width = .8) +
  geom_text(aes(label = round(value, 1)),
            position = position_dodge(.85), vjust = -.45) +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73")) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.position = "top",
        legend.text = element_text(size = 11)) +
  labs(x = "", y = "Percent of Students", fill = "",
       title = "School Demographics by Performance Status in Reading")


# step 6: model comparision (optional) -----

# COMPARE 1: calc BTO using OLS

# fit models
ols_math <- lm(
  scale_score_11_math ~ 
    male + race_white + frpl_ever_in_hs + sped_ever_in_hs + 
    lep_ever_in_hs + gifted_ever_in_hs + scale_score_8_math_center + 
    sch_male_center + sch_white_center + sch_frpl_center +
    sch_sped_center + sch_lep_center + sch_gifted_center +
    sch_8_math_center + flag_2010_cohort + first_hs_code,
  data = df_center
)

summary(ols_math)

ols_read <- lm(
  scale_score_11_read ~ 
    male + race_white + frpl_ever_in_hs + sped_ever_in_hs + 
    lep_ever_in_hs + gifted_ever_in_hs + scale_score_8_read_center + 
    sch_male_center + sch_white_center + sch_frpl_center +
    sch_sped_center + sch_lep_center + sch_gifted_center +
    sch_8_read_center + flag_2010_cohort + first_hs_code,
  data = df_center
)

summary(ols_read)

# access school-level residuals
ols_resids_math <- broom::augment(ols_math)
ols_resids_read <- broom::augment(ols_read)

ols_bto_math <- ols_resids_math %>% 
  group_by(first_hs_code) %>% 
  summarise(.resid = mean(.resid)) %>% 
  mutate(pos_bench = .resid * 1.96,
         neg_bench = .resid * -1.96,) %>% 
  mutate(sig = ifelse(.resid >= pos_bench | .resid <= neg_bench, "yes", "no")) %>% 
  select(first_hs_code, resid_math=.resid, bto_math=sig)

ols_bto_read <- ols_resids_read %>% 
  group_by(first_hs_code) %>% 
  summarise(.resid = mean(.resid)) %>% 
  mutate(pos_bench = .resid * 1.96,
         neg_bench = .resid * -1.96,) %>% 
  mutate(sig = ifelse(.resid >= pos_bench | .resid <= neg_bench, "yes", "no")) %>% 
  select(first_hs_code, resid_read=.resid, bto_read=sig)

# merge bto datasets
ols_bto_read_math <- left_join(ols_bto_read, ols_bto_math, by = "first_hs_code") %>% 
  # conver to numeric for merge
  mutate(first_hs_code = as.numeric(first_hs_code))

# pull school and district info from original dataset
df_names <- faketucky %>% 
  select(first_dist_code, first_hs_code, first_dist_name, first_hs_name) %>% 
  distinct()

# merge bto + school info datasets
df_ols_bto <- left_join(df_names, ols_bto_read_math, by = "first_hs_code") %>% 
  rename(ols_bto_read = bto_read, ols_bto_math = bto_math)

# compare OLS model to HLM model
df_compare <- left_join(df_bto %>% select(first_hs_code, bto_read, bto_math),
                        df_ols_bto %>% select(first_hs_code, ols_bto_math, ols_bto_read))

# calc agreement rate
# NOTE: using BTO +/- -> consider split analysis

df_compare %>% 
  mutate(same_math = ifelse(bto_math == "yes" & ols_bto_math == "yes", "yes", "no"),
         same_read = ifelse(bto_read == "yes" & ols_bto_read == "yes", "yes", "no")) %>% 
  pivot_longer(-first_hs_code) %>% 
  group_by(name) %>% 
  count(value) %>% 
  filter(value == "yes") %>% 
  select(-value) %>% 
  pivot_wider(names_from = name, 
              values_from = n) %>% 
  mutate(agree_math = same_math/(bto_math + ols_bto_math / 2),
         agree_read = same_read/(bto_read + ols_bto_read / 2)) # low!

# COMPARE 2: calc HLM for 2009 and 2010

# fit models
math_2009 <- lmer(
  scale_score_11_math ~ 
    male + race_white + frpl_ever_in_hs + sped_ever_in_hs + 
    lep_ever_in_hs + gifted_ever_in_hs + scale_score_8_math_center + 
    sch_male_center + sch_white_center + sch_frpl_center +
    sch_sped_center + sch_lep_center + sch_gifted_center +
    sch_8_math_center + (1|first_hs_code),
  data = df_center %>% filter(flag_2010_cohort==0), REML = TRUE
)

summary(math_2009)

math_2010 <- lmer(
  scale_score_11_math ~ 
    male + race_white + frpl_ever_in_hs + sped_ever_in_hs + 
    lep_ever_in_hs + gifted_ever_in_hs + scale_score_8_math_center + 
    sch_male_center + sch_white_center + sch_frpl_center +
    sch_sped_center + sch_lep_center + sch_gifted_center +
    sch_8_math_center + (1|first_hs_code),
  data = df_center %>% filter(flag_2010_cohort==1), REML = TRUE
)

summary(math_2010)

# access school-level residuals
resids_math_2009 <- ranef(math_2009)
resids_math_2010 <- ranef(math_2010)

# execute function for each subject area
bto_math_2009 <- calc_bto(resids_math_2009) %>% 
  select(first_hs_code=grp, resid_math_2009=condval, bto_math_2009=sig)

bto_math_2010 <- calc_bto(resids_math_2010) %>% 
  select(first_hs_code=grp, resid_math_2010=condval, bto_math_2010=sig)

# merge bto datasets
bto_math_0910 <- left_join(bto_math_2009, bto_math_2010, by = "first_hs_code") %>% 
  # conver to numeric for merge
  mutate(first_hs_code = as.numeric(first_hs_code))

# pull school and district info from original dataset
df_names <- faketucky %>% 
  select(first_dist_code, first_hs_code, first_dist_name, first_hs_name) %>% 
  distinct()

# merge bto + school info datasets
df_bto_math_0910 <- left_join(df_names, bto_math_0910, by = "first_hs_code") %>% 
  select(first_hs_code, contains("bto"))

# calc agreement rate
df_bto_math_0910 %>% 
  mutate(same_math = ifelse(bto_math_2009 == "yes" & bto_math_2010 == "yes", "yes", "no")) %>% 
  pivot_longer(-first_hs_code) %>% 
  group_by(name) %>% 
  count(value) %>% 
  filter(value == "yes") %>% 
  select(-value) %>% 
  pivot_wider(names_from = name, 
              values_from = n) %>% 
  mutate(agree_math = same_math/(bto_math_2009 + bto_math_2010 / 2)) # again, low!
