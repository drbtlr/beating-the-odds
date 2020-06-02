---
title: "Beating the Odds"
author: "Aaron Butler & Hannah Poquette"
date: "June 2, 2020"
output: 
  html_document:
    theme: simplex
    css: ../includes/styles.css
    highlight: NULL
    keep_md: true
    toc: true
    toc_depth: 3
    toc_float: true
    number_sections: false
---

# Implementing a Beating-the-Odds Analysis 

*A step-by-step guide to implementing a beating-the-odds (BTO) analysis using a multilevel framework. Programmed in R.*



<div class="navbar navbar-default navbar-fixed-top" id="logo">
<div class="container">
<img src="https://opensdp.github.io/assets/images/OpenSDP-Banner_crimson.jpg" style="display: block; margin: 0 auto; height: 115px;">
</div>
</div>

## Getting Started

### Objective

In this guide, you will use statistical models to predict school performance based on the demographic makeup of schools' student populations and compare these predictions with actual school performance. 

### Purpose and Overview of Analyses

School leaders often want to identify promising practices that distinguish high-performing schools from their counterparts and facilitate the transfer of some of these practices to struggling schools. A beating-the-odds analysis is one approach school leaders can take to identify schools that perform better or worse than expected, given the unique student populations they serve.

The purpose of this guide is to present a data-driven approach to identify beating-the-odds schools. We use multilevel models to predict student performance and school-level effects on that performance. Next, we compare each school's predicted performance to its actual performance. The school is identified as beating the odds if its actual performance is higher or lower than predicted by a statistically significant margin. The procedures presented in this guide parallel those used in a collaborative study by the [Kentucky Department of Education](https://education.ky.gov/) and [REL Appalachia](https://ies.ed.gov/ncee/edlabs/regions/appalachia/).

### Using this Guide

This guide utilizes SDP's ["Faketucky"](https://github.com/opensdp/faketucky), a synthetic dataset based on real student data. While the data is synthetic, the code is not. Schools and districts wanting to conduct their own beating-the-odds analysis can easily adapt the code provided in this guide to their own data.

Once you have identified analyses that you want to replicate or modify, click the "Download" buttons to download R code and sample data. You can make changes to the charts using the code and sample data, or modify the code to work with your own data. If you are familiar with GitHub, you can click "Go to Repository" and clone the entire repository to your own computer. 

Go to the Participate page to read about more ways to engage with the OpenSDP community or reach out for assistance in adapting this code for your specific context.

### Installing and Loading R Packages

To complete this tutorial, you will need R, R Studio, and the following R packages installed on your machine: 

- `tidyverse`: For convenient data and output manipulation
- `lme4`: To fit multilevel models

To install packages, such as `lme4`, run the following command in the R console:

`install.packages("lme4")`

In addition, this guide will draw from OpenSDP-written functions defined in the `functions.R` document, which is located in the `R` folder of this guide's GitHub repository. Please make sure to have downloaded the entire GitHub repository to run this code.

After installing your R packages and downloaded this guide's GitHub repository, run the chunk of code below to load them onto your computer.


```r
# Load packages
library(tidyverse)
library(lme4)

# Read in some R functions that are convenience wrappers
source("../R/functions.R")
```

### About the Data

This guide uses SDP's ["Faketucky"](https://github.com/opensdp/faketucky) dataset. The Faketucky synthetic college-going analysis file contains high school and college outcome data for two graduating cohorts of approximately 40,000 students. There are no real children in the dataset, but it mirrors the relationships between variables present in real data. The dataset was developed as an offshoot of [SDP's College-Going Diagnostic for Kentucky](https://cepr.harvard.edu/publications/sdp-college-going-diagnostic-kentucky), using the R [synthpop](https://cran.r-project.org/web/packages/synthpop/index.html) package. In addition, we created school-level aggregates for each variable. The first step of the analysis "Prepare data for analysis" describes the steps to create the school-level variables.

Below is a list of variables and descriptions used in the analyses:

| Variable Name        | Variable Description                                                 |
|:-----------          |:------------------                                                   |
| `first_dist_code`    | Code of first district attended in high school                       |
| `first_dist_name`    | Name of first district attended in high school                       |
| `first_hs_code`      | Code of first high school attended                                   |
| `first_hs_name`      | Name of first high school attended                                   |
| `chrt_ninth`         | Student 9th grade cohort                                             |
| `male`               | Student male indicator                                               |
| `race_ethnicity`     | Student race/ethnicity                                               |
| `frpl_ever_in_hs`    | Student ever received free or reduced price lunch in high school     |
| `sped_ever_in_hs`    | Student ever classified as special ed in high school                 |
| `lep_ever_in_hs`     | Student ever classified as limited English proficiency in high school|
| `gifted_ever_in_hs`  | Student ever classified as gifted in high school                     |
| `scale_score_8_math` | Scaled score of 8th grade math test                                  |
| `scale_score_8_read` | Scaled score of 8th grade reading test                               |
| `scale_score_11_math`| Scaled score of highest math ACT                                     |
| `scale_score_11_read`| Scaled score of highest reading ACT                                  |

#### Loading the Dataset


```r
# Load "Faketucky" data file
load("../data/faketucky.rda")

# Select variables of interest
my_vars <- c("first_dist_code", "first_dist_name", "first_hs_code", 
             "first_hs_name", "chrt_ninth", "male", "race_ethnicity", 
             "frpl_ever_in_hs", "sped_ever_in_hs", "lep_ever_in_hs", 
             "lep_ever_in_hs", "gifted_ever_in_hs", "scale_score_8_math",
             "scale_score_8_read", "scale_score_11_math", "scale_score_11_read")

faketucky <- faketucky_20160923 %>% 
  select(my_vars)
```

### Giving Feedback on this Guide
 
This guide is an open-source document hosted on GitHub and generated using R Markdown. We welcome feedback, corrections, additions, and updates. Please visit the OpenSDP [participate repository](https://opensdp.github.io/participate/) to read our contributor guidelines.

## Analyses

### Identify BTO Schools

**Purpose:** This analysis illustrates how to identify schools that perform better or worse than expected, given the unique student populations they serve, using a beating-the-odds approach. 

**Required Analysis File Variables:**

- `first_hs_code`
- `chrt_ninth`
- `male`
- `race_ethnicity`
- `frpl_ever_in_hs`
- `sped_ever_in_hs`
- `lep_ever_in_hs`
- `gifted_ever_in_hs`
- `scale_score_8_math`
- `scale_score_8_read`
- `scale_score_11_math`
- `scale_score_11_read`

**Analytic Technique:** We use multilevel models to predict student performance and school-level effects on that performance. There are a number of benefits to using a multilevel approach over a more traditional approach like ordinary least squares. In particular, a multilevel approach allows us to account for the hierarchical or nested structure of the data. In this case, student observations and school observations from different years are nested within schools. Additionally, recent beating-the-odds studies have used a multilevel approach with success (e.g., [Bowers, 2015](https://www.tandfonline.com/doi/full/10.1080/13632434.2014.962500); [Partridg, Rudo, & Herrera, 2017](https://eric.ed.gov/?id=ED572602)). 

We encourage you check out Gelman and Hill's book [Data Analysis Using Regression and Multilevel/Hierarchical Models](http://www.stat.columbia.edu/~gelman/arm/) if you're interested in learning more about multilevel models and their applications.

**Ask Yourself:**

- What is the structure of my data? How might it influence the model?
- What variables do I want to include in the model? 
- What students will be included in or excluded from the model?
- What metrics will I use to determine appropriate model fit?

**A Note on Missing Data:** It is important to determine how you want to address missing data before you begin your analysis. For simplicity, we choose to exclude students with data missing from the analyses. We recommend that you conduct a missing data analysis to determine whether your data is missing completely at random, missing at random, or missing not at random and apply the appropriate strategy to address your missing data. Andrew Gelman's chapter on [Missing-data Imputation in R](http://www.stat.columbia.edu/~gelman/arm/missing.pdf) is a great resource to help you think about your options.   

#### Step 1: Prepare data for analysis

This beating-the-odds analysis uses a multilevel framework that incorporates school-level information into the modeling process. To prepare the analytical dataset, we calculated school averages within each school year. These variables where then centered using the grand mean of each variable. 

*Note:* We recognize that grand-mean centering may not be an appropriate option for your analysis. We encourage you to explore other centering techniques for multilevel modeling. Gelman and Hill's [Data Analysis Using Regression and Multilevel/Hierarchical Models](http://www.stat.columbia.edu/~gelman/arm/) and Raudenbush and Bryk's [Hierarchical Linear Models](https://us.sagepub.com/en-us/nam/hierarchical-linear-models/book9230) provide further details on the different centering techniques as well as the advantages and disadvantages of their use in multilevel modeling. 


```r
# // Step 1.1: Create school averages within year
df_sch_avg <- faketucky %>% 
  # Create indicator for students identifying as white
  mutate(race_white = ifelse(race_ethnicity == "White", 1, 0)) %>% 
  # Group data by high school and cohort year
  group_by(first_hs_code, chrt_ninth) %>% 
  # Calculate school averages within year
  mutate(sch_male = mean(male, na.rm = TRUE),
         sch_white = mean(race_white, na.rm = TRUE),
         sch_frpl = mean(frpl_ever_in_hs, na.rm = TRUE),
         sch_sped = mean(sped_ever_in_hs, na.rm = TRUE),
         sch_lep = mean(lep_ever_in_hs, na.rm = TRUE),
         sch_gifted = mean(gifted_ever_in_hs, na.rm = TRUE),
         sch_8_math = mean(scale_score_8_math, na.rm = TRUE),
         sch_8_read = mean(scale_score_8_read, na.rm = TRUE)) %>% 
  # Remember to ungroup
  ungroup()

# // Step 1.2: Center variables around grand mean within year
df_center <- df_sch_avg %>% 
  # Flag 2010 cohort
  mutate(flag_2010_cohort = ifelse(chrt_ninth == 2010, 1, 0)) %>% 
  # Group by cohort year
  group_by(chrt_ninth) %>% 
  # Center student-level variables
  mutate(
    scale_score_8_math_center = scale_score_8_math - mean(scale_score_8_math, 
                                                          na.rm = TRUE),
    scale_score_8_read_center = scale_score_8_read - mean(scale_score_8_read, 
                                                          na.rm = TRUE)) %>% 
  # Calculate within year mean for school-level variables
  mutate(sch_male_mean_year = mean(sch_male, na.rm = TRUE),
         sch_white_mean_year = mean(sch_white, na.rm = TRUE),
         sch_frpl_mean_year = mean(sch_frpl, na.rm = TRUE),
         sch_sped_mean_year = mean(sch_sped, na.rm = TRUE),
         sch_lep_mean_year = mean(sch_lep, na.rm = TRUE),
         sch_gifted_mean_year = mean(sch_gifted, na.rm = TRUE),
         sch_8_math_mean_year = mean(sch_8_math, na.rm = TRUE),
         sch_8_read_mean_year = mean(sch_8_read, na.rm = TRUE)) %>% 
  # Ungroup
  ungroup() %>% 
  # Center school-level variables
  mutate(sch_male_center = sch_male - sch_male_mean_year,
         sch_white_center = sch_white - sch_white_mean_year,
         sch_frpl_center = sch_frpl - sch_frpl_mean_year,
         sch_sped_center = sch_sped - sch_sped_mean_year,
         sch_lep_center = sch_lep - sch_lep_mean_year,
         sch_gifted_center = sch_gifted - sch_gifted_mean_year,
         sch_8_math_center = sch_8_math - sch_8_math_mean_year,
         sch_8_read_center = sch_8_read - sch_8_read_mean_year)
```

#### Step 2: Fit multilevel models

Multilevel models are a powerful and flexible extension to conventional regression frameworks. This is one of the many reasons why they are so attractive to education researchers. However, this added flexibility can make fitting and interpreting such models a challenge. Here, we present a relatively simple multilevel model that takes into account school-level variation. We encourage you to read more on the topic of multilevel modeling and its application in beating-the-odds analyses. 

We fit a two-level multilevel model for each subject area outcome of interest -- ACT math and reading -- using the `lmer` function in the `lme4` package. Specifically, these models use a random intercept framework, which allows the school-level intercept to vary randomly around a cross-school mean. We recommend you read the [lme4 Reference Manual](https://cran.r-project.org/web/packages/lme4/lme4.pdf) and vignette [Fitting Linear Mixed-Effects Models Using lme4](https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf) before you fit your models. These resources contain a wealth of information. 


```r
# // Step 2: Fit multilevel models for each subject area  
m_math <- lmer(
  # Define model formula
  formula = scale_score_11_math ~ 
    male + race_white + frpl_ever_in_hs + sped_ever_in_hs + 
    lep_ever_in_hs + gifted_ever_in_hs + scale_score_8_math_center + 
    sch_male_center + sch_white_center + sch_frpl_center +
    sch_sped_center + sch_lep_center + sch_gifted_center +
    sch_8_math_center + flag_2010_cohort + (1|first_hs_code),
  # Call dataframe containing the variables named in formula
  data = df_center, 
  # Option to use restricted maximum likelihood (REML) estimates
  REML = TRUE
)

m_read <- lmer(
  formula = scale_score_11_read ~ 
    male + race_white + frpl_ever_in_hs + sped_ever_in_hs + 
    lep_ever_in_hs + gifted_ever_in_hs + scale_score_8_read_center + 
    sch_male_center + sch_white_center + sch_frpl_center +
    sch_sped_center + sch_lep_center + sch_gifted_center +
    sch_8_read_center + flag_2010_cohort + (1|first_hs_code),
  data = df_center, REML = TRUE
)
```

#### Step 3: Inspect Summary Statistics for Model Fit

Examine the coefficients and standard errors for each variable. Ask yourself if the estimates are in the range of reasonable possiblity. If not, go back and inspect your dataset and make sure there are no errors in processing the data. Also inspect your dataset to make sure that the assumptions of multilevel modeling hold. 

For information on other model checking and sensitivity analysis for multilevel models, see Snijders and Berkhof's chapter on [Diagnostic Checks for Multilevel Models](https://www.stats.ox.ac.uk/~snijders/handbook_ml_ch3.pdf).


```r
# // Step 3: Call model summary statistics
summary(m_math)
```

```
Linear mixed model fit by REML ['lmerMod']
Formula: 
scale_score_11_math ~ male + race_white + frpl_ever_in_hs + sped_ever_in_hs +  
    lep_ever_in_hs + gifted_ever_in_hs + scale_score_8_math_center +  
    sch_male_center + sch_white_center + sch_frpl_center + sch_sped_center +  
    sch_lep_center + sch_gifted_center + sch_8_math_center +  
    flag_2010_cohort + (1 | first_hs_code)
   Data: df_center

REML criterion at convergence: 404785.3

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-5.8065 -0.6742 -0.1224  0.5558  6.5091 

Random effects:
 Groups        Name        Variance Std.Dev.
 first_hs_code (Intercept)  0.3799  0.6164  
 Residual                  11.6758  3.4170  
Number of obs: 76329, groups:  first_hs_code, 378

Fixed effects:
                            Estimate Std. Error t value
(Intercept)               18.9194811  0.0605949 312.229
male                      -0.0612752  0.0250516  -2.446
race_white                 0.0210941  0.0386175   0.546
frpl_ever_in_hs           -0.7038140  0.0286906 -24.531
sped_ever_in_hs           -0.7261136  0.0445451 -16.301
lep_ever_in_hs            -0.0587592  0.1395022  -0.421
gifted_ever_in_hs          1.7477650  0.0342171  51.079
scale_score_8_math_center  0.1200613  0.0007308 164.289
sch_male_center           -0.4505802  0.3362711  -1.340
sch_white_center          -0.6539464  0.2425249  -2.696
sch_frpl_center           -1.4640371  0.1938528  -7.552
sch_sped_center           -0.6070127  0.4397803  -1.380
sch_lep_center             1.5766275  1.1772262   1.339
sch_gifted_center         -0.2540672  0.3349658  -0.758
sch_8_math_center         -0.0225188  0.0051242  -4.395
flag_2010_cohort           0.3110568  0.0250484  12.418
```

```r
summary(m_read)
```

```
Linear mixed model fit by REML ['lmerMod']
Formula: 
scale_score_11_read ~ male + race_white + frpl_ever_in_hs + sped_ever_in_hs +  
    lep_ever_in_hs + gifted_ever_in_hs + scale_score_8_read_center +  
    sch_male_center + sch_white_center + sch_frpl_center + sch_sped_center +  
    sch_lep_center + sch_gifted_center + sch_8_read_center +  
    flag_2010_cohort + (1 | first_hs_code)
   Data: df_center

REML criterion at convergence: 460911.5

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-3.5857 -0.7081 -0.0759  0.6343  4.4410 

Random effects:
 Groups        Name        Variance Std.Dev.
 first_hs_code (Intercept)  0.338   0.5814  
 Residual                  24.312   4.9307  
Number of obs: 76383, groups:  first_hs_code, 384

Fixed effects:
                           Estimate Std. Error t value
(Intercept)               19.641018   0.075870 258.876
male                      -0.104838   0.036919  -2.840
race_white                 0.295630   0.055652   5.312
frpl_ever_in_hs           -1.193914   0.041178 -28.994
sped_ever_in_hs           -1.787166   0.063736 -28.040
lep_ever_in_hs            -0.011945   0.198216  -0.060
gifted_ever_in_hs          3.126626   0.047793  65.419
scale_score_8_read_center  0.125871   0.001286  97.908
sch_male_center           -0.681720   0.466453  -1.461
sch_white_center          -0.055665   0.270875  -0.206
sch_frpl_center           -1.783482   0.230264  -7.745
sch_sped_center           -0.135876   0.581416  -0.234
sch_lep_center             1.386339   1.290767   1.074
sch_gifted_center         -1.550264   0.429267  -3.611
sch_8_read_center          0.022338   0.009963   2.242
flag_2010_cohort           0.313737   0.036091   8.693
```

#### Step 4: Calculate statistics for beating-the-odds schools

We used the model residuals to identify three groups of schools:

* schools that were beating the odds and performed better than expected
* schools that performed the same as expected
* schools that performed worse than expected

This last group can also be helpful as the information can be used to provide targeted support.

We constructed a 95 percent confidence interval around each school's residual, using the
formula: 

```95 percent confidence interval = School residual ±1.96 × Residual standard error.```

Confidence intervals allowed us to determine whether a value of zero was just as plausible as the estimated residual. We identified schools as beating the odds if the 95 percent confidence interval did not include zero and if the residual was positive. Conversely, schools were identified as performing worse than expected if the 95 percent confidence interval did not include zero and if the residual was negative. Schools that performed as expected were those with confidence intervals that included zero.


```r
# // Step 4.1: Access school-level residuals
resids_math <- ranef(m_math)
resids_read <- ranef(m_read)

# // Step 4.2: Create function to flag BTO schools 
calc_bto <- function(.resids) {
  # Imput model residuals
  .resids %>%
    # Store as a dataframe
    data.frame() %>% 
    # Set benchmark at +2 standard deviations
    mutate(pos_bench = condsd * 1.96,
           neg_bench = condsd * -1.96) %>% 
    # Flag schools that perferm above/below benchmark (i.e., BTOs)
    mutate(sig = ifelse(condval >= pos_bench | 
                          condval <= neg_bench, 
                        "yes", "no"))
}

# // Step 4.3: Execute function for each subject area
bto_math <- calc_bto(resids_math) %>% 
  # Keep variables of interest. 
  # Note: we select and rename variables at the same time.
  select(first_hs_code = grp, resid_math = condval, bto_math = sig)
  
bto_read <- calc_bto(resids_read) %>% 
  select(first_hs_code = grp, resid_read = condval, bto_read = sig)

# // Step 4.4: Merge datsets for plotting

# Merge bto datasets
bto_read_math <- left_join(bto_read, bto_math, by = "first_hs_code") %>% 
  # Convert high school work to numeric for merge
  mutate(first_hs_code = as.numeric(first_hs_code))

# Pull school and district info from original dataset
df_names <- faketucky %>% 
  select(first_dist_code, first_hs_code, first_dist_name, first_hs_name) %>% 
  distinct()

# Merge bto and school info datasets
df_bto <- left_join(df_names, bto_read_math, by = "first_hs_code") 
```

#### Step 5: Plot beating-the-odds schools

It's always helpful to plot the results of your analysis. We've found a nice scatter plot can be an effective visualization for communicating the overall results of the analysis. 


```r
# // Step 5: Create scatter plot math/read residuals
df_bto %>% 
  mutate(bto_both = ifelse(bto_math == "yes" & bto_read == "yes",
                          "Performed Better/Worse than Expected", "Performed As Expected")) %>%
  filter(!is.na(bto_both)) %>%
  ggplot(aes(resid_math, resid_read, color = bto_both)) +
  geom_point(size = 2, alpha = .6) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#999999", "#E69F00")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.position = "top",
        legend.text = element_text(size = 11)) +
  labs(x = "Math (actual - predicted)",
       y = "Reading (actual - predicted)",
       color = "",
       title = "Beating-the-Odds Schools in Math and Reading",
       subtitle = "Schools in upper-right corner are performing better than expected\nin math and reading, whereas schools in lower-left corner or under\nperforming in both subject areas.")
```

<img src="../figure/E_plot-bto-1.png" style="display: block; margin: auto;" />

### BTO schools by performance level

**Purpose:** This analysis examines the distribution of beating-the-odds schools in math and reading and establishes performance levels (ex. small, medium, or large changes in between the predicted and actual school performance). Results provide an overall summary of a beating-the-odds analysis.  

**Required Analysis File Variables:**

- `resid_math`
- `resid_read`
- `bto_math`
- `bto_read`

**Ask Yourself**

- How many schools are performing better than expected given their student characteristics? How many are performing worse than expected?
- How large or small are the differences between schools' predicted performances and actual performances?
- How does school performance vary by subject area?

**Possible Next Steps or Action Plans:** Identify which schools are performing at different levels. Develop academic plan for schools at each performance level.

**Analytic Technique:** Identify school performance levels by subtracting each school's predicted achievement from its actual achievement and determining reasonable cut points for performance categories (such as small, medium, or large). Count the number of schools by performance level and plot a matrix to compare school performance across subject areas.  


```r
# // Step 1: Determine performance levels
# We examined the distribution of BTO schools as one example
# of how to determine performance levels. We recognize that there
# are a number of other ways to establish performance levels.
# Examples include referecing academic literature, technical
# documents, pilot studies, statistical analyses, etc.
df_bto %>% 
  mutate(bto_math = ifelse(bto_math == "yes", "Yes", "No")) %>%
  ggplot(aes(resid_math)) +
  geom_histogram(aes(fill = bto_math), alpha = .8) +
  scale_fill_manual(values = c("#999999", "#E69F00")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.position = "top",
        legend.text = element_text(size = 11)) +
  labs(x = "Math (actual - predicted)", y = "Number of Schools", fill = "",
       title = "Distribution of Beating-the-Odds Schools in Math") 
```

<img src="../figure/E_bto-cuts-plot-1.png" style="display: block; margin: auto;" />

```r
# // Step 2: Set performance levels
df_cuts <- df_bto %>% 
  # Set performance cuts for math
  mutate(perf_math = case_when(
    # No difference
    bto_math == "no" ~ 0,
    # Significant positive difference
    bto_math == "yes" & between(resid_math, 0, .5) ~ 1,
    bto_math == "yes" & between(resid_math, .5, 1) ~ 2,
    bto_math == "yes" & resid_math > 1 ~ 3,
    # Significant negative difference
    bto_math == "yes" & between(resid_math, -.5, 0) ~ -1,
    bto_math == "yes" & between(resid_math, -1, -.5) ~ -2,
    bto_math == "yes" & resid_math < -1 ~ -3
  )) %>% 
  # Set performance cuts for reading, repete previous steps
  mutate(perf_read = case_when(
    bto_read == "no" ~ 0,
    bto_read == "yes" & between(resid_read, 0, .5) ~ 1,
    bto_read == "yes" & between(resid_read, .5, 1) ~ 2,
    bto_read == "yes" & resid_read > 1 ~ 3,
    bto_read == "yes" & between(resid_read, -.5, 0) ~ -1,
    bto_read == "yes" & between(resid_read, -1, -.5) ~ -2,
    bto_read == "yes" & resid_read < -1 ~ -3
  ))

# // Step 2: Create temp datasets for plotting
df_math <- df_cuts %>% 
  count(perf_math) %>% 
  mutate(subject = "Math") %>% 
  select(subject, cut = perf_math, n)

df_read <- df_cuts %>% 
  count(perf_read) %>% 
  mutate(subject = "Reading") %>% 
  select(subject, cut = perf_read, n)

# // Step 3: Plot
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
  mutate(cut = fct_relevel(
    cut, 
    "Large\nnegative\ndecrease", "Medium\nnegative\ndecrease", 
    "Small\nnegative\ndecrease", "No\nsignificant\ndecrease", 
    "Small\npositive\nincrease", "Medium\npositive\nincrease", 
    "Large\npositive\nincrease")) %>% 
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
        legend.position = "top",
        legend.text = element_text(size = 11)) +
  labs(x = "", y = "Number of Schools", fill = "",
       title = "Distribution of Beating-the-Odds Schools in Math and Reading") 
```

<img src="../figure/E_bto-cuts-plot-2.png" style="display: block; margin: auto;" />

Creating tables or plots that compare school performance levels in math and reading allows us to see whether schools perform better or worse in one or two subject areas. In this example, we see that school performance in math and reading are correlated. This information can be used to inform the way we think about best practices.


```r
# Plot table
df_cuts %>%
  # Recode variable for plotting
  mutate(perf_math = case_when(
    perf_math == -3 ~ "Large\nnegative\ndecrease",
    perf_math == -2 ~ "Medium\nnegative\ndecrease",
    perf_math == -1 ~ "Small\nnegative\ndecrease",
    perf_math == 0 ~ "No\nsignificant\ndecrease",
    perf_math == 1 ~ "Small\npositive\nincrease",
    perf_math == 2 ~ "Medium\npositive\nincrease",
    perf_math == 3 ~ "Large\npositive\nincrease"
  )) %>% 
  mutate(perf_math = fct_relevel(
    perf_math, 
    "Large\nnegative\ndecrease", "Medium\nnegative\ndecrease", 
    "Small\nnegative\ndecrease", "No\nsignificant\ndecrease", 
    "Small\npositive\nincrease", "Medium\npositive\nincrease", 
    "Large\npositive\nincrease")) %>%
  mutate(perf_read = case_when(
    perf_read == -3 ~ "Large\nnegative\ndecrease",
    perf_read == -2 ~ "Medium\nnegative\ndecrease",
    perf_read == -1 ~ "Small\nnegative\ndecrease",
    perf_read == 0 ~ "No\nsignificant\ndecrease",
    perf_read == 1 ~ "Small\npositive\nincrease",
    perf_read == 2 ~ "Medium\npositive\nincrease",
    perf_read == 3 ~ "Large\npositive\nincrease"
  )) %>% 
  mutate(perf_read = fct_relevel(
    perf_read, 
    "Large\nnegative\ndecrease", "Medium\nnegative\ndecrease", 
    "Small\nnegative\ndecrease", "No\nsignificant\ndecrease", 
    "Small\npositive\nincrease", "Medium\npositive\nincrease", 
    "Large\npositive\nincrease")) %>%
  # Drop missing values for plotting
  drop_na(perf_math, perf_read) %>%
  # Count number of schools by performance levels
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
```

<img src="../figure/E_bto-cuts-table-1.png" style="display: block; margin: auto;" />

### BTO schools by prior performance

**Purpose:** This analysis compares prior achievement by BTO performance levels to better understand the progress made by schools in the different performance groups.  

**Required Analysis File Variables:**

- `first_hs_code`
- `chrt_ninth`
- `scale_score_8_math` 
- `scale_score_8_read`
- `resid_math`
- `resid_read`
- `bto_math`
- `bto_read`

**Ask Yourself**

- To what extent is prior achievement related to BTO performance levels? What can we infer from these findings?
- How does prior achievement and BTO performance levels differ by subject area? 

**Analytic Technique:** Calcualte the average prior achievement for each school in the BTO analyis then rank schools by their score.


```r
# // Step 1: Calculate average prior achievement for each school
df_avg_prior_ach <- faketucky %>%
  # Restrict to first cohort
  filter(chrt_ninth == "2009") %>%
  select(first_hs_code, scale_score_8_math, scale_score_8_read) %>%
  # Calculate average scores by subject
  group_by(first_hs_code) %>%
  summarise(avg_8_read = mean(scale_score_8_read, na.rm = TRUE),
            avg_8_math = mean(scale_score_8_math, na.rm = TRUE)) %>%
  # Rank schools by subject
  mutate(first_hs_code = as.numeric(first_hs_code),
         avg_8_read_prank = rank(avg_8_read)/length(avg_8_read)*100,
         avg_8_math_prank = rank(avg_8_math)/length(avg_8_math)*100)

# // Step 2: Create temp datasets for plotting
# As a workaround, create separate datasets for math and reading then
# append datasets for plotting.
tmp_ma <- left_join(df_cuts, df_avg_prior_ach) %>% 
  select(perf = perf_math, avg_prank = avg_8_math_prank) %>% 
  mutate(subject = "Math")

tmp_rd <- left_join(df_cuts, df_avg_prior_ach) %>% 
  select(perf = perf_read, avg_prank = avg_8_read_prank) %>% 
  mutate(subject = "Reading")

df_plot <- bind_rows(tmp_ma, tmp_rd) %>% drop_na()

# // Step 3: Plot
df_plot %>% 
  mutate(perf = case_when(
    perf == -3 ~ "Large\nnegative\ndecrease",
    perf == -2 ~ "Medium\nnegative\ndecrease",
    perf == -1 ~ "Small\nnegative\ndecrease",
    perf == 0 ~ "No\nsignificant\ndecrease",
    perf == 1 ~ "Small\npositive\nincrease",
    perf == 2 ~ "Medium\npositive\nincrease",
    perf == 3 ~ "Large\npositive\nincrease"
  )) %>% 
  mutate(perf = fct_relevel(
    perf, 
    "Large\nnegative\ndecrease", "Medium\nnegative\ndecrease", 
    "Small\nnegative\ndecrease", "No\nsignificant\ndecrease", 
    "Small\npositive\nincrease", "Medium\npositive\nincrease", 
    "Large\npositive\nincrease")) %>%
  ggplot(aes(factor(perf), avg_prank, fill=subject)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#E69F00", "#999999")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.position = "top") +
  labs(x = "Performance Level", y = "Percentile Rank", fill = "",
       title = "Prior Performance by Beating-the-Odds Performance Categories")
```

<img src="../figure/E_bto-prior-perf-plot-1.png" style="display: block; margin: auto;" />

### BTO schools by student demographics

**Purpose:** This analysis explores the student demographics of schools at different performance levels. 

**Required Analysis File Variables:**

- `chrt_ninth`
- `sch_white`
- `sch_frpl`
- `sch_sped`
- `sch_lep`
- `resid_math`
- `resid_read`

**Ask Yourself**

- Do student populations differ for high- and low-performing schools?
- How do high- and low-performing schools compare to all schools?

**Analytic Technique:** Calculate the percentage of white (or minority) students, low income students, and special education students for schools in each performance level. 


```r
# // Step 1: Create temp datasets for plotting
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

# // Step 2: Plot

# Math
bind_rows(grp_all, grp_math) %>% 
  # Recode variables for plotting
  mutate(group = fct_relevel(
    group, "All students", "High performing","Low performing", "Other"
    )) %>% 
  mutate(value = round(value * 100, 1)) %>% 
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
  expand_limits(y = c(0, 100)) +
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
```

<img src="../figure/E_bto-demos-1.png" style="display: block; margin: auto;" />

```r
# Reading
bind_rows(grp_all, grp_read) %>% 
  mutate(group = fct_relevel(
    group, "All students", "High performing","Low performing", "Other"
  )) %>% 
  mutate(value = round(value * 100, 1)) %>% 
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
  expand_limits(y = c(0, 100)) +
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
```

<img src="../figure/E_bto-demos-2.png" style="display: block; margin: auto;" />

---

##### *This guide was originally created by [Aaron Butler](https://www.aaronjbutler.com/) and Hannah Poquette in partnership with the Strategic Data Project.*
