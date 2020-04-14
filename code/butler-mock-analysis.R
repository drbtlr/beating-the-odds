# file:     butler-mock-analysis.R
# author:   Aaron Butler
# date:     14-Apr-2020
# purpose:  Mock analysis 

# NOTES
# - 
# - 

# load packages
library(tidyverse)

# import data ----
load("data/faketucky.rda")

# select variables of interest
faketucky <- faketucky_20160923 %>% 
  select(-starts_with("enroll"), 
         -starts_with("ihe"))

# step 1 ----

