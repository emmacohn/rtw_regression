# welcome to the Right to Work wage penalty regression project
# 2026 methodological update by Cohn & Gould
# updating Gould & Kimball 2015 and Gould & Shierholz 2011

library(realtalk)
library(epiextractr)
library(tidyverse)
library(epidatatools)
library(MetricsWeighted)
library(fixest)
library(labelled)
library(openxlsx2)

# create WB object
wb <- wb_workbook()

### PARAMETERS ####
# define variables
var_list <- c("year", "month", "selfemp", "selfinc", "age", "wage", "female", "wbhao",
              "wbho","educ", "gradeatn", "metstat", "emp", "statefips", "married", 
              "a_earnhour", "a_weekpay", "ftptstat", "union", "mocc03","mind03","paidhre", 
              "hoursu1i", "hoursuorg", "pubsec")

# read in RTW status by year, format for R analysis
rtw_status_year <- read.csv("./input/state_rtw_definitions.csv") |> 
  pivot_longer(cols = -year, names_to = "statefips", values_to = "rtw_status") |> 
  arrange(statefips, year)

#bea_rpp_3 replaces 2023 and 2024 data with an average of 2022-2024 data. it uses these averages for 2025 data. 
bea_rpp <- read.csv("./input/bea_rpp_3.csv") |>
  pivot_longer(cols = -statefips, names_to = "year", names_prefix = "X", values_to = "rpp") |>
  mutate(year = as.integer(year)) |>
  arrange(statefips, year)

# pulling in chained CPI-U series
cpi_data <- realtalk::c_cpi_u_annual
cpi2014 <- cpi_data$c_cpi_u[cpi_data$year == 2014]
cpi2025 <- cpi_data$c_cpi_u[cpi_data$year == 2025]

### DATA ####
### create state unemp rates
state_urate <- load_basic(2010:2025, year, age, unemp, statefips, lfstat, basicwgt) |>  
  filter(age >= 16, lfstat != 3)  |>  
  mutate(adj_wgt = case_match(
        year,
        2025 ~ basicwgt / 11,
        .default = basicwgt / 12),
      statefips = to_factor(statefips)) |>  
  summarise( 
    urate = weighted.mean(unemp, w = adj_wgt), 
    .by = c(year, statefips)) 


#### Standard restrictions ####
data <- load_org(2010:2025, all_of(c(var_list, "orgwgt"))) %>% 
  # Age and selfemp restrictions, remove imputed wages
  filter(
       selfemp == 0, age >= 16,
         # remove self-incorporated workers
         #note: not available year < 1989
         case_when(selfinc == 0 & !is.na(selfinc) ~ TRUE,
                   # keep any year that doesn't have selfinc (selfinc is NA)
                   is.na(selfinc) ~ TRUE, 
                   # exclude all other cases
                   TRUE ~ FALSE)) |> 
  filter_out(a_earnhour == 1 & paidhre == 1 | a_weekpay == 1 & paidhre == 0)

#### Master wage dataset ####
wage_master <- data %>%
  left_join(cpi_data, by = 'year') |>
  ## variable creation and correction
  mutate(lnwage = log(wage),
         age2 = age^2,
         statefips = as_factor(statefips),
         all = "all",
         age2 = age^2,
         educ = to_factor(educ),
         educ = case_when(
                gradeatn %in% c(11, 12) ~ "Associate's Degree",
                TRUE ~ educ),
         wgt = case_match(
                year,
                2025 ~ orgwgt / 11,
                .default = orgwgt / 12),
         realwage14 = wage * (cpi2014 /c_cpi_u),
         realwage25 = wage * (cpi2025 /c_cpi_u),
         ft = ifelse(hoursu1i >= 35 | hoursuorg >= 35 & ftptstat == 2, 1, 0)
        ) |> 
   left_join(rtw_status_year, by = c("year", "statefips")) |> 
   left_join(state_urate, by = c("year", "statefips")) |> 
   left_join(bea_rpp, by = c("year", "statefips")) |> 
  mutate(lnrpp = log(rpp))
  
#### Final regression df ####
#note: ungroup and drop invalid data
df <- ungroup(wage_master) %>% filter(wgt > 0, !is.na(lnwage), !is.na(year))

### FINAL NUMBERS ####
## original/benchmark regression
source("./code/01_old_reg.R")

## final regression
source("./code/02_final_reg.R")

## regressions by demographics
source("./code/03_demo_regs.R")

## table 1
source("./code/04_table1.R")

## other regressions -- NOT NECESSARILY METHODOLOGICALLY CONSISTENT WITH FINAL_REG ##
## omitting switcher states, 2012 data
source("./code/other/old_noswitch.R", echo = TRUE)
## 2012 rtw status, 2025 data
source("./code/other/new_2012.R", echo = TRUE)
## omitting switcher states, 2025 data
source("./code/other/new_noswitch.R", echo = TRUE)
## 2025 rtw status, no MI, new controls, 2025 data
source("./code/other/no_mi_2025.R", echo = TRUE)

## other code, you can probably ignore unless specified
## wages
source("./code/other/wage_series.R")
## policy (not using pop-weighted numbers)
source("./code/other/policy.R")

# save workbook to output folder
wb$
  save("./output/rtw_reg_final.xlsx", overwrite = TRUE)
