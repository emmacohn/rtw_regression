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
# define wage years
var_list <- c("year", "month", "selfemp", "selfinc", "age", "wage", "female", "wbhao","wbho","educ", "gradeatn", "metstat", "emp",
                "statefips", "married", "a_earnhour", "a_weekpay", "ftptstat", "union", "mocc03","mind03","paidhre", "hoursu1i", "hoursuorg", "pubsec")

rtw_status_year <- read.csv("./input/state_rtw_definitions.csv") |> 
  pivot_longer(cols = -year, names_to = "statefips", values_to = "rtw_status") |> 
  arrange(statefips, year) |> 
  #gould and kimball keep indiana as a non-rtw state
  mutate(rtw_status = case_when(
    statefips == "IN" & year == 2012 ~ 0,
    TRUE ~ rtw_status
  ))

#bea_rpp_3 replaces 2023 and 2024 data with an average of 2022-2024 data. it uses these averages for 2025 data. 
bea_rpp <- read.csv("./input/bea_rpp_3.csv") |>
  pivot_longer(cols = -statefips, names_to = "year", names_prefix = "X", values_to = "rpp") |>
  mutate(year = as.integer(year)) |>
  arrange(statefips, year)

# pulling in chained CPI-U series
cpi_data <- realtalk::c_cpi_u_annual
cpi2014 <- cpi_data$c_cpi_u[cpi_data$year == 2014]
cpi2024 <- cpi_data$c_cpi_u[cpi_data$year == 2024]
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
         realwage24 = wage * (cpi2024 /c_cpi_u),
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

### REGRESSIONS ####
## table 1
source("./code/table1.R", echo = TRUE)

### table 2 - 2012 data ###
## original/benchmark
source("./code/a_old_reg.R", echo = TRUE)
## omitting switcher states
source("./code/b_old_noswitch.R", echo = TRUE)

### table 2 - 2025 data ###
## 2012 rtw designations
source("./code/c_new_2012.R", echo = TRUE)
## omitting switcher states
source("./code/d_new_noswitch.R", echo = TRUE)
## 2025 rtw designations
source("./code/e_new_2025.R", echo = TRUE)


wb$
  # save workbook to output folder
  save("./output/rtw_reg3.xlsx", overwrite = TRUE)
