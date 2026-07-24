 ## this script uses the same methodology as the final reg but filters
# the population to a number of different demographic cuts
# model 4 here is mostly for verifying that you get the same results as final_reg, 
# the actual demos are below

df_g <- df |> filter(year %in% c(2010:2025)) |> 
  mutate(rtw_status = case_when(
             statefips == "IN" ~ 2,
             statefips == "WI" ~ 2,
             statefips == "WV" ~ 2,
             statefips == "KY" ~ 2,
             statefips == "MI" ~ 2,
             #statefips == "CO" ~ 1,
             TRUE ~ rtw_status),
        metstat = case_when(
                    is.na(metstat) ~ 2,          
                    TRUE ~ metstat
        ))

#########################################################################
## MODEL 4 (demographic + indiv lmc + state lmc + BEA RPPs) ###
#########################################################################

treatment_vars <- "i(rtw_status, ref = '0') + urate + lnrpp"

fe_vars <- paste(c("year", "wbhao", "educ", "female", "metstat",  "pubsec", 
                   "married", "ft", "paidhre", "mind03", "mocc03", "age", "age2"),
                 collapse = " + ")

regression_formula <- as.formula(paste("lnwage ~", treatment_vars, "|", fe_vars))

### OUTPUT ####
model4g <- df_g |>
  filter(year %in% c(2023:2025)) |> 
  (\(d) feols(regression_formula, data = d, weights = ~ wgt, vcov = ~ statefips))()

model4g_results <- broom::tidy(model4g) |>
  filter(term %in% c("rtw_status::1", "rtw_status::2")) |>
  mutate(value = exp(estimate) - 1)


#########################################################################
## MODEL 4 by demographic subgroup ###
#########################################################################

demo_filters <- list(
  women           = quote(female == 1),
  men             = quote(female == 0),
  less_than_hs    = quote(educ == "Less than high school"),
  high_school     = quote(educ == "High school"),
  some_college    = quote(educ %in% c("Some college", "Associate's Degree")),
  college_or_more = quote(educ %in% c("College", "Advanced")),
  white_nonhis    = quote(wbhao == 1),
  black_nonhis    = quote(wbhao == 2),
  hispanic        = quote(wbhao == 3),
  union           = quote(union == 1),
  nonunion        = quote(union == 0)
)

run_demo_model <- function(filter_expr, label) {
  df_g |>
    filter(year %in% c(2023:2025)) |>
    filter(!!filter_expr) |>
    (\(d) feols(regression_formula, data = d, weights = ~ wgt, vcov = ~ statefips))() |>
    broom::tidy() |>
    filter(term %in% c("rtw_status::1", "rtw_status::2")) |>
    mutate(value = exp(estimate) - 1, demographic = label)
}

model4_demo_results <- imap(demo_filters, run_demo_model) |> list_rbind()


wb$
  # add new worksheet
  add_worksheet(sheet = "demo_regs")$
  # add data to worksheet
  add_data(x = "final reg model 4 by demo cuts")$
  add_data(x = model4_demo_results, start_row = 2)