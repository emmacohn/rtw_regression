## calculate average and median wage by RTW, non-RTW, switchers overtime

df_wage <- df |> 
    mutate(rtw_status = case_when(
             statefips == "IN" ~ 2,
             statefips == "WI" ~ 2,
             statefips == "WV" ~ 2,
             statefips == "KY" ~ 2,
             statefips == "MI" ~ 2,
             TRUE ~ rtw_status
  ),
 rtw_status = case_when(
              rtw_status == 0 ~ "non-RTW",
              rtw_status == 1 ~ "RTW",
              rtw_status == 2 ~ "switcher"
 ),
  rpp_wage = realwage25 * (100/rpp)
  )

## unadjusted, 2025 dollars
avg_wage <- df_wage |> 
  summarize(avg_wage = weighted.mean(realwage25, w = wgt),
           .by = c(rtw_status,year)) |> 
  pivot_wider(id_cols = year, names_from = rtw_status, values_from = avg_wage)

med_wage <- df_wage |> 
  summarize(med_wage = weighted_median(realwage25, w = wgt),
           .by = c(rtw_status,year)) |> 
  pivot_wider(id_cols = year, names_from = rtw_status, values_from = med_wage)

## using bea rpp to adjust 
avg_wage_rpp <- df_wage |> 
  summarize(avg_wage = weighted.mean(rpp_wage, w = wgt),
           .by = c(rtw_status,year)) |> 
  pivot_wider(id_cols = year, names_from = rtw_status, values_from = avg_wage)

med_wage_rpp <- df_wage |> 
  summarize(med_wage = weighted_median(rpp_wage, w = wgt),
           .by = c(rtw_status,year)) |> 
  pivot_wider(id_cols = year, names_from = rtw_status, values_from = med_wage)