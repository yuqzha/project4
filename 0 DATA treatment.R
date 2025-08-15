## trt data sources rcc & nkbc
## 
rcc_trt <- rcc |> 
  rename(diadat = vdiadat) |> 
  mutate(post_endo = ifelse(is.na(postopbeh) | postopbeh == 99, NA,
                            ifelse(postopbeh %in% c(30, 13, 15, 23, 34), 1, 0)),
         post_radio = ifelse(is.na(postopbeh) | postopbeh == 99, NA,
                             ifelse(postopbeh %in% c(10, 12, 13, 14, 15), 1, 0)),
         post_chemo = ifelse(is.na(postopbeh) | postopbeh == 99, NA,
                             ifelse(postopbeh %in% c(20, 12, 15, 23, 24, 25), 1, 0)),
         source = "rcc") |> 
  select(id, diadat, post_endo, post_radio, post_chemo, source) 

nkbc_trt <- nkbc |> 
  rename(post_endo = post_endo_varde,
         post_radio = post_rt_varde,
         post_chemo = post_kemo_varde) |> 
  mutate(diadat = as_date(a_diag_dat, origin = "1970-01-01"),
         source = "nkbc") |> 
  select(id, diadat, post_endo, post_radio, post_chemo, source)

trt <- bind_rows(rcc_trt, nkbc_trt)

## choose first non-missing value for each person-date combination
trt <- trt %>%
  arrange(id, diadat) %>%  # Sort to ensure consistent ordering
  group_by(id, diadat) %>%
  summarize(
    # Find first non-NA value for each column
    post_endo = na.omit(post_endo)[1],
    post_radio = na.omit(post_radio)[1],
    post_chemo = na.omit(post_chemo)[1],
    .groups = "drop"
  )


## join trt to cohort to get diadat
cohort_trt <- cohort[, c("id", "diadat")] |> 
  inner_join(trt, by = "id")

# Then filter for dates within 6 months and keep the closest match
cohort_trt <- cohort_trt %>%
  # Calculate the absolute difference between dates in days
  mutate(date_diff = abs(as.numeric(difftime(diadat.x, diadat.y, units = "days")))) %>%
  # Filter to keep only rows where dates are within 6 months (180 days)
  filter(date_diff <= 190) %>%
  # Group by the cohort ID to handle cases with multiple matches
  group_by(id, diadat.x) %>%
  # Sort by the date difference within each group
  arrange(date_diff, .by_group = TRUE) %>%
  # Keep only the first row in each group (smallest date difference)
  slice(1) %>%
  # Remove the date_diff column if you don't need it anymore
  select(-date_diff) %>%
  ungroup()
