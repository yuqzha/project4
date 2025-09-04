
oldcols <- c("cad_date", "calcs", "masses", "mammography_date", "calcs_l_nbr", "calcs_r_nbr", "masses_l_nbr", "masses_r_nbr") # mammography_date is not date for pd, which is pd_date

cohort2 <- cohort2 |> 
  select(!any_of(oldcols)) # any_of only matches cols that exist 


cohort2 <- cohort2 |> 
  left_join(mam_bf_4y[, c("studieid", "calcs","calcs_l_nbr", "calcs_r_nbr", "masses","masses_l_nbr", "masses_r_nbr")], join_by(id == studieid)) # alternatively use mam_bf_2y

cohort2 <- cohort2 |> 
  mutate(
    calcs_sameside = case_when(
      side_right == 0 ~ calcs_l_nbr,
      side_right == 1 ~ calcs_r_nbr),
    calcs_otherside = case_when(
      side_right == 0 ~ calcs_r_nbr,
      side_right == 1 ~ calcs_l_nbr),
    masses_sameside = case_when(
      side_right == 0 ~ masses_l_nbr,
      side_right == 1 ~ masses_r_nbr),
    masses_otherside = case_when(
      side_right == 0 ~ masses_r_nbr,
      side_right == 1 ~ masses_l_nbr),
    calcs_bi = case_when(
      calcs_sameside == 0 ~ 0, 
      calcs_sameside >= 1 ~ 1),
    calcs_bi = factor(calcs_bi, labels = c("0", ">=1")),
    masses_bi = case_when(
      masses_sameside == 0 ~ 0, 
      masses_sameside >= 1 ~ 1),
    masses_bi = factor(masses_bi, labels = c("0", ">=1")),
    masses_3cat = case_when(
      masses_sameside == 0 ~ 0, 
      masses_sameside == 1 ~ 1,
      masses_sameside >=2 ~ 2),
    masses_3cat = factor(masses_3cat, labels = c("0", "1", ">=2")),
    masses_dif = masses_sameside - masses_otherside,
    masses_dif_4cat = case_when(
      masses_dif < 0 ~ 0, 
      masses_dif == 0 ~ 1,
      masses_dif == 1 ~ 2,
      masses_dif >1 ~ 3),
    masses_dif_4cat = factor(masses_dif_4cat, labels = c("<0", "0","1", ">=2")),
    masses_dif_3cat = case_when(
      masses_dif <= 0 ~ 0, 
      masses_dif == 1 ~ 1,
      masses_dif >1 ~ 2),
    masses_dif_3cat = factor(masses_dif_3cat, labels = c("<=0", "1", ">=2")),
    masses_dif_2cat = case_when(
      masses_dif <= 0 ~ 0, 
      masses_dif >= 1 ~ 1),
    masses_dif_2cat = factor(masses_dif_2cat, labels = c("<=0", ">=1"))
  )