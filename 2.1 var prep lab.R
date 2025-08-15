## create covariate and label them
## create
## factor(if true)
cohort1 <- cohort1 |> 
  mutate(         
     year1 = case_when(
      year_since_screen == 0 ~ 1,
      year_since_screen == 1 ~ 0,
      TRUE ~ NA_real_),
    period = cut(year, 
                 breaks = c(1989,2000,2011,2023),
                 include.lowest = TRUE),
    period = factor(period, labels = c("1989-1999", "2000-2010", "2011-2023")),
    detection_mode = factor(detection_mode_1, labels = c("ScrCa", "IntCa")),
    Education = factor(Education, levels = c(3,2,1)),
    Education_2cat = case_when(
      Education %in% c(1,2) ~ 1,
      Education == 3 ~ 3,
      TRUE ~ NA),
    Education_2cat = factor(Education_2cat, levels = c(1,3)),
    age_1st_child = factor(age_1st_child, levels = c("<30", ">=30", "None")))


cohort2 <- cohort2 |> 
  mutate(detection_mode = factor(detection_mode, labels = c("ScrCa", "IntCa")),
         ## outcome
         sc_ic = ifelse(detection_mode == "ScrCa", 0,
                        ifelse(detection_mode == "IntCa", 1, NA)),
         month = as.factor(floor(interval/ 30)), # months since screening
         year_since_screen = ifelse(is.na(interval), NA,
                                    ifelse(interval <= 365, 0, 1)),
         year1 = case_when(
           year_since_screen == 0 ~ 1,
           year_since_screen == 1 ~ 0,
           TRUE ~ NA_real_),
        parity = case_when(n_child==0 ~ 0,
                            n_child==1 ~ 1,
                            n_child>=2 ~ 2),
         parity = factor(parity),
         ## age at child
         age_1st_child = case_when(age_child_min<30 & age_child_min>0 ~ "<30",
                                   age_child_min>=30 ~ ">=30",
                                   age_child_min==0 ~ "None"),
         age_1st_child = factor(age_1st_child, levels = c("<30", ">=30", "None")),
         age_1st_child_20_25 = case_when(age_child_min<20  & age_child_min>0~ "<20",
                                         age_child_min<25 & age_child_min>20 ~  "20-25",
                                         age_child_min>=25 ~ ">=25",
                                         age_child_min==0 ~ "None"),
         age_1st_child_20_25 = factor(age_1st_child_20_25, levels = c("<20", "20-25", ">=25", "None")),
         Education_lisa = case_when(Education_lisa == 0 ~ "<10",
                                    Education_lisa == 1 ~ "10-12",
                                    Education_lisa == 2 ~ ">12",
                                    Education_lisa == 9 ~ "Unknown"),
         Education_lisa = factor(Education_lisa, levels = c("<10", "10-12", ">12","Unknown")),
         ## diagnosis age, year, period
         age_at_diagnosis = floor((bc_diagdate - Birthdate)/365.25),
         age_cat = case_when(
             age_at_diagnosis>=40 & age_at_diagnosis < 50 ~ "40-49",
             age_at_diagnosis>=50 & age_at_diagnosis < 60 ~ "50-59",
             age_at_diagnosis>=60 & age_at_diagnosis < 75 ~ "60-74"),
         diadat = bc_diagdate,
         year = year(diadat),
         period = cut(year, 
                      breaks = c(2000,2011,2023),
                      include.lowest = TRUE),
         period = factor(period, labels = c("2000-2010", "2011-2023")),
         # mam features
        pd_BIRADs = cut(pd,
                           breaks = c(0,2,18,49,100),
                           include.lowest = TRUE),
        pd_cat10 = cut(pd,
                          breaks = c(0,10,20,30,40,50,60,100),
                          include.lowest = TRUE),
        pd_3group = cut(pd,
                           breaks = c(0,20,40,100),
                           include.lowest = TRUE),
        pd_3group = factor(pd_3group, levels = c( "[0,20]","(20,40]", "(40,100]")),
        pd_2group = cut(pd,
                           breaks = c(0,20,100),
                           include.lowest = TRUE),
        calcs_bi = case_when(
          calcs == 0 ~ 0, 
          calcs >= 1 ~ 1),
        calcs_bi = factor(calcs_bi, labels = c("0", ">=1")),
        masses_3cat = case_when(
          masses == 0 ~ 0, 
          masses == 1 ~ 1,
          masses >=2 ~ 2),
        masses_3cat = factor(masses_3cat, labels = c("0", "1", ">=2")),
        # menarche_age_cat = cut(menarche_age,
         #                        breaks = c(0,12,15,25),
         #                        include.lowest = TRUE), #max=25
         # menarche_age_cat = factor(menarche_age_cat, levels = c("(12,15]","[0,12]", "(15,25]")),
         bmi_cat = cut(bmi,
                       breaks = c(0,25,55),
                       include.lowest = TRUE),
         bmi_cat = factor(bmi_cat, levels = c("[0,25]", "(25,55]")),
        #   mutate(across(c(smoking_status_dummy, smoking_status_dummy2), factor))
        across(c(risk_breastcancer_own_10yr, GRS_BC), 
                        ~ cut(., breaks = quantile(., probs = c(0, 0.25,0.50,0.75, 1), na.rm = TRUE), 
                              include.lowest = TRUE))
  ) 
