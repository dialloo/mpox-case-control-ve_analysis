
# Load packages --------------------------------
library("pacman")

pacman::p_load(tidyverse,    # data management and visualization
               lubridate    # working with dates/epiweeks
               )

# Global option number format
options(scipen = 9999)


# Helper (custom functions) ------------------------------------------------------------


# This function helps create consistent values for Yes/No variables.
# by reassigning '2' values a zero. 
give_two_a_zero <- function(x){
  x[x == 2 ] <- 0
  x
}


# Source data and define environment variables ---------------------------------------------------------------------

source("00_REDCap-API-linkage.R") # this gives me the raw dataset referred to below.


# This the date of the earliest symptom per the study protocol, 
# but there is flexibility around this date to account for recall bias.
eligibility_date <- ymd("2022-08-19")

# This date represent the most recent data transfer from jurisdictions
redcap_trans_date <- ymd("2023-01-06")



# Tidy data ----------------------------------------------------------------------
tt <- raw %>%
  # Restrict to eligible participants (i.e., those that passed the eligibility screening)
  filter(inelig_stop ==  1) %>% 
  # Restrict to those who have data for relevant eligibility screening questions 
  filter(!if_all(c(elig_age, elig_sexuality, elig_gender,
                   elig_gender_msm, elig_sexual_partners,
                   elig_healthcare), is.na)) %>% 
  mutate(#! Variable name key: 
    #!  *_cat: indicates a categorical these are flags (0/1)
    #!  *_f: these are flags (0/1)
    #!  *_na: these are also flags but specifically related to missingness (NA = missing in R)
    #!  *_int: these refer to some calculate interval
    #!  pd: an abbreviation for period
    # Format all yes/no variables with 1(Yes)/2(No) values as 1(Yes)/0(No).
    across(contains(c("elig_age", "elig_gender_msm", "elig_sexual_partners",
                      "elig_healthcare", "first_dose_yesno", "second_dose_yesno",
                      "symptoms_yesno", "provider_dx", "clinic_yesno", "hospitalized",
                      "dose1_yesno", "dose2_yesno"), ignore.case = TRUE), give_two_a_zero),
    
    
    # Format all date variables as YYYY-MM-DD
    across(contains("_date", ignore.case = TRUE), ymd),
    
    # Create indicator vars to help flag records missing minimum set of data elements need for VE  variables
    
    provider_dx_na = case_when(is.na(provider_dx) ~ 1,
                               TRUE ~ as.numeric(0)),
    
    clinic_yesno_na = case_when((provider_dx == 0 | is.na(provider_dx)) & is.na(clinic_yesno) ~ 1,
                                TRUE ~ as.numeric(0)),
    
    clinic_date_na = case_when(clinic_yesno == 1 & is.na(clinic_date) ~ 1,
                               TRUE ~ as.numeric(0)),
    
    symptoms_yesno_na = case_when(is.na(symptoms_yesno) ~ 1,
                                  TRUE ~ as.numeric(0)),
    
    symptoms_date_na = case_when(symptoms_yesno == 1 & is.na(symptoms_date) ~ 1,
                                 TRUE ~ as.numeric(0)),
    
    first_dose_yesno_na = case_when(is.na(first_dose_yesno) ~ 1,
                                    TRUE ~ as.numeric(0)),
    
    first_dose_date_na = case_when(first_dose_yesno == 1 & is.na(first_dose_date) ~ 1,
                                   TRUE ~ as.numeric(0)),
    
    second_dose_yesno_na = case_when(first_dose_yesno == 1 & is.na(second_dose_yesno) ~ 1,
                                     TRUE ~ as.numeric(0)),
    
    second_dose_date_na = case_when(second_dose_yesno == 1 & is.na(second_dose_date) ~ 1,
                                    TRUE ~ as.numeric(0)),
    
    # Flag records missing any minimum set of data element needed for VE estimate
    minset_vars_self = ifelse(if_any(contains("_na"), ~ . == 1), 0, 1),
    
    # Create indicator vars to help flag records missing jurisdiction completed vax info
    
    case_yesno_na = case_when(is.na(case_yesno) ~ 1,
                              TRUE ~ as.numeric(0)),
    
    test_result_date_na = case_when(case_yesno %in% c(1:2) & is.na(test_result_date) ~ 1,
                                    TRUE ~ as.numeric(0)),
    
    control_visit_date_na = case_when(case_yesno == 3 & is.na(control_visit_date) ~ 1,
                                      TRUE ~ as.numeric(0)),
    
    dose1_yesno_na = case_when(is.na(dose1_yesno) ~ 1,
                               TRUE ~ as.numeric(0)),
    
    dose1_date_na = case_when(dose1_yesno == 1 & is.na(dose1_date) ~ 1,
                              TRUE ~ as.numeric(0)),
    
    dose2_yesno_na = case_when(dose1_yesno == 1 & is.na(dose2_yesno) ~ 1,
                               TRUE ~ as.numeric(0)),
    
    dose2_date_na = case_when(dose2_yesno == 1 & is.na(dose2_date) ~ 1,
                              TRUE ~ as.numeric(0)),
    
    
    # Flag records missing any minimum set of data element needed for VE estimate
    minset_vars_site = ifelse(if_any(c("case_yesno_na", "test_result_date_na", "control_visit_date_na", 
                                       "dose1_yesno_na", "dose1_date_na",
                                       "dose2_yesno_na", "dose2_date_na",
                                       "symptoms_yesno_na", "symptoms_date_na"), ~ . == 1), 0, 1),
    
    
    # Although not part of data quality check report, flag mpox symptoms date within study period: 2022-08-19 and most recent report date 
    study_pd_int = interval(eligibility_date, redcap_trans_date),
    
    study_pd_int_site_f = case_when((test_result_date %within% study_pd_int) |
                                      (control_visit_date %within% study_pd_int) ~ 1,
                                    ((test_result_date < eligibility_date | 
                                        control_visit_date < eligibility_date) |
                                       (test_result_date > redcap_trans_date | 
                                          control_visit_date > redcap_trans_date)) ~ 0,
                                    TRUE ~ as.numeric(NA)),
    
    study_pd_int_self_f = case_when((symptoms_date %within% study_pd_int) |
                                      (clinic_date %within% study_pd_int) ~ 1,
                                    ((symptoms_date < eligibility_date | 
                                        clinic_date < eligibility_date) |
                                       (symptoms_date > redcap_trans_date | 
                                          clinic_date > redcap_trans_date)) ~ 0,
                                    TRUE ~ as.numeric(NA)),
    
    #==== Case/Control Questionnaire Checks ====#
    
    # Dose 1 and Dose 2 Vaccination Interval (self-report) - Flag interval < 24 days and > 24 days to ID implausible intervals
    dose_int_self = ifelse(!is.na(first_dose_date) & !is.na(second_dose_date), second_dose_date - first_dose_date, NA), 
    dose_int_cat_self = fct_relevel(as_factor(case_when(first_dose_yesno != 1                                   ~ "Not Vaxed",
                                                        !is.na(first_dose_date) & is.na(second_dose_date)    ~ "Dose #1 only",
                                                        !is.na(first_dose_date) & !is.na(second_dose_date) & 
                                                          ((second_dose_date - first_dose_date) < 24)        ~ "<24 days",
                                                        !is.na(first_dose_date) & !is.na(second_dose_date) & 
                                                          (((second_dose_date - first_dose_date) %in% c(24:28)))  ~ "24-28 days",
                                                        !is.na(first_dose_date) & !is.na(second_dose_date) & 
                                                          ((second_dose_date - first_dose_date) > 28)            ~ ">28 days",
                                                        TRUE ~ as.character(NA))),
                                    "Dose #1 only", "<24 days", "24-28 days", ">28 days", "Not Vaxed"),
    # site-report: Dose 1 and Dose 2 Vaccination Interval - Flag interval < 24 days and > 24 days to ID implausible intervals
    dose_int_site = ifelse(!is.na(dose1_date) & !is.na(dose2_date), dose2_date - dose1_date, NA),
    dose_int_cat_site = fct_relevel(as_factor(case_when(dose1_yesno != 1                             ~ "Not Vaxed",
                                                        !is.na(dose1_date) & is.na(dose2_date)      ~ "Dose #1 only",
                                                        !is.na(dose1_date) & !is.na(dose2_date) & 
                                                          ((dose2_date - dose1_date) < 24)          ~ "<24 days",
                                                        !is.na(dose1_date) & !is.na(dose2_date) & 
                                                          (((dose2_date - dose1_date) %in% c(24:28)))  ~ "24-28 days",
                                                        !is.na(dose1_date) & !is.na(dose2_date) & 
                                                          ((dose2_date - dose1_date) > 28)          ~ ">28 days",
                                                        TRUE ~ as.character(NA))),
                                    "Dose #1 only", "<24 days", "24-28 days", ">28 days", "Not Vaxed"
    ),
    
    #==== Case & Vaccination Status vs C/C Questionnaire ====#
    
    # Case Status – site vs Self Report - Compare case status from health department to self-reported mpox diagnosis 
    caco_stat_match_f = fct_relevel(as_factor(case_when(case_yesno %in% c(1,2) & provider_dx == 1 ~ "Case: site & self",
                                                        is.na(case_yesno)  & provider_dx == 1 ~ "Case: self but site missing",
                                                        case_yesno == 3 & provider_dx == 0 ~ "Control: site & self",
                                                        is.na(case_yesno)  & provider_dx == 0 ~ "Control: self but site missing",
                                                        case_yesno %in% c(1,2) & provider_dx == 0 ~ "Discrepancy: site case",
                                                        case_yesno == 3 & provider_dx == 1 ~ "Discrepancy: self case",
                                                        TRUE ~ as.character(NA))),
                                    "Case: site & self", "Case: self but site missing",
                                    "Control: site & self", "Control: self but site missing",
                                    "Discrepancy: site case", "Discrepancy: self case"),
    
    # Flag cases with/without symptoms; Everyone who said that they had mpox symptoms, specified at least one of the options.
    Case_symp_f = fct_relevel(as_factor(case_when(caco_stat_match_f %in% c("Case: site & self", "Case: self but site missing") &
                                                    symptoms_yesno == 1 ~ "Case: symptom +",
                                                  caco_stat_match_f %in% c("Case: self & site", "Case: self but site missing") &
                                                    symptoms_yesno == 0 ~ "Case: symptom -",
                                                  caco_stat_match_f %in% c("Control: site & self", "Control: self but site missing") &
                                                    (clinic_date_na == 0 | control_visit_date_na == 0)  ~
                                                    "Control: clinic date +",
                                                  caco_stat_match_f %in% c("Control: site & self", "Control: self but site missing") &
                                                    (clinic_date_na == 1 & control_visit_date_na == 1)  ~
                                                    "Control: clinic date -",
                                                  TRUE ~ as.character(NA))),
                              "Case: symptom +", "Case: symptom -", "Control: clinic date +", "Control: clinic date -"),
    
    # Case/control definition per study protocol 
    caco = fct_relevel(as_factor(case_when(case_yesno %in% c(1,2) & 
                                             (symptoms_yesno == 1 & symptoms_date_na == 0)  ~ "Case",
                                           case_yesno == 3 & 
                                             (control_visit_date_na == 0 | clinic_date_na == 0) ~ "Control",
                                           TRUE ~ as.character(NA))),
                       "Case", "Control"),
    
    elig_case_date_abs = abs(symptoms_date-test_result_date),
    elig_control_date_abs = abs(clinic_date-control_visit_date),
    
    index_date = case_when(caco == "Case" & 
                             ((study_pd_int_site_f == 0 | study_pd_int_self_f == 0) |
                                (study_pd_int_site_f == 1  & study_pd_int_self_f == 1)) &
                             elig_case_date_abs >31                ~ test_result_date,
                           caco == "Case"                          ~ symptoms_date,
                           caco == "Control" & control_visit_date_na == 0 ~ control_visit_date,
                           caco == "Control" & control_visit_date_na == 1 ~ clinic_date,
                           TRUE ~ as.Date(NA)),
    # Site
    index_dose1_int_site = dose1_date - index_date,
    index_dose2_int_site = dose2_date - index_date,
    vax_stat_site = fct_relevel(as_factor(case_when((dose1_yesno == 0 |                                 
                                                    (dose1_yesno == 1 & index_dose1_int_site < 0))      ~ "Unvaccinated", 
                                                    ((dose1_yesno == 1 & dose2_yesno == 1 & index_dose2_int_site <= 13) |
                                                       (dose1_yesno == 1 & dose2_yesno != 1 & index_dose1_int_site > 13))  ~ "Partial",
                                                    dose1_yesno == 1 & dose2_yesno == 1 &
                                                      dose_int_site >=24 & index_dose2_int_site > 13 ~ "Full",
                                                    TRUE ~ as.character(NA))),
                                "Full", "Partial", "Unvaccinated"),
    # Self
    index_dose1_int_self = first_dose_date - index_date,
    index_dose2_int_self = second_dose_date - index_date,
    vax_stat_self = fct_relevel(as_factor(case_when((first_dose_yesno == 0 |
                                                    (first_dose_yesno == 1 & index_dose1_int_self < 0))   ~ "Unvaccinated",
                                                    ((first_dose_yesno == 1 & second_dose_yesno == 1 & index_dose2_int_self <= 13) |
                                                       (first_dose_yesno == 1 & second_dose_yesno != 1 & index_dose1_int_self > 13))  ~ "Partial",
                                                    first_dose_yesno == 1 & second_dose_yesno == 1 &
                                                      dose_int_self >=24 & index_dose2_int_self > 13 ~ "Full",
                                                    TRUE ~ as.character(NA))),
                                "Full", "Partial", "Unvaccinated"))
    
  
