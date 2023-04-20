#========================================================================================================
# Author: Alpha Oumar Diallo (YQL8@cdc.gov) & Amy Fothergill (UYL0@cdc.gov)
# CDC Collaborators: Anna N Chard, Alexandra F Dalton, Nicholas P Deputy, Leora R Feldstein, 
#                    and Danielle L Moulia 
#                    as part of the Case-Control Unit|VE Team|VTF|2022 Multinational Monkeypox Response
# Program: 06_tidy-data
# Project: Multijurisdictional Mpox JYNNEOS Vaccine effectiveness Case Control Study
# Main Objectives: 
#            1. Evaluate the effectiveness of the JYNNEOS vaccine in preventing symptomatic mpox disease 
#               among 18-49 years old gay, bisexual, and other men who have sex with men, as well as 
#               transgender persons residing in the US.
#            2. Estimate effectiveness by immunocompromising conditions and route of administmatch_ration
# Task: Import raw data from REDCap and clean  
# Data in: REDCap using API token
# Initial program date: 2023-03-28
# Last modified date (note): 2023-04-17 (change match week from 4 to 2)
#==========================================================================================================

# Load packages & helper functions --------------------------------
pacman::p_load(here,        # file paths relative to R project root folder
               rio,         # import/export
               readxl,      # read excel files
               tidyverse,   # data management and visualization
               lubridate,   # working with dates/epiweeks
               MatchIt      # match cases and controls
               #collaborator
)

# Global option number format
options(scipen = 9999)

#This function replicates the SAS proc freq function with the '/ list missing statement'. 
#Source: https://github.com/asnr/sas-to-r
pfreq = function(...) {
  dplyr::group_by(...) %>%
    dplyr::summarize(n = n()) %>%
    dplyr::mutate(pct= round(100 * n/sum(n), 1)) 
  #mutate(Percent=paste0(round(100 * n/sum(n), 1), "%")) 
}

# This function helps create consistent values for Yes/No variables.
# by reassigning '2' values a zero. 
give_two_a_zero <- function(x){
  x[x == 2 ] <- 0
  x
}

# This functions helps create two level factor variables
turn_to_2Lfactor <- function(x){
  x = fct_relevel(as_factor(case_when(x == 1 ~ "Yes",
                                      x == 0 ~ "No",
                                      TRUE ~ as.character(NA))),
                  "No", "Yes")
  x
}

# This functions helps create three level factor variables
turn_to_3Lfactor <- function(x){
  x = fct_relevel(as_factor(case_when(x == 1 ~ "Yes",
                                      x == 2 ~ "No",
                                      x == 3 ~ "Don't know",
                                      TRUE ~ as.character(NA))),
                  "Yes", "No", "Don't know")
  x
}

# Source data and define environment variables ---------------------------------------------------------------------

# source("00_REDCap-API-linkage.R")

raw <- here("data", "MultiJurisdictionalM_DATA_2023-03-31_1922.csv") %>%
read_csv()

#MultiJurisdictionalM_DATA_2023-03-31_1922
#MultiJurisdictionalM_DATA_2023-04-14_1533

# Manual review results 
mr <- here("data", "mpoxve_manual_review_combined_aod.xlsx") %>% 
  read_excel(sheet = "with_pid") %>% 
  rename_with(function(x) paste0("mr_", x), -pid)%>% 
  mutate(mr = 1) %>% 
  select(mr, pid, everything())


# Tidy data -------------------------------------------------------------------------------------------------------------
mpoxve <- raw %>%    
  # Restrict to eligible respondents (i.e., those that passed the eligibility screening)
  filter(inelig_stop ==  1) %>% 
  # exclude those who did not give consent in NYC
  filter(is.na(consent) | consent == 1) %>% 
  # Restrict to those who have data for relevant eligibility screening questions 
  filter(!if_all(c(elig_age, elig_sexuality, elig_gender,
                   elig_gender_msm, elig_sexual_partners,
                   elig_healthcare), is.na)) %>%
  # Join manual review dataset
  left_join(mr, by = c("pid")) %>% 
  mutate( #! Variable name key: 
    #!  *_cat: indicates a categorical these are flags (0/1)
    #!  *_f: these are flags (0/1)
    #!  *_na: these are also flags but specifically related to missingness (NA = missing in R)
    #!  *_int: these refer to some calculate interval
    #!  pd: an abbreviation for period
    
    #
    mr = ifelse(is.na(mr), 0, mr),
    
    #==== Key dates to modify ----
    eligibility_date        = ymd("2022-08-19"),
    
    redcap_trans_date       = ymd("2023-04-14"),
    
    mmwr_data_closing_date  = ymd("2023-04-31"),
    
    #==== General variable cleaning ----
    # Format all yes/no variables with 1(Yes)/2(No) values as 1(Yes)/0(No).
    across(contains(c("elig_age", "elig_gender_msm", "elig_sexual_partners",
                      "elig_healthcare", "first_dose_yesno", "second_dose_yesno",
                      "symptoms_yesno", "provider_dx", "clinic_yesno", "hospitalized",
                      "dose1_yesno", "dose2_yesno"), ignore.case = TRUE), give_two_a_zero),
    
    # Format all date variables as YYYY-MM-DD
    across(contains("_date", ignore.case = TRUE), ymd),
    
    # Rename REDCap Data Access Group variable and capitalize the letters
    jid = fct_relevel(as_factor(toupper(redcap_data_access_group)),
                      "CA", "LAC", "CO", "CT", "GA", "MD", "MN",
                      "NYS", "NYC", "OR", "TN", "DC"),
    
    # State ID
    sid = fct_relevel(factor(case_when(jid == "LAC" ~ "CA",
                                       jid %in% c("NYS", "NYC") ~ "NY", 
                                       jid == "DC" ~ "MD", 
                                       TRUE ~ as.character(jid)))),
    
    #==== Index date (mpox symptom onset [for cases] or clinic visit date [for controls]) ----
    
    # Create a study period variable to store mpox symptoms date within 2022-08-19 and most recent data transfer 
    study_pd_int = interval(eligibility_date, redcap_trans_date),
    
    # address invalid dates for self-reported symptoms and clinic dates
    test_yr = case_when(test_result_date > redcap_trans_date ~ 2022,
                        year(test_result_date) == 2021 ~ 2022,
                        test_result_date < mmwr_data_closing_date ~ 2023,
                        TRUE ~ as.numeric(year(test_result_date))),
    
    test_result_date = make_date(year = test_yr, month = month(test_result_date),
                                 day = day(test_result_date)),
    
    symptoms_yr = case_when(symptoms_date > redcap_trans_date ~ 2022,
                            year(symptoms_date) == 2021 ~ 2022,
                            symptoms_date < mmwr_data_closing_date ~ 2023,
                            TRUE ~ as.numeric(year(symptoms_date))),
    
    symptoms_date = make_date(year = symptoms_yr, month = month(symptoms_date),
                              day = day(symptoms_date)),
    
    clinic_date_mod = case_when(clinic_date == ymd("2012-10-02") ~ ymd("2012-10-02"),
                                clinic_date == ymd("2013-05-10") ~ ymd("2022-10-05"),
                                clinic_date == ymd("2018-11-20") ~ ymd("2022-11-20"),
                                clinic_date == ymd("2019-02-19") ~ ymd("2023-02-19"),
                                TRUE ~ as.Date(clinic_date)),
    
    self_clinic_yr = case_when(clinic_date_mod > redcap_trans_date ~ 2022,
                               year(clinic_date_mod) == 2021 ~ 2022,
                               clinic_date_mod < mmwr_data_closing_date ~ 2023,
                               TRUE ~ as.numeric(year(clinic_date_mod))),
    
    clinic_date = make_date(year = self_clinic_yr, month = month(clinic_date_mod),
                            day = day(clinic_date_mod)),
    
    index_date = case_when(test_result_date %within% study_pd_int ~ test_result_date,
                           ((is.na(test_result_date) | #if incomplete or
                               (test_result_date < eligibility_date)) & # before Aug 19 
                              (symptoms_date %within% study_pd_int)) ~ symptoms_date,
                           clinic_date %within% study_pd_int ~ clinic_date,
                           ((is.na(clinic_date) | #if incomplete or 
                               (clinic_date < eligibility_date)) & # before Aug 19 
                              (control_visit_date %within% study_pd_int)) ~ control_visit_date,
                           TRUE ~ as.Date(NA)),
    
    #==== Case ascertainment----
    #  2022-01-18 data call decisions:  
    #      Cases: Use site test result date, and if missing, use self-reported symptoms date
    #      Controls: Use self-report clinic date, if self-reported missing OR before Aug 19th, 
    #                then use the sit- reported clinic date
    
    
    # Case status categories
    caco_stat_cat = fct_relevel(as_factor(case_when(
      
      case_yesno %in% c(1,2) & # Site-reported as confirmed or probable case
        provider_dx == 1 &
        is.na(index_date)      ~ "Case: invalid date",
      
      case_yesno %in% c(1,2) & # Site-reported as confirmed or probable case
        (provider_dx == 0 &
           clinic_yesno == 1) ~ "Case-site, Control-self",
      
      case_yesno %in% c(1,2) & # Site-reported as confirmed or probable case
        (provider_dx == 0 &
           clinic_yesno != 1) ~ "Case: self-no dx & clinic visit",
      
      case_yesno %in% c(1,2) & # Site-reported as confirmed or probable case
        (provider_dx == 1 | is.na(provider_dx)) &
        !is.na(index_date)     ~ "Case",
      
      case_yesno == 3 & # Site-reported as control
        provider_dx == 1     ~ "Control-site, Case-self",
      
      case_yesno == 3 & # Site-reported as confirmed or probable case
        provider_dx != 1 &
        is.na(index_date) ~ "Control: invalid date",
      
      case_yesno == 3 & # Site-reported as control
        is.na(clinic_date) &
        !is.na(index_date)  ~ "Control: no clinic visit-self",
      
      case_yesno == 3 & # Site-reported as control
        clinic_yesno == 1 &
        #provider_dx != 1 &
        !is.na(index_date)    ~ "Control",
      
      is.na(case_yesno) ~ "Site-report missing",
      
      TRUE ~ as.character(NA))),
      "Case", 
      "Case: invalid date",
      "Case: self-no dx & clinic visit",
      "Control",
      "Control: no clinic visit-self",
      "Control: invalid date",
      "Case-site, Control-self",
      "Control-site, Case-self", 
      "Site-report missing"),
    
    # binary case status: main analysis
    caco = fct_relevel(as_factor(case_when(caco_stat_cat %in% c("Case",
                                                                "Control-site, Case-self",
                                                                "Case-site, Control-self",
                                                                "Case: self-no dx & clinic visit") ~ "Case",
                                           caco_stat_cat %in% c("Control",
                                                                
                                                                "Control: no clinic visit-self") ~ "Control",
                                           TRUE ~ as.character(NA))),
                       "Case", "Control"),
    
    # binary case status: sensitivity analysis, controls who did not self-report as having clinic visit
    caco_sen = fct_relevel(as_factor(case_when(caco_stat_cat %in% c("Case") ~ "Case",
                                               caco_stat_cat %in% c("Control") ~ "Control",
                                               TRUE ~ as.character(NA))),
                           "Case", "Control"),
    
    # Re assign index dates based on case status
    index_date = case_when((caco_stat_cat == "Control-site, Case-self" &
                              (symptoms_date %within% study_pd_int)) ~ symptoms_date,
                           (caco_stat_cat == "Case-site, Control-self" &
                              (test_result_date %within% study_pd_int)) ~ test_result_date,
                           !caco_stat_cat %in%c("Control-site, Case-self", "Case-site, Control-self") ~ index_date,
                           TRUE ~ as.Date(NA)),
    
    #==== Vaccination Status ----
    
    # address invalid dates for self-reported vaccination dates
    d1_yr = case_when(first_dose_date > redcap_trans_date ~ 2022,
                      year(first_dose_date) == 2021 ~ 2022,
                      first_dose_date < mmwr_data_closing_date ~ 2023,
                      TRUE ~ as.numeric(year(first_dose_date))),
    first_dose_date = make_date(year = d1_yr, month = month(first_dose_date), day = day(first_dose_date)),
    
    d2_yr = case_when(second_dose_date > redcap_trans_date ~ 2022,
                      year(second_dose_date) == 2021 ~ 2022,
                      second_dose_date < mmwr_data_closing_date ~ 2023,
                      TRUE ~ as.numeric(year(second_dose_date))),
    second_dose_date = make_date(year = d2_yr, month = month(second_dose_date), day = day(second_dose_date)),
    
    # Combine site- and self-reported vaccination dose variables
    vax1_yesno = case_when((is.na(dose1_yesno) & !is.na(first_dose_yesno)) |
                             (dose1_yesno == 0 & first_dose_yesno == 1)      ~ first_dose_yesno,
                           TRUE ~ as.numeric(dose1_yesno)),
    
    vax1_date = case_when((!is.na(first_dose_date) & !is.na(second_dose_date) & is.na(dose2_date)) | 
                            (is.na(dose1_date) &
                               first_dose_yesno == 1 & !is.na(first_dose_date)) ~ first_dose_date,
                          TRUE ~ as.Date(dose1_date)),
    
    vax1_yrmo = format_ISO8601(vax1_date, precision = "ym"),
    
    vax2_yesno = case_when((is.na(dose2_yesno) & !is.na(second_dose_yesno)) |
                             (dose2_yesno == 0 & second_dose_yesno == 1) ~ second_dose_yesno,
                           TRUE ~ as.numeric(dose2_yesno)),
    
    vax2_date = case_when(is.na(dose2_date) &
                            second_dose_yesno == 1 & !is.na(second_dose_date) ~ second_dose_date,
                          TRUE ~ as.Date(dose2_date)),
    
    vax2_yrmo = format_ISO8601(vax2_date, precision = "ym"),
    
    # Time (days) between index event date and 1st/2nd doses
    dose1index_days = index_date - vax1_date,
    
    dose2index_days = index_date - vax2_date,
    
    dose_int        = vax2_date - vax1_date,
    
    # Vax status: Site
    vax_stat_na = as.character(NA),
    
    vax_stat_acip = fct_relevel(as_factor(case_when(!is.na(index_date) &
                                                      (vax1_yesno == 0 &  # no documented vaccination OR Post index event vax
                                                         (vax2_yesno == 0 | is.na(vax2_yesno))) ~ "S1",
                                                    vax1_date >= index_date ~ "S2",
                                                    vax1_yesno == 1 &
                                                      dose1index_days <= 13    ~ "S3",
                                                    (vax1_yesno == 1 & vax2_yesno != 1 &
                                                       dose1index_days >= 14)   ~ "S4",
                                                    (vax1_yesno == 1 & vax2_yesno == 1 &
                                                       dose1index_days >= 14 & dose_int <24) ~ "S5",
                                                    (vax2_yesno == 1 &
                                                       dose1index_days >= 14 &
                                                       dose_int >=24 & dose2index_days < 14) ~ "S6",
                                                    (vax2_yesno == 1 &
                                                       dose_int >=24 & dose2index_days >= 14) ~ "S7",
                                                    TRUE ~ as.character(vax_stat_na))),
                                "S1", "S2", "S3", "S4", "S5", "S6", "S7"),
    
    mr_vax_stat_acip = fct_relevel(as_factor(mr_vax_stat_acip), 
                                   "S1", "S2", "S3", "S4", "S5", "S6", "S7"),
    
    mr_review_vax_stat = fct_relevel(as_factor(mr_scenario_r1), 
                                     "S1", "S2", "S3", "S4", "S5",
                                     "S6", "S7", "Exclude/investigate"),
    
    vax_scenario = fct_relevel(as_factor(case_when(!is.na(index_date) &
                                                     (vax1_yesno == 0 &  # no documented vaccination OR Post index event vax
                                                        (vax2_yesno == 0 | is.na(vax2_yesno))) ~ "S1",
                                                   vax1_date >= index_date ~ "S2",
                                                   vax1_yesno == 1 &
                                                     dose1index_days <= 13    ~ "S3",
                                                   ((vax1_yesno == 1 & vax2_yesno != 1 &
                                                       dose1index_days >= 14) |
                                                      (vax2_yesno == 1 &
                                                         dose1index_days >= 14 &
                                                         dose_int >=24 & dose2index_days <= 0))   ~ "S4",
                                                   (vax1_yesno == 1 & vax2_yesno == 1 &
                                                      dose1index_days >= 14 & dose_int <24) ~ "S5",
                                                   (vax2_yesno == 1 &
                                                      dose1index_days >= 14 &
                                                      dose_int >=24 & dose2index_days < 14) ~ "S6",
                                                   (vax2_yesno == 1 &
                                                      dose_int >=24 & dose2index_days >= 14) ~ "S7",
                                                   TRUE ~ as.character(vax_stat_na))),
                               "S1", "S2", "S3", "S4", "S5", "S6", "S7"),
    
    vax_stat = fct_relevel(as_factor(case_when(!is.na(index_date) & 
                                                 ((vax1_yesno == 0 &  # no documented vaccination OR Post index event vax
                                                     (vax2_yesno == 0 | is.na(vax2_yesno)))|
                                                    (vax1_date >= index_date)) ~ "Unvaccinated",
                                               (vax1_yesno == 1 &                   # 1 dose but dose1-index date interval < 14 days
                                                  dose1index_days <= 13)    ~ "PEP",
                                               ((vax1_yesno == 1 & vax2_yesno != 1 &   # 1 dose partial (1st dose but no 2nd dose)
                                                   dose1index_days >= 14) |
                                                  (vax1_yesno == 1 & vax2_yesno == 1 &  # D1 is <24 days before D2
                                                     dose1index_days >= 14 &          
                                                     dose_int <24) |
                                                  (vax2_yesno == 1 &                  # 2 doses partial
                                                     dose1index_days >= 14 &          # (dose 2-index date interval < 14 days )
                                                     dose_int >=24 & dose2index_days < 14)) ~ "Partially vaccinated",
                                               (vax2_yesno == 1 &
                                                  dose_int >=24 & dose2index_days >= 14) ~ "Fully vaccinated",  
                                               TRUE ~ as.character(vax_stat_na))),
                           "Fully vaccinated", "Partially vaccinated", "PEP", "Unvaccinated"),
    
    # Exclude those who had 2 doses but D1 is <24 days before D2 for VE analysis
    vax_stat_ve = case_when((vax1_yesno == 1 & vax2_yesno == 1 & 
                               dose1index_days >= 14 & dose_int <24) | # S5: Partial, D1-D2 interval <24d
                              vax_stat == "PEP" ~ NA_character_,     # S3: PEP, IE based
                            TRUE ~ as.character(vax_stat)),
    
    vax_stat_ve_acip = case_when((vax1_yesno == 1 & vax2_yesno == 1 & 
                                    dose1index_days >= 14 & dose_int <24) | # S5: Partial, D1-D2 interval <24d
                                   (vax2_yesno == 1 & dose1index_days >= 14 &  
                                      dose_int >=24 & dose2index_days < 14) |   # S6: Partial, D2 <14 before IE,
                                   vax_stat == "PEP" ~ NA_character_,     # S3: PEP, IE based
                                 TRUE ~ as.character(vax_stat)),
    
    # Model 1 (main model):
    vax_stat_ve_main = case_when(vax_scenario %in% c("S1", "S2") ~ "Unvaccinated",
                                 vax_scenario %in% c("S4", "S6") ~ "Partially vaccinated",
                                 vax_scenario %in% c("S7") ~ "Fully vaccinated",
                                 TRUE ~ as.character(NA)),
    
    # Sensitivity Model 2 (include S6 as fully vaccinated): 
    vax_stat_ve_s6full = case_when(vax_scenario %in% c("S1", "S2") ~ "Unvaccinated",
                                   vax_scenario %in% c("S4") ~ "Partially vaccinated",
                                   vax_scenario %in% c("S7", "S6") ~ "Fully vaccinated",
                                   TRUE ~ as.character(NA)),
    
    #Sensitivity Model 3 (exclude S6 all together)
    vax_stat_ve_s6excl = case_when(vax_scenario %in% c("S1", "S2") ~ "Unvaccinated",
                                   vax_scenario %in% c("S4") ~ "Partially vaccinated",
                                   vax_scenario %in% c("S7") ~ "Fully vaccinated",
                                   TRUE ~ as.character(NA)),
    
    # Post-exposure prophylaxis
    pep_stat_ve = fct_relevel(as_factor(case_when(why_vax___1 == 1 ~ "PEP-Vaccinated",
                                                  vax_stat == "Unvaccinated" ~ "Unvaccinated",
                                                  TRUE ~as.character(NA))),
                              "PEP-Vaccinated", "Unvaccinated"),
    # Combine site- and self-reported vaccination dose administmatch_ration route variables
    
    vax1_adminroute = case_when(!is.na(vax1_date) &
                                  (dose1_adminroute == 3 & first_dose_site == 1) ~ 2,
                                !is.na(vax1_date) &
                                  (dose1_adminroute == 3 & first_dose_site == 2) ~ 1,
                                is.na(dose1_adminroute) ~ first_dose_site,
                                TRUE ~ as.numeric(dose1_adminroute)),
    
    vax1_adminroute_cat = fct_relevel(as_factor(case_when(vax1_adminroute == 1 ~ "Intradermal",
                                                          vax1_adminroute == 2 ~ "Subcutaneous",
                                                          TRUE ~ as.character("Don't know"))),
                                      "Subcutaneous", "Intradermal", "Don't know"),
    
    vax2_adminroute = case_when(!is.na(vax2_date) &
                                  (dose2_adminroute == 3 & second_dose_site == 1) ~ 2,
                                !is.na(vax2_date) &
                                  (dose2_adminroute == 3 & second_dose_site == 2) ~ 1,
                                is.na(dose2_adminroute) ~ second_dose_site,
                                TRUE ~ as.numeric(dose2_adminroute)),
    
    vax2_adminroute_cat = fct_relevel(as_factor(case_when(vax2_adminroute == 1 ~ "Intradermal",
                                                          vax2_adminroute == 2 ~ "Subcutaneous",
                                                          TRUE ~ as.character("Don't know"))),
                                      
                                      "Subcutaneous", "Intradermal", "Don't know"),
    
    fully_adminroute_cat = fct_relevel(as_factor(case_when(vax_stat_ve_main == "Fully vaccinated" &
                                                             vax1_adminroute_cat == "Intradermal" &
                                                             vax2_adminroute_cat == "Intradermal" ~ "Intradermal",
                                                           vax_stat_ve_main == "Fully vaccinated" &
                                                             vax1_adminroute_cat == "Subcutaneous" &
                                                             vax2_adminroute_cat == "Subcutaneous" ~ "Subcutaneous",
                                                           vax_stat_ve_main == "Fully vaccinated" &
                                                             vax1_adminroute_cat != vax2_adminroute_cat ~ "Heterologous",
                                                           vax_stat_ve_main == "Fully vaccinated" ~ "Don't know",
                                                           TRUE ~ as.character(NA))),
                                       "Subcutaneous", "Intradermal", "Heterologous", "Don't know"),
    
    partially_adminroute_cat = fct_relevel(as_factor(case_when(vax_stat_ve_main == "Partially vaccinated" &
                                                                 vax1_adminroute_cat == "Intradermal" ~ "Intradermal",
                                                               vax_stat_ve_main == "Partially vaccinated" &
                                                                 vax1_adminroute_cat == "Subcutaneous" ~ "Subcutaneous",
                                                               vax_stat_ve_main == "Partially vaccinated" ~ "Don't know",
                                                               TRUE ~ as.character(NA))),
                                           "Subcutaneous", "Intradermal", "Don't know"),
    
    fully_adminroute_mod = fct_rev(fct_relevel(as_factor(case_when(vax_stat_ve_main == "Unvaccinated" ~ "Unvaccinated",
                                                                   TRUE ~ as.character(fully_adminroute_cat))),
                                               "Heterologous", "Intradermal", "Subcutaneous", "Unvaccinated")),
    
    partially_adminroute_mod = fct_rev(fct_relevel(as_factor(case_when(vax_stat_ve_main == "Unvaccinated" ~ "Unvaccinated",
                                                                       partially_adminroute_cat == "Don't know" ~ NA_character_,
                                                                       TRUE ~ as.character(partially_adminroute_cat))),
                                                   "Intradermal", "Subcutaneous", "Unvaccinated")),
    
    #==== Minimum set variables ----
    # Create indicator vars to help flag records missing minimum set of data elements need for VE  variables
    minset_vars = fct_relevel(as_factor(case_when(is.na(case_yesno) |
                                                    is.na(vax1_yesno) |
                                                    (vax1_yesno == 1 & is.na(vax1_date)) |
                                                    (vax1_yesno == 1 & is.na(vax2_yesno)) |
                                                    (vax2_yesno == 1 & is.na(vax2_date)) |
                                                    (!is.na(control_visit_date) &
                                                       !is.na(test_result_date)) | # MD respondents where site-reported test results and control_visit date are available; they people who had test results before Aug 19 as controls.
                                                    #is.na(provider_dx) |
                                                    #is.na(clinic_yesno) | 
                                                    is.na(index_date) ~ "Incomplete",
                                                  TRUE ~ as.character("Complete"))),
                              "Complete", "Incomplete"),
    
    #==== Clean other anlaysis variables ----
    # Check to make sure birth year is within range of a person being between 18-49 years old
    #               Invalid birth years/ages should be captured via REDCap checks, except for some unusual
    #                situations which should be reviewed and resolved on a case-by-case basis 
    # Calculate age based on birth year and symptom or clinic date
    age = year(index_date) - birth_year,
    
    # Flag if submitted date– birth_year <18 | > 49 (AOD: we do not have submitted date) 
    age_f = fct_relevel(as_factor(case_when(age < 18          ~ "<18",
                                            age %in% c(18:49) ~ "18-49",
                                            age > 49          ~ "\u226550",
                                            TRUE ~ as.character(NA))),
                        "<18", "18-49", "\u226550"),
    
    # Age categorical
    age_cat = fct_relevel(as_factor(case_when(age %in% c(18:29) ~ "18-29",
                                              age %in% c(30:39) ~ "30-39",
                                              age >=40 ~ "40-49",
                                              TRUE ~ as.character(NA))),
                          "18-29", "30-39", "40-49"),
    
    #categorical race variable
    race_cat = fct_relevel(as_factor(case_when((race___1 == 1 & race___2 == 0 & race___3 == 0 &
                                                  race___4 == 0 & race___5 == 0 & race___6 == 0) ~ "African American or Black",
                                               (race___1 == 0 & race___2 == 1 & race___3 == 0 &
                                                  race___4 == 0 & race___5 == 0 & race___6 == 0) ~ "White",
                                               (race___1 == 0 & race___2 == 0 & race___3 == 1 &
                                                  race___4 == 0 & race___5 == 0 & race___6 == 0) ~ "Asian",
                                               (race___1 == 0 & race___2 == 0 & race___3 == 0 &
                                                  race___4 == 1 & race___5 == 0 & race___6 == 0) ~ "Native Hawaiin/Pacific Islander",
                                               (race___1 == 0 & race___2 == 0 & race___3 == 0 &
                                                  race___4 == 0 & race___5 == 1 & race___6 == 0) ~ "American Indian/Alaska Native", 
                                               (race___1 == 0 & race___2 == 0 & race___3 == 0 &
                                                  race___4 == 0 & race___5 == 0 & race___6 == 1) ~ "Prefer not to answer",
                                               (race___1 == 1 | race___2 == 1 | race___3 == 1 |
                                                  race___4 == 1 | race___5 == 1 | race___6 == 1) ~ "Multiracial",
                                               TRUE ~ NA_character_)), 
                           "African American or Black", "White", "Asian",
                           "Native Hawaiin/Pacific Islander", "American Indian/Alaska Native", "Prefer not to answer",
                           "Multiracial"),
    
    
    #race-ethnicity: ethnicity (1, Hispanic or Latino | 2, Not Hispanic or Latino | 3, Prefer not to answer)
    race_ethn_8_cat = fct_relevel(as_factor(case_when(ethnicity == 1 ~ "Hispanic",
                                                    race_cat == "White" ~ "White",
                                                    race_cat == "African American or Black" ~ "Black",
                                                    race_cat == "Asian" ~ "Asian",
                                                    race_cat == "Native Hawaiin/Pacific Islander" ~ "Native Hawaiin/Pacific Islander",
                                                    race_cat == "American Indian/Alaska Native" ~ "American Indian/Alaska Native",
                                                    race_cat == "Multiracial" ~ "Multiracial",
                                                    TRUE ~ as.character("Other"))),
                                "Black", "White", "Hispanic", "Asian",
                                "Native Hawaiin/Pacific Islander", "American Indian/Alaska Native", "Multiracial", "Other"),
    
    race_ethn = fct_relevel(as_factor(case_when(ethnicity == 1 ~ "Hispanic",
                                                race_cat == "White" ~ "White",
                                                race_cat == "African American or Black" ~ "Black",
                                                TRUE ~ as.character("Other"))),
                            "Black", "White", "Hispanic", "Other"),
    
    
    
    # Length of Hospitalization - Examine if length of days of hospitalization is longer than
    #                             the time between illness onset and survey submission
    #                             Flag if hospitalized_days > (submitted date - symptoms_date)
    #!                            AOD: We do not have a submitted date variable at CDC as the sites
    #!                                are not uploading the time stamps. However, only 2 people reported
    #!                                being hospitalized and their days of hospitalization (3 & 13) seem plausible.
    hospitalized_cat = fct_relevel(as_factor(case_when(hospitalized == 0 ~ "No",
                                                       hospitalized == 1 ~ "Yes",
                                                       TRUE ~ as.character(NA))),
                                   "Yes", "No"),
    hospitalized_days_cat = fct_relevel(as_factor(case_when(hospitalized_days == 0         ~ "0",
                                                            hospitalized_days %in% c(1:7)   ~ "1-7",
                                                            hospitalized_days %in% c(8:14)  ~ "8-14",
                                                            hospitalized_days %in% c(15:21) ~ "15-21",
                                                            hospitalized_days %in% c(22:28) ~ "22-28",
                                                            hospitalized_days >= 29         ~ "\u226529",
                                                            TRUE ~ as.character(NA))),
                                        "0", "1-7","8-14", "15-21", "22-28", "22-28", "\u226529"),
    
    # Number of sexual partners - Examine distribution of number of sexual partners for any implausible values
    #                   AOD: what would be considered implausible values?  Range: 0-15 for cases; 0-20 for controls                     
    sex_part_num = ifelse(caco == "Case", sex_part_num_case, 
                          ifelse(caco == "Control", sex_part_num_control, NA)),
    
    sex_part_num_cat = fct_relevel(as_factor(case_when(sex_part_num == 0 ~ "0",
                                                       sex_part_num == 1 ~ "1",
                                                       sex_part_num == 2 ~ "2",
                                                       sex_part_num == 3 ~ "3",
                                                       sex_part_num >= 4 ~ "\u22654",
                                                       TRUE ~ as.character(NA))),
                                   "0", "1", "2", "3", "\u22654"),
    
    sex_part_num_mod = fct_relevel(as_factor(case_when(sex_part_num == 0 ~ "0",
                                                       sex_part_num == 1 ~ "1",
                                                       sex_part_num == 2 ~ "2",
                                                       sex_part_num == 3 ~ "3",
                                                       sex_part_num >= 4 ~ "\u22654",
                                                       TRUE ~ as.character("missing"))),
                                   "0", "1", "2", "3", "\u22654", "missing"),
    
    # Mpox exposure history: contacts
    
    close_contact_dx = case_when(caco == "Case" & !is.na(contact_mpx_dx_case) ~ contact_mpx_dx_case,
                                 caco == "Case" & 
                                   (is.na(contact_mpx_dx_case) & 
                                      !is.na(contact_mpx_dx_control)) ~ contact_mpx_dx_control,
                                 caco == "Control" & !is.na(contact_mpx_dx_control) ~ contact_mpx_dx_control,
                                 caco == "Control" & 
                                   (is.na(contact_mpx_dx_control) & 
                                      !is.na(contact_mpx_dx_case)) ~ contact_mpx_dx_case,
                                 TRUE ~ as.numeric(3)),
    
    close_contact_dx_cat = fct_relevel(as_factor(case_when(close_contact_dx == 1 ~ "Yes",
                                                           close_contact_dx == 2 ~ "No",
                                                           close_contact_dx == 3 ~ "Don't know",
                                                           TRUE ~ as.character(NA))),
                                       "Yes", "No","Don't know"),
    
    elig_gender_cat = fct_relevel(as_factor(case_when(elig_gender == 1 ~ "Male",
                                                      elig_gender == 2 ~ "Female",
                                                      elig_gender == 3 ~ "Transgender male",
                                                      elig_gender == 4 ~ "Transgender female",
                                                      elig_gender == 5 ~ "Another gender identity",
                                                      TRUE ~ as.character(NA))),
                                  "Male", "Female", "Transgender male", "Transgender female",
                                  "Another gender identity"),
    
    elig_sexuality_cat = fct_relevel(as_factor(case_when(elig_sexuality == 1 ~ "Gay or lesbian",
                                                         elig_sexuality == 2 ~ "Bisexual",
                                                         elig_sexuality == 3 ~ "Straight",
                                                         elig_sexuality == 4 ~ "A different term",
                                                         TRUE ~ as.character("Prefer not to answer"))),
                                     "Gay or lesbian", "Bisexual", "Straight", "A different term",
                                     "Prefer not to answer"),
    
    health_ins_cat = fct_relevel(as_factor(case_when(health_ins == 1 ~ "Public",
                                                     health_ins == 2 ~ "Private",
                                                     health_ins == 3 ~ "Public & private",
                                                     health_ins == 4 ~ "None",
                                                     health_ins == 5 ~ "Don't know",
                                                     TRUE ~ as.character(NA))),
                                 "Public", "Private", "Public & private", "None","Don't know"),
    
    housing_cat = fct_relevel(as_factor(case_when(homeless %in% c(1, 2) ~ "Homeless",
                                                  homeless == 3 ~ "Sheltered",
                                                  homeless == 4 ~ "Prefer not to answer",
                                                  TRUE ~ as.character(NA))),
                              "Homeless", "Sheltered", "Prefer not to answer"),
    
    hiv_status_cat = fct_relevel(as_factor(case_when(hiv_status == 1 ~ "Living with HIV",
                                                     hiv_status == 2 ~ "Not living with HIV",
                                                     hiv_status == 3 ~ "Don't know",
                                                     hiv_status == 4 ~ "Prefer not to answer",
                                                     TRUE ~ as.character(NA))),
                                 "Living with HIV", "Not living with HIV", "Don't know", "Prefer not to answer"),
    
    hiv_cd4_cat = fct_relevel(as_factor(case_when(hiv_cd4 == 1 ~ "<200",
                                                  hiv_cd4 == 2 ~ "200-500",
                                                  hiv_cd4 == 3 ~ ">500",
                                                  hiv_cd4 == 4 ~ "Don't know",
                                                  hiv_cd4 == 5 ~ "CD4 not tested in past 6mos",
                                                  TRUE ~ as.character(NA))),
                              "<200", "200-500", ">500", "Don't know", "CD4 not tested in past 6mos"),
    
    hiv_cd4_200_cat = fct_relevel(as_factor(case_when(hiv_cd4 == 1 ~ "Yes",
                                                      hiv_cd4 %in% c(2:5) ~ "No",
                                                  TRUE ~ as.character(NA))),
                              "No", "Yes"),
    hiv_meds_2moredays = fct_relevel(as_factor(case_when(hiv_meds %in% c(3:6) ~ "Yes",
                                                         hiv_meds %in% c(1:2) ~ "No",
                                                         TRUE ~ as.character(NA))),
                                     "No", "Yes"),
    
    hiv_prep_cat = fct_relevel(as_factor(case_when(hiv_prep == 1 ~ "Yes",
                                                   hiv_prep == 2 ~ "No",
                                                   hiv_prep == 3 ~ "Don't know",
                                                   TRUE ~ as.character(NA))),
                               "Yes", "No", "Don't know"),
    
    immunocompromised = fct_relevel(as_factor(case_when(hiv_status == 1 | immune_response ==  1 ~ "Yes",
                                                        hiv_status == 2 & immune_response ==  2 ~ "No",
                                                        TRUE ~ as.character("Prefer not to answer"))),
                                    "Yes", "No", "Prefer not to answer"),
    
    immune_response_cat = fct_relevel(as_factor(case_when(immune_response == 1 ~ "Yes",
                                                          immune_response == 2 ~ "No",
                                                          immune_response == 3 ~ "Don't know",
                                                          TRUE ~ as.character(NA))),
                                      "Yes", "No","Don't know"),
    
    sex_work_cat = fct_relevel(as_factor(case_when(sex_work == 1 ~ "Yes",
                                                   sex_work == 2 ~ "No",
                                                   sex_work == 3 ~ "Prefer not to answer",
                                                   TRUE ~ as.character(NA))),
                               "Yes", "No", "Prefer not to answer"),
    
    #STIs
    sti_gonorrhea = case_when(sti_case___1 == 1 | sti_control___1 == 1 ~ 1,
                              TRUE ~ as.numeric(0)),
    
    sti_chlamydia = case_when(sti_case___2 == 1 | sti_control___2 == 1 ~ 1,
                              TRUE ~ as.numeric(0)),
    
    sti_syphilis = case_when(sti_case___3 == 1 | sti_control___3 == 1 ~ 1,
                             TRUE ~ as.numeric(0)),
    
    sti_other = case_when(sti_case___4 == 1 | sti_control___4 == 1 ~ 1,
                          TRUE ~ as.numeric(0)),
    
    sti_none = case_when(sti_case___5 == 1 | sti_control___5 == 1 ~ 1,
                         TRUE ~ as.numeric(0)),
    
    sti_any = ifelse(if_any(c("sti_gonorrhea", "sti_chlamydia",
                              "sti_syphilis", "sti_other"), ~ . == 1), 1, 0),
    
    sti_sum = rowSums(across(c(sti_gonorrhea, sti_chlamydia,
                               sti_syphilis, sti_other))),
    
    sti_sum_cat = fct_relevel(as_factor(case_when(sti_sum >= 2 ~ "\u22652",
                                                  TRUE ~ as.character(sti_sum))),
                              "0", "1", "\u22652")
    
    # close ------
    
  ) %>% 
  arrange(index_date) %>%
  
  mutate(la_mhfs = str_detect(pid, "MHFS"),
         index_yrmo = format_ISO8601(index_date, precision = "ym"),
         
         index_epiyr   = epiyear(index_date),
         
         index_epiwk   = epiweek(index_date),
         
         index_epiyrwk_4 = fct_inorder(as_factor(case_when(index_epiyr == 2022 &
                                                             index_epiwk %in% c(33, 34, 35, 36)~ "2022: 33-36",
                                                           index_epiyr == 2022 &
                                                             index_epiwk %in% c(37, 38, 39, 40)~ "2022: 37-40",
                                                           index_epiyr == 2022 &
                                                             index_epiwk %in% c(41, 42, 43, 44)~ "2022: 41-44",
                                                           index_epiyr == 2022 &
                                                             index_epiwk %in% c(45, 46, 47, 48)~ "2022: 45-48",
                                                           index_epiyr == 2022 &
                                                             index_epiwk %in% c(49, 50, 51, 52)~ "2022: 49-52",
                                                           index_epiyr == 2023 &
                                                             index_epiwk %in% c(1, 2, 3, 4)  ~  "2023: 1-4",
                                                           index_epiyr == 2023 &
                                                             index_epiwk %in% c(5, 6, 7, 8)  ~  "2023: 5-8",
                                                           index_epiyr == 2023 &
                                                             index_epiwk %in% c(9, 10, 11, 12)  ~ "2023: 9-12",
                                                           index_epiyr == 2023 &
                                                             index_epiwk %in% c(13, 14, 15, 16)  ~ "2023: 13-16"))))%>%
  select(pid, sid, jid, everything(), -redcap_data_access_group)

# tt <- mpoxve %>%
#   filter(minset_vars == "Complete") %>%
#   pfreq(partially_adminroute_mod, partially_adminroute_cat, vax_stat_ve_main)
# 
# 
# 
# tt <- mpoxve %>%
#   filter(minset_vars == "Complete") %>%
#   pfreq(vax_stat_ve_main, fully_adminroute_cat, partially_adminroute_cat)
# Case and control matching by state and within 4-weeks of index event (IE) date ----------------------------------------------

#==== Resources ----
#https://stackoverflow.com/questions/75185184/matching-controls-and-cases-using-some-conditions
#https://cran.r-project.org/web/packages/MatchIt/MatchIt.pdf

#==== set seed ----
set.seed(20230320)

#==== subset by models based on exposure status ------------------
mpoxve_main <- mpoxve %>% 
  filter(minset_vars == "Complete")  %>% 
  drop_na(caco, vax_stat_ve_main, age, race_ethn, immunocompromised,
          close_contact_dx_cat, vax_stat_ve_main, vax_stat_ve_s6full)

mpoxve_s6excl <- mpoxve %>% 
  filter(minset_vars == "Complete") %>% 
  drop_na(caco, vax_stat_ve_s6excl, age, race_ethn, immunocompromised,
          close_contact_dx_cat)

mpoxve_roa_partial <- mpoxve %>% 
  filter(minset_vars == "Complete") %>% 
  drop_na(caco, partially_adminroute_mod, age, race_ethn, immunocompromised,
          close_contact_dx_cat)

mpoxve_roa_full <- mpoxve %>% 
  filter(minset_vars == "Complete") %>% 
  drop_na(caco, fully_adminroute_mod, age, race_ethn, immunocompromised,
          close_contact_dx_cat)

#==== Output match dataset for 1:4 match_ratio: main model & s6 full vaccinated ----

# IE (caliper)  & State (Exact)
m_data_r4 <- match.data(matchit(I(caco == "Case") ~ index_date, 
                                data = mpoxve_main,
                                exact = ~ sid,
                                caliper = c(index_date = 28), std.caliper = FALSE,
                                distance = "euclidean", ratio = 4),
                        subclass = "matched_id") %>% 
  mutate(match_ratio = "1 case:4 controls",
         match_vars = "State & IE",
         model = "Main")

#==== Output match dataset for 1:4 match_ratio: s6 excluded ----
# IE (caliper)  & State (Exact)
m_data_r4_s6excl <- match.data(matchit(I(caco == "Case") ~ index_date, 
                                       data = mpoxve_s6excl,
                                       exact = ~ sid,
                                       caliper = c(index_date = 28), std.caliper = FALSE,
                                       distance = "euclidean", ratio = 4),
                               subclass = "matched_id") %>% 
  mutate(match_ratio = "1 case:4 controls",
         match_vars = "State & IE",
         model = "s6excl")


#==== Output match dataset for 1:4 match_ratio: ROA Partially Vaccinated----
# IE (caliper)  & State (Exact)
m_data_r4_roa_partial <- match.data(matchit(I(caco == "Case") ~ index_date, 
                                            data = mpoxve_roa_partial,
                                            exact = ~ sid,
                                            caliper = c(index_date = 28), std.caliper = FALSE,
                                            distance = "euclidean", ratio = 4),
                                    subclass = "matched_id") %>% 
  mutate(match_ratio = "1 case:4 controls",
         match_vars = "State & IE",
         model = "roa_partial")

#==== Output match dataset for 1:4 match_ratio: ROA Partially Vaccinated----
# IE (caliper)  & State (Exact)
m_data_r4_roa_full <- match.data(matchit(I(caco == "Case") ~ index_date, 
                                         data = mpoxve_roa_full,
                                         exact = ~ sid,
                                         caliper = c(index_date = 28), std.caliper = FALSE,
                                         distance = "euclidean", ratio = 4),
                                 subclass = "matched_id") %>% 
  mutate(match_ratio = "1 case:4 controls",
         match_vars = "State & IE",
         model = "roa_full")

#==== Flag exact match IDs ----

subclass_4 <- levels(m_data_r4$matched_id)[table(m_data_r4$matched_id) == 5]
m_data_r4_exact <- m_data_r4[m_data_r4$matched_id %in% subclass_4,]%>% 
  mutate(match_ratio_exact = 1)

subclass_4_s6excl  <- levels(m_data_r4_s6excl$matched_id)[table(m_data_r4_s6excl$matched_id) == 5]
m_data_r4_exact_s6excl <- m_data_r4_s6excl[m_data_r4_s6excl$matched_id %in% subclass_4_s6excl,]%>% 
  mutate(match_ratio_exact = 1)

subclass_4_roa_partial  <- levels(m_data_r4_roa_partial$matched_id)[table(m_data_r4_roa_partial$matched_id) == 5]
m_data_r4_exact_roa_partial <- m_data_r4_roa_partial[m_data_r4_roa_partial$matched_id %in% subclass_4_roa_partial,]%>% 
  mutate(match_ratio_exact = 1)

subclass_4_roa_full  <- levels(m_data_r4_roa_full$matched_id)[table(m_data_r4_roa_full$matched_id) == 5]
m_data_r4_exact_roa_full <- m_data_r4_roa_full[m_data_r4_roa_full$matched_id %in% subclass_4_roa_full,]%>% 
  mutate(match_ratio_exact = 1)

#==== combine the data to create analysis dataset ----------------------------------

mpoxve_matched <-  mpoxve_main %>% 
  mutate(match_ratio = "None",
         match_vars = "None",
         model = "Main") %>% 
  bind_rows((mpoxve_roa_partial %>% 
               mutate(match_ratio = "None",
                      match_vars = "None",
                      model = "roa_partial")),
            (mpoxve_roa_full %>% 
               mutate(match_ratio = "None",
                      match_vars = "None",
                      model = "roa_full")),
            (mpoxve_s6excl %>% 
               mutate(match_ratio = "None",
                      match_vars = "None",
                      model = "s6excl")),
            m_data_r4, m_data_r4_roa_partial, 
            m_data_r4_roa_full, m_data_r4_s6excl) %>%
  mutate(match_ratio = fct_inorder(as_factor(match_ratio)),
         match_vars = fct_inorder(as_factor(match_vars)),
         model = fct_inorder(as_factor(model)),
         match_ratio_exact = case_when(((model ==  "Main" & match_ratio == "1 case:4 controls" &
                                           match_vars == "State & IE" &
                                           matched_id %in% subclass_4)|
                                          (model ==  "s6excl" & match_ratio == "1 case:4 controls" &
                                             match_vars == "State & IE" &
                                             matched_id %in% subclass_4_s6excl)) ~ 1,
                                       TRUE ~ as.numeric(0))) %>% 
  rename(match_weights = weights) %>% 
  select(model, match_ratio, match_vars, match_ratio_exact, pid, caco, matched_id, match_weights,
         index_date, vax_stat_ve_main,
         sid, jid, age, age_cat, race_ethn_8_cat, race_ethn, elig_gender_cat, health_ins_cat,
         housing_cat,sex_work_cat, hiv_status_cat, hiv_cd4_cat, hiv_meds_2moredays, hiv_prep_cat,
         immune_response_cat, immunocompromised, close_contact_dx_cat, sex_part_num_cat,
         sti_gonorrhea, sti_chlamydia, sti_syphilis, sti_other, sti_any, sti_sum_cat,
         fully_adminroute_cat, fully_adminroute_mod,
         partially_adminroute_cat, partially_adminroute_mod, 
         vax_stat_ve_s6full, vax_stat_ve_s6excl,
         everything()) %>% 
  arrange(model, match_ratio, match_vars, matched_id, caco)


#==== summary of matched data ----

r_upto<- mpoxve_matched %>%
  pfreq(model, match_ratio, match_vars, #jid,
        caco) %>%
  mutate(match_data = case_when(match_ratio == "None" ~ "None",
                                TRUE ~ as.character("Upto")))

r_exact <- mpoxve_matched %>%
  filter(match_ratio_exact == 1) %>% 
  pfreq(model, match_ratio, match_vars, #jid,
        caco) %>%
  mutate(match_data = "Exact")

caco_match_counts <- bind_rows(r_upto, r_exact) %>% 
  select(model, match_ratio, match_vars, match_data, #jid,
         everything()) %>% 
  mutate(match_data = fct_relevel(as_factor(match_data),
                                  "None", "Exact", "Upto"),
         model_match_ratio_vars_data = fct_relevel(as_factor(str_c(model, match_ratio, 
                                                                   match_vars, match_data, sep = "_")))) %>% 
  arrange(model_match_ratio_vars_data, caco) %>% 
  select(-model_match_ratio_vars_data)

caco_match_counts_r <- caco_match_counts %>%
  filter(match_ratio!= "None") %>% 
  full_join((caco_match_counts %>%
               filter(match_ratio== "None") %>% 
               ungroup() %>% 
               rename(tot = n) %>% 
               select(model, #jid,
                      caco, tot)), 
            by = c("model", #"jid",
                   "caco")) %>% 
  mutate(pct_tot = round(100*(n/tot), 1)) %>% 
  filter(match_ratio == "1 case:4 controls" & match_vars == "State & IE" & match_data == "Upto") %>% 
  ungroup() %>% 
  select(model, #jid,
         caco, n, tot, pct_tot) %>% 
  mutate(model = fct_inorder(case_when(model == "Main" ~ "M1-S6 partial",
                                       model == "roa_partial" ~ "M-ROA Partial",
                                       model == "roa_full" ~ "M-ROA Full",
                                       TRUE ~ as.character("M2-S6 excl")))) %>% 
  rename(Model = model, `Case status` = caco, 
         `N matched` = n, `N complete` =  tot,
         Percent = pct_tot) %>% 
  arrange(Model, #jid,
          `Case status`)

clipr::write_clip(caco_match_counts_r)
# Save analysis data ------------------------------------------------------------------

#==== MMWR analysis ----
saveRDS(mpoxve_matched, "data/analysis_mpoxve_mmwr.RDS")

write.csv(mpoxve_matched, "data/analysis_mpoxve_mmwr.csv", na = "")



#==== subset by models based on exposure status ------------------
mpoxve_main <- mpoxve %>% 
  filter(minset_vars == "Complete")  %>% 
  drop_na(caco, vax_stat_ve_main, age, race_ethn, immunocompromised,
          close_contact_dx_cat, vax_stat_ve_main, vax_stat_ve_s6full)


mpoxve %>% 
  filter(jid == "LAC") %>% 
  pfreq(minset_vars, caco)

library(naniar)
mpoxve %>% 
  filter(minset_vars == "Complete") %>%
  filter(jid == "CA") %>% 
  select(vax_stat_ve_main, age, race_ethn, immunocompromised,
         close_contact_dx_cat)%>%
  gg_miss_upset()

#==== Homelessness/sex work in mpox vax case control study ----    
  # In addition to meeting the inclusion criteria, respondents needed
  # to have a complete set of minimum data elements to calculate unadjusted
  # vaccine effectiveness. These data elements were the index event date, 
  # vaccination status, and case status. The special populations dataset is
  # limited to those with a complete set of minimum data elements.

  # spop <- mpoxve_main %>% 
  #   left_join((m_data_r4 %>% 
  #                select(pid,
  #                       matched_id)), by = "pid") %>% 
  #   select(pid, caco, matched_id,
  #          vax_stat_ve_main, index_date,
  #          jid, sid, age, age_cat, race_ethn_8_cat, race_ethn, elig_gender_cat, health_ins_cat,
  #          housing_cat, sex_work_cat, hiv_status_cat, hiv_cd4_cat, hiv_cd4_200_cat, hiv_meds_2moredays, hiv_prep_cat,
  #          immune_response_cat, immunocompromised, close_contact_dx_cat, sex_part_num_cat,
  #          sti_gonorrhea, sti_chlamydia, sti_syphilis, sti_other, sti_any, sti_sum_cat,
  #          everything(), 
  #          # Variables excluded
  #          -eligibility_date,-redcap_trans_date,-mmwr_data_closing_date,-study_pd_int,
  #          -test_yr,-symptoms_yr,-clinic_date_mod,-self_clinic_yr,-caco_stat_cat,
  #          -caco_sen,-d1_yr,-d2_yr,-vax1_yrmo,-vax2_yrmo,
  #          -vax_stat_na,-vax_stat_acip,-vax_stat,-vax_stat_ve,
  #          -vax_stat_ve_acip,-vax_stat_ve_s6full,-vax_stat_ve_s6excl,-pep_stat_ve,
  #          -la_mhfs,-starts_with("mr_"),
  #          -fully_adminroute_cat, -fully_adminroute_mod,
  #          -partially_adminroute_cat, -partially_adminroute_mod) %>% 
  # arrange(matched_id, caco, pid)
  # 
  # 
  # saveRDS(spop, "data/spop/spop_mpox_ve.RDS")
  # 
  # write.csv(spop, "data/spop/spop_mpox_ve.csv", na = "")



# tt <- create_dictionary(spop)
# 
# clipr::write_clip(tt)

