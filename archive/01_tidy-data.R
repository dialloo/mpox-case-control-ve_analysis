#=========================================================================================
# Author(s): Alpha Oumar Diallo
# Collaborators: MPX-VE Case-Control Team - Leora Feldstein, Alex Dalton,
#                Dani Moulia, Anna Chard, Nick Deputy, Amy Fothergill, and Jurisdictions
# Program: 00_REDCap-API-linkage
# Project: MPX Multi-Jurisdictional Vaccine Effectiveness Case-Control Evaluation 
# Objective: Evaluate the effectiveness of the JYNNEOS vaccine in preventing symptomatic
#           mpox disease among 18 to 49-year-old gay, bisexual, and other men who have 
#           sex with men, as well as transgender, non-binary, and gender-diverse persons. 
# Task: tidy data in preparations for data quality check
# Data in: Pulled data directly from REDCap using API token
# Initial program date: 2022-12-15
# Last modified date: 2022-12-19
#===========================================================================================

# Load packages -----------------------------------------------------------------
  pacman::p_load(here,         # file paths relative to R project root folder
                 tidyverse,    # data management and visualization
                 purrr,        # iteration
                 janitor,      # data management: cleaning and exploring data
                 lubridate,    # working with dates/epiweeks
                 naniar,       # Working with data structures, summaries, and visualizations for Missing Data
                 arsenal,      # provides unctions for large-scale statistical summaries
                 ggsci,        # scientific journal and sci-fi themed color palettes for 'ggplot2'
                 cowplot       # streamlined plot theme and plot annotations for 'ggplot2'
  )



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


# Source data ---------------------------------------------------------------------

  source("~/GitHub/analysis_mpx-case-control/00_REDCap-API-linkage.R")

# Tidy data -------------------------
  
 
  mpoxve <- raw %>%
    # Restrict to eligible participants (i.e., those that passed the eligibility screening)
    filter(inelig_stop ==  1) %>% 
    mutate(# Format the all yes/no variables by giving values of 2 a zero and all check box variables as 1(Yes)/2(No)
           across(contains(c("elig_age", "elig_gender_msm", "elig_sexual_partners",
                             "elig_healthcare", "first_dose_yesno", "second_dose_yesno",
                             "symptoms_yesno", "provider_dx", "clinic_yesno", "hospitalized",
                             "dose1_yesno", "dose2_yesno"), ignore.case = TRUE), give_two_a_zero),
           
           # Check the presence of a jurisdiction ID
           juris_detect = case_when(str_detect(pid, "CA_|CO_| CT_|DC_|GA_|LAC_|MD_|MN_|NYC_|NYS_|OR_|TN_") ~ 1,
                           TRUE ~ as.numeric(0)),
             #! On 2022-12-19, there was 9 records (1 in GA [#59] & 8 in TN) without a jurisdiction ID,
             #                 I contacted TN in early December and GA on 2022-12-19 to append ID the next they share data.
             #                 TN has already address the issue based on results from my comparison of records that were missing
             #                 and those that include the IDs using R script 'archived codes'. After archiving the data on 
             #                 here (Case Control > Data management and analysis > data_archive > MultiJurisdictionalM_DATA_2022-12-19_1108),
             #                 I deleted the records from Have deleted on REDCap.
           
           # rename REDCap DAG variable
           jid = toupper(redcap_data_access_group),
           
           # Case Ascertainment: Symptomatic cases identified by state or local health departments who meet either
           #                     the confirmed or probable case definition will be eligible for inclusion. 
           #                     #! Based on the case ascertainment description, we should use the jurisdiction 
           #                     #! case classification. When there is a discrepancy, we should follow-up with the
           #                     #! with he jurisdiction to confirm. 
           
           # Flag those who reported having experienced mpox symptoms and specified one of the listed symptoms 
           symptom_type_complete = case_when(symptoms_yesno == 1 &
                                               (symptom_type___1 == 1 | symptom_type___2 == 1 | symptom_type___3 == 1 |
                                                  symptom_type___4 == 1 | symptom_type___5 == 1 | symptom_type___6 == 1 |
                                                  symptom_type___7 == 1 | symptom_type___8 == 1 | symptom_type___9 == 1 |
                                                  symptom_type___10 == 1 | symptom_type___11 == 1 | symptom_type___12 == 1 |
                                                  symptom_type___13 == 1 | symptom_type___14 == 1 | symptom_type___15 == 1 |
                                                  symptom_type___16 == 1) ~ 1,
                                                symptoms_yesno == 1 &
                                                  (symptom_type___1 == 1 & symptom_type___2 == 1 & symptom_type___3 == 1 &
                                                     symptom_type___4 == 1 & symptom_type___5 == 1 & symptom_type___6 == 1 &
                                                     symptom_type___7 == 1 & symptom_type___8 == 1 & symptom_type___9 == 1 &
                                                     symptom_type___10 == 1 & symptom_type___11 == 1 & symptom_type___12 == 1 &
                                                     symptom_type___13 == 1 & symptom_type___14 == 1 & symptom_type___15 == 1 &
                                                     symptom_type___16 == 1) ~ 0,
                                                TRUE ~ as.numeric(NA)),
           
           # Create variable for case/control classification by self- or site-report consistency checks. 
           ca_co = fct_relevel(as_factor(case_when(case_yesno %in% c(1,2) & provider_dx == 1 ~ "Case: self & site",
                                                   is.na(case_yesno)  & provider_dx == 1 ~ "Case: self but site missing",
                                                   case_yesno == 3 & provider_dx == 0 ~ "Control: self & site",
                                                   is.na(case_yesno)  & provider_dx == 0 ~ "Control: self but site missing",
                                                   case_yesno %in% c(1,2) & provider_dx == 0 ~ "Discrepancy: site case",
                                                   case_yesno == 3 & provider_dx == 1 ~ "Discrepancy: self case",
                                                   TRUE ~ as.character(NA))),
                              "Case: self & site", "Case: self but site missing",
                              "Control: self & site", "Control: self but site missing",
                              "Discrepancy: site case", "Discrepancy: self case"),
           
           # Flag cases with/without symptoms; Everyone who said that they had mpox symptoms, specified at least one of the options.
           ca_co_symp = fct_relevel(as_factor(case_when(ca_co %in% c("Case: self & site", "Case: self but site missing") &
                                                          symptoms_yesno == 1  ~ "Case: symptom +",
                                                        ca_co %in% c("Case: self & site", "Case: self but site missing") &
                                                          symptoms_yesno == 0  ~ "Case: symptom -",
                                                        ca_co %in% c("Control: self & site", "Control: self but site missing") ~ "Control",
                                                TRUE ~ as.character(NA))),
                                    "Case: symptom +", "Case: symptom -",
                                    "Control"),
           
           # Format all date variables as YYYY-MM-DD
           across(contains("_date", ignore.case = TRUE), ymd),
           
           # Flag mpox symptoms date within 2022-08-15 and system date. 
              #! Note that I am using Aug 15, rather Aug 19 per the protocol because
              #! jurisdictions have report discrepancies between the self-reported symptoms date
              #! on the survey versus clinic records. 
           timepoint_interval <- interval(ymd("2022-08-15"), ymd(Sys.Date())),
           symp_clinic_date_met = case_when((symptoms_date %within% timepoint_interval) |
                                              (clinic_date %within% timepoint_interval) ~ 1,
                                            TRUE ~ as.numeric(0))
            ) %>% 
    select(pid, jid, everything(), -redcap_data_access_group)


# Confirm the number of cases  & Controls -----------------------------------------
  # Although jurisdictions classify participants as a case or control, we can also 
  # classify them as a case or control using the self reported data. We may also need 
  # drop participants (controls) who passed the eligibility screener but did not have an visit at an 
  # HIV PrEP or STI clinic. 
 

  aa <- mpoxve %>%
    pfreq(jid, ca_co, case_yesno, provider_dx, symptoms_yesno)
  
  bb <- mpoxve %>%
    pfreq(jid, ca_co, symptoms_yesno, symptom_type_complete)
  
  cc <- mpoxve %>%
    pfreq(symptom_type_complete, symptoms_yesno, symptom_type___1, symptom_type___2, symptom_type___3,
            symptom_type___4, symptom_type___5, symptom_type___6,
            symptom_type___7, symptom_type___8, symptom_type___9,
            symptom_type___10, symptom_type___11, symptom_type___12,
            symptom_type___13, symptom_type___14, symptom_type___15,
            symptom_type___16)
  
  dd <- mpoxve %>%
    #select(jid, case, case_yesno, symptoms_yesno, provider_dx) %>% 
    pfreq(jid, ca_co_symp, ca_co, symptoms_yesno, symptom_type_complete)
  
  ee <- mpoxve %>%
    #select(jid, case, case_yesno, symptoms_yesno, provider_dx) %>% 
    pfreq(ca_co_symp, ca_co)
  
  ee <- mpoxve %>%
    #select(jid, case, case_yesno, symptoms_yesno, provider_dx) %>% 
    pfreq(jid, ca_co, ca_co_symp, symp_clinic_date_met, symptoms_date, test_result_date, clinic_date, control_visit_date)
  
# Confirm the jurisdictions submitting data -------------------------
  
  
  
  
  
    