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

# Jurisdiction ID for TN -------------------------------------------------------
# Used data archived data on 2022-12-19 (Case Control > Data management and analysis > data_archive > MultiJurisdictionalM_DATA_2022-12-19_1108)
# and the code below to compare records with and without the jurisdiction ID. TN addressed the issue but this
# created duplicate records. After confirming the records are duplicated, I (AOD) dropped these records from REDCap.

      tn_missing <- mpoxve %>% 
        filter(juris_detect == 0 & redcap_data_access_group == "tn") %>% 
        mutate(pid = str_c("TN_", pid, sep = ""))%>% 
        select(-juris_detect)
      
      
      tn_corrected <- mpoxve %>% 
        filter(pid %in% c("TN_101", "TN_104", "TN_111", "TN_134",
                          "TN_161", "TN_177", "TN_183", "TN_187") == T) %>% 
        select(-juris_detect)
      
      
      diff_corrected_vs_missing <- anti_join(tn_corrected, tn_missing)    
      
      summary(comparedf(tn_corrected, tn_missing))
