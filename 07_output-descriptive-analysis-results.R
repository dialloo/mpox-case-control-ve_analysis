#========================================================================================================
# Author: Alpha Oumar Diallo (YQL8@cdc.gov)
# CDC Collaborators: Anna N Chard, Alexandra F Dalton, Nicholas P Deputy, Leora R Feldstein, 
#                    Amy Fothergill (UYL0@cdc.gov) and Danielle L Moulia 
#                    as part of the Case-Control Unit|VE Team|VTF|2022 Multinational Monkeypox Response
# Program: 07_output-descriptive-analysis-results
# Project: Multijurisdictional Mpox JYNNEOS Vaccine effectiveness Case Control Study
# Main Objectives: 
#            1. Evaluate the effectiveness of the JYNNEOS vaccine in preventing symptomatic mpox disease 
#               among 18-49 years old gay, bisexual, and other men who have sex with men, as well as 
#               transgender persons residing in the US.
#            2. Estimate effectiveness by immunocompromising conditions and route of administmatch_ration
# Task: Output descriptive restults 
# Data in: analysis_mpoxve_mmwr.RDS
# Initial program date: 2023-03-29
# Last modified date: 2023-03-29
#==========================================================================================================


# Load packages & helper functions --------------------------------
pacman::p_load(here,        # file paths relative to R project root folder
               rio,         # import/export
               readxl,      # read excel files
               tidyverse,   # data management and visualization
               data.table,  # Provides a high-performance version of base R's data.frame
               tableone     # Study participant descriptive statistics
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


# Helper function to generate study participants descriptions.
gen_tbl1 <- function(df, tbl_strata = tbl_strata) {
  
  # Crude ------------------------------
  tbl_crude <- CreateTableOne(data = df,                           
                              vars = tbl_vars,
                              strata = tbl_strata, 
                              factorVars = tbl_factorvars)
  
  tbl_crude_print <- print(tbl_crude, showAllLevels = F, quote = FALSE, smd = TRUE,
                           noSpaces = TRUE, test = TRUE, printToggle = FALSE,
                           nonnormal = tbl_nonnormal, formatOptions = list(big.mark = ",")) %>% 
    as.data.frame() %>% rownames_to_column(var = "covariates")  
    #rename(lower_crude = Lower, higher_crude = `Higher (ref.)`, smd_crude = SMD,p_crude = p) %>%
    #select(covariates, lower_crude, higher_crude, smd_crude, p_crude, test)
  
  return(tbl_crude_print)
  #https://stats.stackexchange.com/questions/472421/how-to-calculate-standardized-mean-difference-after-matching
  #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3144483/#s11title
  
}



# Load analysis dataset and output template ----------------------------------------------------------------------

  mpoxve <- here("data","analysis_mpoxve_mmwr.RDS") %>%  
    read_rds()  

  tbl1 <- here("results_output", "tbl_output_template.xlsx") %>% 
    read_excel(sheet = "Table 1")
  
  tbl2 <- here("results_output", "tbl_output_template.xlsx") %>% 
    read_excel(sheet = "Table 2")
# Prep descriptive vars ----------------------------------------------------------------------

  cov_names <- names(mpoxve %>% 
                       # Select the variables for table 1:
                       select(# Outcome: case status
                                caco,
                              
                              # Covariates included in table 1
                                age_cat, race_ethn, elig_gender_cat, health_ins_cat,
                                housing_cat, sex_work_cat, hiv_status_cat, hiv_cd4_cat, 
                                hiv_cd4_200_cat, hiv_meds_2moredays,
                                hiv_prep_cat, immunocompromised, close_contact_dx_cat, 
                                sex_part_num_cat, sti_gonorrhea, sti_chlamydia, sti_syphilis, sti_other, 
                                sti_any,
                              
                              # Exposure: vaccination status based on models
                              vax_stat_ve_main, fully_adminroute_cat, partially_adminroute_cat,
                              
                              # Study variables
                              jid, index_epiyrwk_4
                                                     ))
  
  tbl_vars <- cov_names
  
  tbl_factorvars <- tbl_vars
  
  tbl_nonnormal <- c("age")
  
  tbl1_strata <- c("caco")
  
  tbl2_strata <- c("vax_stat_ve_main")

# Generate and output table 1 characteristics ---------------------------------------------------------------------  
  

  tbl_characteristic <- mpoxve %>%
    group_by(model, match_ratio) %>%
    nest() %>%
    rename(df = data) %>%
    mutate(tbl_characteristic =
             map(df, ~.x %>% gen_tbl1(tbl_strata = tbl1_strata))) %>% 
    select(model, match_ratio, tbl_characteristic) %>% 
    unnest(cols = c(model, match_ratio, tbl_characteristic)) %>% 
    filter((model == "s6excl") == FALSE) %>% 
    mutate(dataset = case_when(model == "Main" &
                                 match_ratio == "None"  ~ "m0",
                               model == "Main" &
                                 match_ratio == "1 case:4 controls"  ~ "m1",
                                TRUE ~ as.character(NA))) %>% 
    ungroup() %>% 
    filter(!is.na(dataset)) %>% 
    select(-test, -SMD, -model, -match_ratio) %>% 
    pivot_wider(id_cols = covariates,
                names_from = dataset,
                values_from = c(Case, Control, p)) 
  
  #clipr::write_clip(tbl_characteristic)
  
  tbl_characteristic_out <- tbl1 %>% 
      left_join(tbl_characteristic, by = c("covariates")) %>% 
      select(characteristics, covariates,
             Case_m1, Control_m1, p_m1, Case_m0, Control_m0, p_m0)
   
  clipr::write_clip(tbl_characteristic_out)
    
  write.csv(tbl_characteristic_out, file = "results_output/tbl_characteristic.csv", na = "")
  


# Generate and output table 2 characteristics ---------------------------------------------------------------------  
  
  
  tbl2_characteristic <- mpoxve %>%
    filter((model == "Main" &
             match_ratio == "None") == F) %>% 
    group_by(model) %>%
    nest() %>%
    rename(df = data) %>%
    mutate(tbl_characteristic =
             map(df, ~.x %>% gen_tbl1(tbl_strata = tbl2_strata))) %>% 
    select(model, tbl_characteristic) %>% 
    unnest(cols = c(model, tbl_characteristic)) %>% 
    filter((model == "s6excl") == FALSE)  %>% 
    ungroup() %>% 
    select(-test, -SMD, -model)
  
  
  tbl2_characteristic_out <- tbl2 %>% 
    left_join(tbl2_characteristic, by = c("covariates")) %>% 
    select(characteristics, covariates, everything())
  
  #clipr::write_clip(tbl2_characteristic_out)
  
  tbl2_caco <- mpoxve %>%
    filter((model == "Main" &
              match_ratio == "None") == F) %>% 
    filter((model == "s6excl") == F) %>% 
    pfreq(vax_stat_ve_main, caco)
  
  write.csv(tbl_characteristic_out, file = "results_output/tbl_characteristic.csv")
  
  
  
  