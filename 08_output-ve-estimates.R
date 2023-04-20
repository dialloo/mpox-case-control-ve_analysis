#========================================================================================================
# Author: Alpha Oumar Diallo (YQL8@cdc.gov)
# CDC Collaborators: Anna N Chard, Alexandra F Dalton, Nicholas P Deputy, Leora R Feldstein, 
#                    Amy Fothergill (UYL0@cdc.gov) and Danielle L Moulia 
#                    as part of the Case-Control Unit|VE Team|VTF|2022 Multinational Monkeypox Response
# Program: 08_output-ve-estimates
# Project: Multijurisdictional Mpox JYNNEOS Vaccine effectiveness Case Control Study
# Main Objectives: 
#            1. Evaluate the effectiveness of the JYNNEOS vaccine in preventing symptomatic mpox disease 
#               among 18-49 years old gay, bisexual, and other men who have sex with men, as well as 
#               transgender persons residing in the US.
#            2. Estimate effectiveness by immunocompromising conditions and route of administmatch_ration
# Task: Output VE estimates 
# Data in: analysis_mpoxve_mmwr.RDS
# Initial program date: 2023-03-29
# Last modified date: 2023-04-05
#==========================================================================================================


# Load packages & helper functions --------------------------------
pacman::p_load(here,        # file paths relative to R project root folder
               rio,         # import/export
               readxl,      # read excel files
               tidyverse,   # data management and visualization
               stringr,     # manipulate text strings 
               purrr,       # loop over objects in a tidy way
               data.table,  # Provides a high-performance version of base R's data.frame
               tableone,    # Study participant descriptive statistics
               survival,    # conditional logistic regression
               sjplot,      # output model result HTML
               modelr,      # Provides functions that help you create elegant pipelines when modelling
               gtsummary,   # summary statistics and tests 
               broom,       # tidy up results from regressions
               lmtest,      # likelihood-ratio tests
               parameters,  # alternative to tidy up results from regressions
               see,         # alternative to visualise forest plots
               easystats

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


# Load analysis dataset and output template ----------------------------------------------------------------------

mpoxve <- here("data","analysis_mpoxve_mmwr.RDS") %>%  
  read_rds() %>% 
  filter((match_ratio == "1 case:4 controls") == TRUE)  %>%
  rename(vax_main = vax_stat_ve_main,
         vax_s6full = vax_stat_ve_s6full,
         vax_s6excl = vax_stat_ve_s6excl) %>% 
  mutate(# Reverse factor levels
         vax_main = fct_rev(vax_main),
         vax_s6full = fct_rev(vax_s6full),
         vax_s6excl = fct_rev(vax_s6excl),
         
         # Turn variables into numerics
         caco = case_when(caco == "Control" ~ 0,
                          TRUE ~ as.numeric(1)),
         
         vax_main_mod = case_when(vax_main == "Unvaccinated" ~ 0,
                                  vax_main == "Partially vaccinated" ~ 1,
                                  vax_main == "Fully vaccinated" ~ 2,
                                  TRUE ~ as.numeric(NA)),
         
         vax_s6full_mod = case_when(vax_s6full == "Unvaccinated" ~ 0,
                                    vax_s6full == "Partially vaccinated" ~ 1,
                                    vax_s6full == "Fully vaccinated" ~ 2,
                                    TRUE ~ as.numeric(NA)),
         
         vax_s6excl_mod = case_when(vax_s6excl == "Unvaccinated" ~ 0,
                                    vax_s6excl == "Partially vaccinated" ~ 1,
                                    vax_s6excl == "Fully vaccinated" ~ 2,
                                    TRUE ~ as.numeric(NA)),
         
         immunocompromised_mod = case_when(immunocompromised == "No" ~ 0,
                                       immunocompromised == "Yes" ~ 1,
                                       TRUE ~ as.numeric(NA)),
         
         close_contact_dx_mod = case_when(close_contact_dx_cat == "No" ~ 0,
                                          close_contact_dx_cat == "Yes" ~ 1,
                                          close_contact_dx_cat == "Don't know" ~ 2,
                                          TRUE ~ as.numeric(NA))
         )


  tbl3 <- here("results_output", "tbl_output_template.xlsx") %>% 
    read_excel(sheet = "Table 3")

# Main VE estimates ------------------------------------------------------------------

  analysis_main <- mpoxve %>% 
    filter(model == "Main") 

  mod_unadj_main <- clogit(caco ~ vax_main,
                            strata(matched_id),
                            data = analysis_main)
  
  mod_adj_main <- clogit(caco ~ vax_main + age + race_ethn +  immunocompromised +
                            close_contact_dx_cat,
                          strata(matched_id),
                          data = analysis_main)
  
  mod_tidy <- bind_rows(tidy(mod_unadj_main) %>% mutate(model = "M1-s6 partial",
                                                          stratified = "None", adj = "Crude"),
                         tidy(mod_adj_main) %>% mutate(model = "M1-s6 partial",
                                                        stratified = "None", adj = "Adjusted"))
  caco_main <- analysis_main %>% 
    group_by(caco, vax_main) %>% 
    summarise(n = n()) %>% 
    mutate(model = "M1-s6 partial",
           stratified = "None") %>% 
    rename(term = vax_main)
# Stratified VE estimates: immunocompromising conditions ------------------------------------------------------------------
  
  #==== immunocompromising conditions: Yes ----
    analysis_ic_yes <- mpoxve %>% 
      filter(model == "Main" & immunocompromised == "Yes")
    
    mod_unadj_ic_yes <- clogit(caco ~ vax_main,
                                strata(matched_id),
                                data = analysis_ic_yes)
    
    mod_adj_ic_yes <- clogit(caco ~ vax_main + age + race_ethn +
                                close_contact_dx_cat,
                              strata(matched_id),
                              data = analysis_ic_yes)
    
    mod_ic_yes_tidy <- bind_rows(tidy(mod_unadj_ic_yes) %>% mutate(model = "M1-s6 partial",
                                                                     stratified = "IC-Yes", adj = "Crude"),
                                  tidy(mod_adj_ic_yes) %>% mutate(model = "M1-s6 partial",
                                                                   stratified = "IC-Yes", adj = "Adjusted"))
    # tab_model(mod_unadj_fullroute, mod_adj_fullroute,  show.aic = T, show.obs = T)
    
    caco_ic_yes <- analysis_ic_yes %>% 
      group_by(caco, vax_main) %>% 
      summarise(n = n())%>% 
      mutate(model = "M1-s6 partial",
             stratified = "IC-Yes") %>% 
      rename(term = vax_main)
  #==== immunocompromising conditions: No ----
    analysis_ic_no <- mpoxve %>% 
      filter(model == "Main" & immunocompromised == "No")
    
    mod_unadj_ic_no <- clogit(caco ~ vax_main,
                                strata(matched_id),
                                data = analysis_ic_no)
    
    mod_adj_ic_no <- clogit(caco ~ vax_main + age + race_ethn +
                                close_contact_dx_cat,
                              strata(matched_id),
                              data = analysis_ic_no)
    
    mod_ic_no_tidy <- bind_rows(tidy(mod_unadj_ic_no) %>% mutate(model = "M1-s6 partial",
                                                                     stratified = "IC-No", adj = "Crude"),
                                  tidy(mod_adj_ic_no) %>% mutate(model = "M1-s6 partial",
                                                                   stratified = "IC-No", adj = "Adjusted"))
    caco_ic_no <- analysis_ic_no %>% 
      group_by(caco, vax_main) %>% 
      summarise(n = n())%>% 
      mutate(model = "M1-s6 partial",
             stratified = "IC-No") %>% 
      rename(term = vax_main)
# Stratified VE estimates: route of administration ------------------------------------------------------------------

  #==== Fully vaccinated ----
    analysis_full_route <- mpoxve %>% 
      filter(model == "roa_full")
    
    mod_unadj_fullroute <- clogit(caco ~ fully_adminroute_mod,
                              strata(matched_id),
                              data = analysis_full_route)
    
    mod_adj_fullroute <- clogit(caco ~ fully_adminroute_mod + age + race_ethn +
                                immunocompromised +  close_contact_dx_cat,
                              strata(matched_id),
                              data = analysis_full_route)
    
    mod_fullroute_tidy <- bind_rows(tidy(mod_unadj_fullroute) %>% mutate(model = "M1-s6 partial",
                                                                   stratified = "ROA-Fully", adj = "Crude"),
                                 tidy(mod_adj_fullroute) %>% mutate(model = "M1-s6 partial",
                                                                 stratified = "ROA-Fully", adj = "Adjusted"))
    
    caco_fullroute <- analysis_full_route %>% 
      group_by(caco, fully_adminroute_mod) %>% 
      summarise(n = n())%>% 
      mutate(model = "M1-s6 partial",
             stratified = "ROA-Fully") %>% 
      rename(term = fully_adminroute_mod)
  
  #==== Partially vaccinated ----
   analysis_partial_route <- mpoxve %>% 
      filter(model == "roa_partial")
    
    
    mod_unadj_partialroute <- clogit(caco ~ partially_adminroute_mod,
                                   strata(matched_id),
                                   data = analysis_partial_route)
    
    
    mod_adj_partialroute <- clogit(caco ~ partially_adminroute_mod + age + race_ethn +
                                   immunocompromised +  close_contact_dx_cat,
                                 strata(matched_id),
                                 data = analysis_partial_route)
    
    mod_partroute_tidy <- bind_rows(tidy(mod_unadj_partialroute) %>% mutate(model = "M1-s6 partial",
                                                                           stratified = "ROA-Partially", adj = "Crude"),
                                     tidy(mod_adj_partialroute) %>% mutate(model = "M1-s6 partial",
                                                                         stratified = "ROA-Partially", adj = "Adjusted"))
    
    caco_partroute <- analysis_partial_route %>% 
      group_by(caco, partially_adminroute_mod) %>% 
      summarise(n = n())%>% 
      mutate(model = "M1-s6 partial",
             stratified = "ROA-Partially") %>% 
      rename(term = partially_adminroute_mod)

# Sensitivity Analysis: restricting to S6 excluded ------------------------------------------------------------------

    analysis_s6excl <- mpoxve %>%
      filter(model == "s6excl")

    # caco_s6excl <- analysis_s6excl %>% 
    #   group_by(caco, vax_main) %>% 
    #   summarise(n = n())%>% 
    #   mutate(model == "s6excl",
    #          stratified = "None") %>% 
    #   rename(term = vax_main)
    
  # ==== Sen: M2-s6 as partially vaccinated ----
    mod1_unadj <- clogit(caco ~ vax_main,
                              strata(matched_id),
                              data = analysis_s6excl)

    mod1_adj <- clogit(caco ~ vax_main + age + race_ethn +  immunocompromised +
                              close_contact_dx_cat,
                            strata(matched_id),
                            data = analysis_s6excl)

    mod1_tidy <- bind_rows(tidy(mod1_unadj) %>% mutate(model = "Sen: M1-s6 partial",
                                                            stratified = "None", adj = "Crude"),
                           tidy(mod1_adj) %>% mutate(model = "Sen: M1-s6 partial",
                                                          stratified = "None", adj = "Adjusted"))

  # ==== Sen: M2-s6 as fully vaccinated  ----
    mod2_unadj <- clogit(caco ~ vax_s6full,
                              strata(matched_id),
                              data = analysis_s6excl)
    
    mod2_adj <- clogit(caco ~ vax_s6full + age + race_ethn +  immunocompromised +
                              close_contact_dx_cat,
                            strata(matched_id),
                            data = analysis_s6excl)
    
    mod2_tidy <- bind_rows(tidy(mod2_unadj) %>% mutate(model = "Sen: M2-s6 full",
                                                            stratified = "None", adj = "Crude"),
                           tidy(mod2_adj) %>% mutate(model = "Sen: M2-s6 full",
                                                          stratified = "None", adj = "Adjusted"))
    
    
  # ==== Sen: M3-s6 excluded ----
    mod3_unadj <- clogit(caco ~ vax_s6excl,
                         strata(matched_id),
                         data = analysis_s6excl)
    
    mod3_adj <- clogit(caco ~ vax_s6excl + age + race_ethn +  immunocompromised +
                         close_contact_dx_cat,
                       strata(matched_id),
                       data = analysis_s6excl)
    
    mod3_tidy <- bind_rows(tidy(mod3_unadj) %>% mutate(model = "Sen: M2-s6 excl",
                                                       stratified = "None", adj = "Crude"),
                           tidy(mod3_adj) %>% mutate(model = "Sen: M2-s6 excl",
                                                     stratified = "None", adj = "Adjusted"))

    
# Interaction Analysis: immunocompromising conditions -------------------------------------
  # No interaction:  mod_adj_main
    
  #==== Interaction model ----
    # mod_adj_interaction <- clogit(caco ~ immunocompromised*vax_main + age + race_ethn +
    #                          close_contact_dx_cat,
    #                        strata(matched_id),
    #                        data = analysis_main)
    # 
    # mod_adj_interaction_tidy <- tidy(mod_adj_interaction)
    # 
    # lmtest::lrtest(mod_adj_main, mod_adj_interaction)
    
    #Based on the p-value there are no differences between the main and interaction model fits.
# Combine model results ------------------------------------------------------------------
  
  #==== VE estimates ----
  ve_tidy <- bind_rows (mod_tidy,
                        mod_ic_yes_tidy, mod_ic_no_tidy,
                        mod_fullroute_tidy, mod_partroute_tidy,
                        mod1_tidy, mod2_tidy, mod3_tidy) %>% 
      filter(term %in% c ("vax_mainFully vaccinated", 
                          "vax_mainPartially vaccinated",
                          "vax_s6fullFully vaccinated", 
                          "vax_s6fullPartially vaccinated",
                          "vax_s6exclFully vaccinated", 
                          "vax_s6exclPartially vaccinated",
                          "fully_adminroute_modSubcutaneous",
                          "fully_adminroute_modIntradermal",
                          "fully_adminroute_modHeterologous",
                          "partially_adminroute_modSubcutaneous",
                          "partially_adminroute_modIntradermal")) %>% 
      mutate(term = fct_inorder(as_factor(case_when(term %in% c("vax_mainFully vaccinated",
                                          "vax_s6fullFully vaccinated",
                                          "vax_s6exclFully vaccinated") ~ "Fully vaccinated", 
                              term %in% c("vax_mainPartially vaccinated",
                                          "vax_s6fullPartially vaccinated",
                                          "vax_s6exclPartially vaccinated") ~ "Partially vaccinated",
                              term %in% c("fully_adminroute_modSubcutaneous",
                                          "partially_adminroute_modSubcutaneous") ~ "Subcutaneous",
                              term %in% c("fully_adminroute_modIntradermal", 
                                          "partially_adminroute_modIntradermal") ~ "Intradermal",
                              term == "fully_adminroute_modHeterologous" ~ "Heterologous"))),
             model = fct_inorder(as_factor(model)),
             stratified = fct_inorder(as_factor(stratified)),
             adj = fct_inorder(as_factor(adj)),
             est = 100*(1-(exp(estimate))),
             lcl = 100*(1-(exp(estimate + 1.96*std.error))),
             ucl = 100*(1-(exp(estimate - 1.96*std.error))),
             ci95 = paste0("(", format(round(lcl, 1), nsmall =1), "\u2013",  format(round(ucl, 1), nsmall =1), ")"),
             ve_ci95 = str_trim(str_c(format(round(est, 1), nsmall =1), ci95, sep = " ")),
             p.value_formated = format(round(p.value, 3), nsmall = 3),
             pvalue = case_when(p.value <0.0001 ~ "<0.001",
                                TRUE ~ as.character(p.value_formated))) %>% 
      select(model, stratified, adj, term, ve_ci95, pvalue, everything()) 
    
    ve_tidy_tbl <- tbl3 %>% 
      left_join((ve_tidy %>% 
                   select(model, stratified, adj, term, ve_ci95) %>% 
                   pivot_wider(id_cols = c(model, stratified, term),
                               names_from = adj,
                               values_from = ve_ci95)),
                by = c("model", "stratified", "term")) 
    
    clipr::write_clip(ve_tidy_tbl)
    
  #==== Case control countes estimates ----
    
    caco_tidy <- bind_rows (caco_main, caco_ic_yes, caco_ic_no,
                            caco_fullroute, caco_partroute) %>% 
      filter(!term %in% c("Don't know", "Unvaccinated"))%>% 
      mutate(caco = fct_relevel(as_factor(case_when(caco == 0 ~ "Control",
                                                    TRUE ~ as.character("Case"))),
                                "Case", "Control"),
             model = fct_inorder(as_factor(model)),
             stratified = fct_inorder(as_factor(stratified))) %>% 
      pivot_wider(id_cols = c(model, stratified, term),
                  names_from = caco,
                  values_from = n)%>% 
      select(model, stratified, term, Case, Control)
    
    ve_tidy_tbl2 <- ve_tidy_tbl %>% 
      left_join(caco_tidy, c("model", "stratified", "term")) %>% 
      select(model, stratified, term, Case, Control, everything())
    
    clipr::write_clip(ve_tidy_tbl2)
    
    
    
# Save VE estimates for figures later ------------------------------------------------------------------

  saveRDS(ve_tidy, file = "results_output/tbl_ve-results.RDS")
  write.csv(ve_tidy, file = "results_output/tbl_ve-results.csv")

# Examine the negative VE issue for ROA partially vaccinated issue ------------------------------------------------------------------
  
  tt <- analysis_partial_route %>%
    #filter(sid == "GA") %>% 
    select(pid, matched_id, caco, vax_main, partially_adminroute_mod,
           age, race_ethn, immunocompromised, close_contact_dx_cat) %>% 
    arrange(matched_id, pid, caco)
  
  
  
    