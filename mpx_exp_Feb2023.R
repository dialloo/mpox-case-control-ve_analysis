##Call in data, call it raw
#run top of Oumar's code to get packages+ functions



#There are multiple individuals reporting multiple races categorize;
exp_catdata <- raw %>% 
 
  mutate(
    
    #can also just add these four into the code that does this in the main step
    
    across(contains(c("contact_mpx_dx_case", "contact_mpx_dx_control", "contact_mpx_symp_case", "contact_mpx_symp_control"), ignore.case = TRUE), give_two_a_zero),
    
    #categorical race variable
    race_cat = fct_relevel(as_factor(case_when(
                     (race___1 == 1 & race___2 == 0 & race___3 == 0 & race___4 == 0 & race___5 == 0 & race___6 == 0) ~ "African American or Black",
                     (race___1 == 0 & race___2 == 1 & race___3 == 0 & race___4 == 0 & race___5 == 0 & race___6 == 0) ~ "White", 
                     (race___1 == 0 & race___2 == 0 & race___3 == 1 & race___4 == 0 & race___5 == 0 & race___6 == 0) ~ "Asian",
                     (race___1 == 0 & race___2 == 0 & race___3 == 0 & race___4 == 1 & race___5 == 0 & race___6 == 0) ~ "Native Hawaiin/Pacific Islander", 
                     (race___1 == 0 & race___2 == 0 & race___3 == 0 & race___4 == 0 & race___5 == 1 & race___6 == 0) ~ "American Indian/Alaska Native", 
                     (race___1 == 0 & race___2 == 0 & race___3 == 0 & race___4 == 0 & race___5 == 0 & race___6 == 1) ~ "Prefer not to answer",
                     (race___1 == 1 | race___2 == 1 | race___3 == 1 | race___4 == 1 | race___5 == 1 | race___6 == 1) ~ "Multiracial",
                      TRUE ~ NA_character_)), 
                     "African American or Black", "White", "Asian", "Native Hawaiin/Pacific Islander", "American Indian/Alaska Native", "Prefer not to answer",
                     "Multiracial"),


    #race ethnicity
    #ethnicity (1, Hispanic or Latino | 2, Not Hispanic or Latino | 3, Prefer not to answer)
    race_ethnicity = fct_relevel(as_factor(case_when(
                                          (ethnicity ==1 & race_cat == "African American or Black") ~ "Hispanic, Black",
                                          (ethnicity ==1 & race_cat == "White") ~ "Hispanic, White",
                                          (ethnicity ==1 & race_cat == "Asian") ~ "Hispanic, Asian", 
                                          (ethnicity ==1 & race_cat == "Native Hawaiin/Pacific Islander")  ~ "Hispanic, NH/PI",
                                          (ethnicity ==1 & race_cat == "American Indian/Alaska Native")  ~ "Hispanic, AI/AN",
                                          (ethnicity ==1 & race_cat == "Prefer not to answer")  ~ "Hispanic, declined race",
                                          (ethnicity ==1 & race_cat == "Multiracial") ~ "Hispanic, Multiracial", 
                                          (ethnicity ==2 & race_cat == "African American or Black") ~ "Non-Hispanic, Black",
                                          (ethnicity ==2 & race_cat == "White") ~ "Non-Hispanic, White",
                                          (ethnicity ==2 & race_cat == "Asian") ~ "Non-Hispanic, Asian", 
                                          (ethnicity ==2 & race_cat == "Native Hawaiin/Pacific Islander")  ~ "Non-Hispanic, NH/PI",
                                          (ethnicity ==2 & race_cat == "American Indian/Alaska Native")  ~ "Non-Hispanic, AI/AN",
                                          (ethnicity ==2 & race_cat == "Prefer not to answer")  ~ "Non-Hispanic, declined race",
                                          (ethnicity ==2 & race_cat == "Multiracial") ~ "Non-Hispanic, Multiracial", 
                                          (ethnicity ==3 & race_cat == "African American or Black") ~ "NS, Black",
                                          (ethnicity ==3 & race_cat == "White") ~ "NS, White",
                                          (ethnicity ==3 & race_cat == "Asian") ~ "NS, Asian", 
                                          (ethnicity ==3 & race_cat == "Native Hawaiin/Pacific Islander") ~ "NS, NH/PI",
                                          (ethnicity ==3 & race_cat == "American Indian/Alaska Native") ~ "NS, AI/AN", 
                                          (ethnicity ==3 & race_cat == "Prefer not to answer") ~ "NS, declined race",
                                          (ethnicity ==3 & race_cat == "Multiracial") ~ "NS, Multiracial", 
                                           TRUE ~ NA_character_)), 
                                          "Hispanic, Black", "Hispanic, White","Hispanic, Asian", "Hispanic, NH/PI", "Hispanic, AI/AN", "Hispanic, declined race", "Hispanic, Multiracial", 
                                          "Non-Hispanic, Black", "Non-Hispanic, White", "Non-Hispanic, Asian", "Non-Hispanic, NH/PI", "Non-Hispanic, AI/AN", "Non-Hispanic, declined race",  "Non-Hispanic, Multiracial", 
                                           "NS, Black", "NS, White", "NS, Asian", "NS, NH/PI", "NS, AI/AN", "NS, declined race", "NS, Multiracial"),
    

                                           
#If needed, re-group individuals who said "other" (16) to symptom, right now (06Feb) the others (13) do not overlap with other symptom types
    
    #signs or symptoms experienced with recent illness - ;
    symptoms_count = symptom_type___1 + symptom_type___2 + symptom_type___3 + symptom_type___4 + symptom_type___5 + symptom_type___6 +
                    symptom_type___7 + symptom_type___8 + symptom_type___9 + symptom_type___10 + symptom_type___11 + symptom_type___12 +
                    symptom_type___13 + symptom_type___14 + symptom_type___15 + symptom_type___16,
    
    
    #create a variable that is a yes/no indicator of having more than one symptom
    symptoms_count_ge1 = fct_relevel(as_factor(case_when(
      (symptoms_count >= 1) ~ "One or more symptoms",
      (symptoms_count == 0) ~ "No symptoms reported", 
      TRUE ~ NA_character_)), 
      "No symptoms reported", "One or more symptoms"),

    #create a variable that is a yes/no indicator of having more than one symptom
    symptoms_count_3lvl = fct_relevel(as_factor(case_when(
        (symptoms_count == 1) ~ "One symptom",
        (symptoms_count >= 2) ~ "Two or more symptoms",
        (symptoms_count == 0) ~ "No symptoms reported", 
        TRUE ~ NA_character_)), 
        "No symptoms reported", "One symptom", "Two or more symptoms"),

    #create a variable that is a categorical counts of symptoms
    symptoms_count_cat = fct_relevel(as_factor(case_when(
      (symptoms_count == 0) ~ "No symptoms reported", 
      (symptoms_count >= 1 & symptoms_count <=3) ~ "One to 3 symptoms",
      (symptoms_count >= 4 & symptoms_count <=6) ~ "Four to 6 symptoms",
      (symptoms_count >= 7 & symptoms_count <=9) ~ "7 to 9 symptoms",
      (symptoms_count >= 10 ) ~ "10 or more symptoms",
      TRUE ~ NA_character_)), 
      "No symptoms reported", "One to 3 symptoms", "Four to 6 symptoms", "7 to 9 symptoms","10 or more symptoms" ),


      #create a variable that is a categorical counts of symptoms
      symptoms_count_ge6 = fct_relevel(as_factor(case_when(
        (symptoms_count == 0) ~ "No symptoms reported", 
        (symptoms_count == 1) ~ "One symptom",
        (symptoms_count == 2) ~ "Two symptoms",
        (symptoms_count == 3) ~ "Three symptom2",
        (symptoms_count == 4) ~ "Four symptoms",
        (symptoms_count == 5) ~ "Five symptoms",
        (symptoms_count >= 6 ) ~ "6 or more symptoms",
        TRUE ~ NA_character_)), 
        "No symptoms reported", "One symptom", "Two symptoms", "Three symptom2",  "Four symptoms", "Five symptoms", "6 or more symptoms"),




#If needed, re-group individuals who said "other" (13) to rash location, right now (06Feb) the others (20 observations) do overlap with other symptom types - am working on regrouping these

  #Count of rash/lesion locations;
  rashlesion_loc_count = lesion_location___1 + lesion_location___2 + lesion_location___3 + lesion_location___4 + lesion_location___5 + lesion_location___6 + lesion_location___7 +
                         lesion_location___8 + lesion_location___9 + lesion_location___10 + lesion_location___11 + lesion_location___12 + lesion_location___13, 


  #create a variable that is a yes/no indicator of having more than one rash/lesion location
  rashlesion_loc_count_ge1 = fct_relevel(as_factor(case_when(
    (rashlesion_loc_count >= 1) ~ "One or more rash/lesion locations",
    (rashlesion_loc_count == 0) ~ "No rash or lesion locations reported", 
    TRUE ~ NA_character_)), 
    "No rash or lesion locations reported", "One or more rash/lesion locations"),

  
    #create a variable that is a yes/no indicator of having more than one rash/lesion location
    rashlesion_count_3lvl = fct_relevel(as_factor(case_when(
      (rashlesion_loc_count == 1) ~ "One rash/lesion location",
      (rashlesion_loc_count >= 2) ~ "Two or more rash/lesion locations",
      (rashlesion_loc_count == 0) ~ "No rash/lesion locations reported", 
      TRUE ~ NA_character_)), 
      "No rash/lesion locations reported", "One rash/lesion location", "Two or more rash/lesion locations"),
    
    #create a variable that is a categorical counts of symptoms
    rashlesion_count_cat = fct_relevel(as_factor(case_when(
      (rashlesion_loc_count == 0) ~ "No rash/lesion locations reported", 
      (rashlesion_loc_count >= 1 & rashlesion_loc_count <=3) ~ "One to 3 rash/lesion locations",
      (rashlesion_loc_count >= 4 & rashlesion_loc_count <=6) ~ "Four to 6 rash/lesion locations",
      (rashlesion_loc_count >= 7 & rashlesion_loc_count <=9) ~ "7 to 9 rash/lesion locations",
      (rashlesion_loc_count >= 10 ) ~ "10 or more rash/lesion locations",
      TRUE ~ NA_character_)), 
      "No rash/lesion locations reported", "One to 3 rash/lesion locations", "Four to 6 rash/lesion locations",  "7 to 9 rash/lesion locations", "10 or more rash/lesion locations"),
      
      #create a variable that is a categorical counts of symptoms
      rashlesion_count_cat2 = fct_relevel(as_factor(case_when(
        (rashlesion_loc_count == 0) ~ "No rash/lesion locations reported", 
        (rashlesion_loc_count >= 1 & rashlesion_loc_count <=10) ~ "One to 10 rash/lesion locations",
        (rashlesion_loc_count >= 11 & rashlesion_loc_count <=99) ~ "11 to 99 rash/lesion locations",
        (rashlesion_loc_count >= 100 ) ~ "100 or more rash/lesion locations",
       TRUE ~ NA_character_)), 
      "No rash/lesion locations reported", "One to 10 rash/lesion locations", "11 to 99 rash/lesion locations", "100 or more rash/lesion locations" ),



    #####################################################
    #Non intimate contact with someone with mpx symptoms;
    
    nonint_sympcontact_cat = fct_relevel(as_factor(case_when(
      (mpx_symp_nonintimate___1 == 1 & mpx_symp_nonintimate___2 == 0 & mpx_symp_nonintimate___3 == 0 & mpx_symp_nonintimate___4 == 0 & mpx_symp_nonintimate___5 == 0 & mpx_symp_nonintimate___6 == 0 & mpx_symp_nonintimate___7 == 0) ~ "Provided in-home care",
      (mpx_symp_nonintimate___1 == 0 & mpx_symp_nonintimate___2 == 1 & mpx_symp_nonintimate___3 == 0 & mpx_symp_nonintimate___4 == 0 & mpx_symp_nonintimate___5 == 0 & mpx_symp_nonintimate___6 == 0 & mpx_symp_nonintimate___7 == 0) ~ "Shared food, utensils, or dishes", 
      (mpx_symp_nonintimate___1 == 0 & mpx_symp_nonintimate___2 == 0 & mpx_symp_nonintimate___3 == 1 & mpx_symp_nonintimate___4 == 0 & mpx_symp_nonintimate___5 == 0 & mpx_symp_nonintimate___6 == 0 & mpx_symp_nonintimate___7 == 0) ~ "Shared towels, bedding or clothing",
      (mpx_symp_nonintimate___1 == 0 & mpx_symp_nonintimate___2 == 0 & mpx_symp_nonintimate___3 == 0 & mpx_symp_nonintimate___4 == 1 & mpx_symp_nonintimate___5 == 0 & mpx_symp_nonintimate___6 == 0 & mpx_symp_nonintimate___7 == 0) ~ "Shared drug equipment", 
      (mpx_symp_nonintimate___1 == 0 & mpx_symp_nonintimate___2 == 0 & mpx_symp_nonintimate___3 == 0 & mpx_symp_nonintimate___4 == 0 & mpx_symp_nonintimate___5 == 1 & mpx_symp_nonintimate___6 == 0 & mpx_symp_nonintimate___7 == 0) ~ "Face to face contact", 
      (mpx_symp_nonintimate___1 == 0 & mpx_symp_nonintimate___2 == 0 & mpx_symp_nonintimate___3 == 0 & mpx_symp_nonintimate___4 == 0 & mpx_symp_nonintimate___5 == 0 & mpx_symp_nonintimate___6 == 1 & mpx_symp_nonintimate___7 == 0) ~ "Other, non-intimate contact",
      (mpx_symp_nonintimate___1 == 0 & mpx_symp_nonintimate___2 == 0 & mpx_symp_nonintimate___3 == 0 & mpx_symp_nonintimate___4 == 0 & mpx_symp_nonintimate___5 == 0 & mpx_symp_nonintimate___6 == 0 & mpx_symp_nonintimate___7 == 1) ~ "No non-intimate contac",
      (mpx_symp_nonintimate___1 == 1 | mpx_symp_nonintimate___2 == 1 | mpx_symp_nonintimate___3 == 1 | mpx_symp_nonintimate___4 == 1 | mpx_symp_nonintimate___5 == 1 | mpx_symp_nonintimate___6 == 1 & mpx_symp_nonintimate___7 == 1) ~ "Multiple non-intimate contacts",
      TRUE ~ NA_character_)), 
      "Provided in-home care","Shared food, utensils, or dishes", "Shared towels, bedding or clothing", "Shared drug equipment","Face to face contact", "Other, non-intimate contact","No non-intimate contact", "Multiple non-intimate contacts"),
    
    
    #create a variable that is a count of non-intimate contacts (with someone with mpx symptoms)
   nonint_sympcontact_count = mpx_symp_nonintimate___1 +mpx_symp_nonintimate___2 +mpx_symp_nonintimate___3 +mpx_symp_nonintimate___4 + mpx_symp_nonintimate___5 +
                         mpx_symp_nonintimate___6+ mpx_symp_nonintimate___7,
   
   #create a variable that is a count of intimate contacts (with someone with mpx symptoms)
   int_sympcontact_count = mpx_symp_intimate___1 + mpx_symp_intimate___2 + mpx_symp_intimate___3 + mpx_symp_intimate___4 + mpx_symp_intimate___5 + mpx_symp_intimate___6 +
                       mpx_symp_intimate___7 + mpx_symp_intimate___8 + mpx_symp_intimate___9 + mpx_symp_intimate___10 + mpx_symp_intimate___11+ mpx_symp_intimate___12,


    #create a variable that is a yes/no indicator of having more than one non-intimate contact (with someone with mpx symptoms)
   nonint_sympcontact_ge1 = fct_relevel(as_factor(case_when(
     (nonint_sympcontact_count >= 1) ~ "One or more non-intimate contacts",
       (nonint_sympcontact_count == 0) ~ "No non-intimate contacts", 
     TRUE ~ NA_character_)), 
     "No non-intimate contacts","One or more non-intimate contacts"),
    
    #create a 3-level variable that is an indicator of having more than one non-intimate contact (with someone with mpx symptoms)
    nonint_sympcontact_3lvl = fct_relevel(as_factor(case_when(
      (nonint_sympcontact_count == 1) ~ "One non-intimate contact",
      (nonint_sympcontact_count == 0) ~ "No non-intimate contacts",
      (nonint_sympcontact_count >= 2) ~ "Two or more non-intimate contacts",
      TRUE ~ NA_character_)), 
      "No non-intimate contacts","One non-intimate contact", "Two or more non-intimate contacts"),
    
   
   #create a variable that is a yes/no indicator of having more than one non-intimate contact (with someone with mpx symptoms)
   int_sympcontact_ge1 = fct_relevel(as_factor(case_when(
     (int_sympcontact_count >= 1) ~ "One or more intimate contacts",
     (int_sympcontact_count == 0) ~ "No intimate contacts", 
     TRUE ~ NA_character_)), 
     "No intimate contacts","One or more intimate contacts"),
   
    
    #create a  3-level variable that is an indicator of having more than one intimate contact (with someone with mpx symptoms)
    int_sympcontact_3lvl = fct_relevel(as_factor(case_when(
      (int_sympcontact_count == 1) ~ "One non-intimate contact",
      (int_sympcontact_count == 0) ~ "No non-intimate contacts",
      (int_sympcontact_count >= 2) ~ "Two or more non-intimate contacts",
      TRUE ~ NA_character_)), 
      "No non-intimate contacts","One non-intimate contact", "Two or more non-intimate contacts"),


## Create a combined contact count + yes/no variable that lumps together the above individually reported contacts (with someone with mpx symptoms); 


  all_sympcontact_count = nonint_sympcontact_count + int_sympcontact_count,

  all_sympcontact_ge1 = fct_relevel(as_factor(case_when(
    (all_sympcontact_count >= 1) ~ "One or more contacts",
    (all_sympcontact_count == 0) ~ "No contacts", 
    TRUE ~ NA_character_)), 
    "No contacts","One or more contacts"),

  #create a  3-level variable that is an indicator of having more than one contact (with someone with mpx symptoms)
  all_sympcontact_3lvl = fct_relevel(as_factor(case_when(
    (all_sympcontact_count == 1) ~ "One contact",
    (all_sympcontact_count == 0) ~ "No contacts",
    (all_sympcontact_count >= 2) ~ "Two or more contacts",
    TRUE ~ NA_character_)), 
    "No contacts","One contact", "Two or more contacts"),





#####################################################
# Repeate contacts w/ someone who was diagnosed with mpx (vs. symptoms consistent w/mpx)
#####################################################

#Non intimate contact with someone DIAGNOSED with mpx;

  nonint_dxcontact_cat = fct_relevel(as_factor(case_when(
    (mpx_dx_nonintimate___1 == 1 & mpx_dx_nonintimate___2 == 0 & mpx_dx_nonintimate___3 == 0 & mpx_dx_nonintimate___4 == 0 & mpx_dx_nonintimate___5 == 0 & mpx_dx_nonintimate___6 == 0 & mpx_dx_nonintimate___7 == 0) ~ "Provided in-home care",
    (mpx_dx_nonintimate___1 == 0 & mpx_dx_nonintimate___2 == 1 & mpx_dx_nonintimate___3 == 0 & mpx_dx_nonintimate___4 == 0 & mpx_dx_nonintimate___5 == 0 & mpx_dx_nonintimate___6 == 0 & mpx_dx_nonintimate___7 == 0) ~ "Shared food, utensils, or dishes", 
    (mpx_dx_nonintimate___1 == 0 & mpx_dx_nonintimate___2 == 0 & mpx_dx_nonintimate___3 == 1 & mpx_dx_nonintimate___4 == 0 & mpx_dx_nonintimate___5 == 0 & mpx_dx_nonintimate___6 == 0 & mpx_dx_nonintimate___7 == 0) ~ "Shared towels, bedding or clothing",
    (mpx_dx_nonintimate___1 == 0 & mpx_dx_nonintimate___2 == 0 & mpx_dx_nonintimate___3 == 0 & mpx_dx_nonintimate___4 == 1 & mpx_dx_nonintimate___5 == 0 & mpx_dx_nonintimate___6 == 0 & mpx_dx_nonintimate___7 == 0) ~ "Shared drug equipment", 
    (mpx_dx_nonintimate___1 == 0 & mpx_dx_nonintimate___2 == 0 & mpx_dx_nonintimate___3 == 0 & mpx_dx_nonintimate___4 == 0 & mpx_dx_nonintimate___5 == 1 & mpx_dx_nonintimate___6 == 0 & mpx_dx_nonintimate___7 == 0) ~ "Face to face contact", 
    (mpx_dx_nonintimate___1 == 0 & mpx_dx_nonintimate___2 == 0 & mpx_dx_nonintimate___3 == 0 & mpx_dx_nonintimate___4 == 0 & mpx_dx_nonintimate___5 == 0 & mpx_dx_nonintimate___6 == 1 & mpx_dx_nonintimate___7 == 0) ~ "Other, non-intimate contact",
    (mpx_dx_nonintimate___1 == 0 & mpx_dx_nonintimate___2 == 0 & mpx_dx_nonintimate___3 == 0 & mpx_dx_nonintimate___4 == 0 & mpx_dx_nonintimate___5 == 0 & mpx_dx_nonintimate___6 == 0 & mpx_dx_nonintimate___7 == 1) ~ "No non-intimate contac",
    (mpx_dx_nonintimate___1 == 1 | mpx_dx_nonintimate___2 == 1 | mpx_dx_nonintimate___3 == 1 | mpx_dx_nonintimate___4 == 1 | mpx_dx_nonintimate___5 == 1 | mpx_dx_nonintimate___6 == 1 & mpx_dx_nonintimate___7 == 1) ~ "Multiple non-intimate contacts",
    TRUE ~ NA_character_)), 
    "Provided in-home care","Shared food, utensils, or dishes", "Shared towels, bedding or clothing", "Shared drug equipment","Face to face contact", "Other, non-intimate contact","No non-intimate contact", "Multiple non-intimate contacts"),
  
  
  #create a variable that is a count of non-intimate contacts (with someone with mpx dxtoms)
  nonint_dxcontact_count = mpx_dx_nonintimate___1 +mpx_dx_nonintimate___2 +mpx_dx_nonintimate___3 +mpx_dx_nonintimate___4 + mpx_dx_nonintimate___5 +
    mpx_dx_nonintimate___6+ mpx_dx_nonintimate___7,
  
  #create a variable that is a count of intimate contacts (with someone with mpx dxtoms)
  int_dxcontact_count = mpx_dx_intimate___1 + mpx_dx_intimate___2 + mpx_dx_intimate___3 + mpx_dx_intimate___4 + mpx_dx_intimate___5 + mpx_dx_intimate___6 +
    mpx_dx_intimate___7 + mpx_dx_intimate___8 + mpx_dx_intimate___9 + mpx_dx_intimate___10 + mpx_dx_intimate___11+ mpx_dx_intimate___12,
  
  
  #create a variable that is a yes/no indicator of having more than one non-intimate contact (with someone with mpx dxtoms)
  nonint_dxcontact_ge1 = fct_relevel(as_factor(case_when(
    (nonint_dxcontact_count >= 1) ~ "One or more non-intimate contacts",
    (nonint_dxcontact_count == 0) ~ "No non-intimate contacts", 
    TRUE ~ NA_character_)), 
    "No non-intimate contacts","One or more non-intimate contacts"),
  
  #create a 3-level variable that is an indicator of having more than one non-intimate contact (with someone with mpx dxtoms)
  nonint_dxcontact_3lvl = fct_relevel(as_factor(case_when(
    (nonint_dxcontact_count == 1) ~ "One non-intimate contact",
    (nonint_dxcontact_count == 0) ~ "No non-intimate contacts",
    (nonint_dxcontact_count >= 2) ~ "Two or more non-intimate contacts",
    TRUE ~ NA_character_)), 
    "No non-intimate contacts","One non-intimate contact", "Two or more non-intimate contacts"),
  
  
  #create a variable that is a yes/no indicator of having more than one non-intimate contact (with someone with mpx dxtoms)
  int_dxcontact_ge1 = fct_relevel(as_factor(case_when(
    (int_dxcontact_count >= 1) ~ "One or more intimate contacts",
    (int_dxcontact_count == 0) ~ "No intimate contacts", 
    TRUE ~ NA_character_)), 
    "No intimate contacts","One or more intimate contacts"),
  
  
  #create a  3-level variable that is an indicator of having more than one intimate contact (with someone with mpx dxtoms)
  int_dxcontact_3lvl = fct_relevel(as_factor(case_when(
    (int_dxcontact_count == 1) ~ "One non-intimate contact",
    (int_dxcontact_count == 0) ~ "No non-intimate contacts",
    (int_dxcontact_count >= 2) ~ "Two or more non-intimate contacts",
    TRUE ~ NA_character_)), 
    "No non-intimate contacts","One non-intimate contact", "Two or more non-intimate contacts"),
  
  
  ## Create a combined contact count + yes/no variable that lumps together the above individually reported contacts (with someone with mpx dxtoms); 
  
  
  all_dxcontact_count = nonint_dxcontact_count + int_dxcontact_count,
  
  all_dxcontact_ge1 = fct_relevel(as_factor(case_when(
    (all_dxcontact_count >= 1) ~ "One or more contacts",
    (all_dxcontact_count == 0) ~ "No contacts", 
    TRUE ~ NA_character_)), 
    "No contacts","One or more contacts"),
  
  #create a  3-level variable that is an indicator of having more than one contact (with someone with mpx dxtoms)
  all_dxcontact_3lvl = fct_relevel(as_factor(case_when(
    (all_dxcontact_count == 1) ~ "One contact",
    (all_dxcontact_count == 0) ~ "No contacts",
    (all_dxcontact_count >= 2) ~ "Two or more contacts",
    TRUE ~ NA_character_)), 
    "No contacts","One contact", "Two or more contacts"),
  


  ##Combined any contact with person with symptoms consistent w/ mpx or diagnosed w/ mpx
  all_sympdxcontact = all_dxcontact_count + all_sympcontact_count, 

  all_sympdxcontact_ge1 = fct_relevel(as_factor(case_when(
    (all_sympdxcontact >= 1) ~ "One or more contacts with symptomatic or diagnosed w/mpx",
    (all_sympdxcontact == 0) ~ "No contacts", 
    TRUE ~ NA_character_)), 
    "One or more contacts with symptomatic or diagnosed w/mpx","One or more contacts"),



  #Using the 4 yes/no REDCap questions about contacts, create an overall summary variable
  any_contact_confprob = fct_relevel (as_factor(case_when(
    (contact_mpx_dx_case ==1 |contact_mpx_dx_control ==1 | contact_mpx_symp_case==1 |contact_mpx_symp_control ==1) ~ "Yes, contact with confirmed or probable case", 
    (contact_mpx_dx_case == 0 & contact_mpx_dx_control == 0 & contact_mpx_symp_case== 0 & contact_mpx_symp_control ==0 ) ~ "No, no contact with confirmed or probable case", 
    TRUE ~ NA_character_)), 
    "No, no contact with confirmed or probable case", "Yes, contact with confirmed or probable case"),



  )%>%   

    
    #rename race variables to be more intuitive;
    rename (race_africanamerican_black = race___1,
            race_white = race___2,
            race_asian = race___3,
            race_NHPI = race___4,
            race_AIAN = race___5,
            race_noanswer = race___6,


    #rename the sympsoms to be more intuitive;
            symp_fever = symptom_type___1, 
            symp_rashlesion = symptom_type___2, 
            symp_lymph = symptom_type___3, 
            symp_itch = symptom_type___4, 
            symp_rectalpain = symptom_type___5, 
            symp_retalbleed = symptom_type___6,
            symp_bloodstool = symptom_type___7, 
            symp_rectalswell = symptom_type___8, 
            symp_urgentbm = symptom_type___9, 
            symp_headache = symptom_type___10, 
            symp_malaise = symptom_type___11,  
            symp_conjunctiv = symptom_type___12,
            symp_vomit = symptom_type___13, 
            symp_mylagia = symptom_type___14,  
            symp_chills = symptom_type___15, 
            symp_other = symptom_type___16,
  
    #rename the original mpx_symp_contact variables to be more informative
            exp_inhomecare = mpx_symp_nonintimate___1, 
            exp_shareddishes = mpx_symp_nonintimate___2,
            exp_sharedlinens = mpx_symp_nonintimate___3 ,
            exp_shareddrugeq = mpx_symp_nonintimate___4 ,
            exp_ftfcontact = mpx_symp_nonintimate___5 ,
            exp_nonint_other = mpx_symp_nonintimate___6 ,
            exp_nonint_none= mpx_symp_nonintimate___7,
            
            
            exp_bot_noc =mpx_symp_intimate___1,
            exp_top_noc =mpx_symp_intimate___2, 
            exp_anal_c =mpx_symp_intimate___3, 
            exp_vag =mpx_symp_intimate___4, 
            exp_roral_noc =mpx_symp_intimate___5,
            exp_goral_noc =mpx_symp_intimate___6, 
            exp_oral_c =mpx_symp_intimate___7,
            exp_othint =mpx_symp_intimate___8,
            exp_closecont_massgath =mpx_symp_intimate___9, 
            exp_stscontact=mpx_symp_intimate___10,
            exp_int_other=mpx_symp_intimate___11,
            exp_int_none=mpx_symp_intimate___12,
    
    
      rlloc_face = lesion_location___1 , 
      rlloc_head = lesion_location___2 , 
      rlloc_neck = lesion_location___3, 
      rlloc_mouthroat = lesion_location___4 , 
      rlloc_lips = lesion_location___5 , 
      rlloc_trucktorso = lesion_location___6 , 
      rlloc_arms = lesion_location___7 ,
      rlloc_legs = lesion_location___8, 
      rlloc_palms = lesion_location___9, 
      rlloc_feetsoles = lesion_location___10 , 
      rlloc_genitals = lesion_location___11 , 
      rlloc_perianal = lesion_location___12 , 
      rlloc_other = lesion_location___13
      
    )
     
    


###################################################################################
###################################################################################
###################################################################################
#Checks
 exp_catdata %>% pfreq(any_contact_confprob)
    
#Does anyone report 'other' symptoms
sympcheck <-exp_catdata  %>% pfreq(symptoms_count_ge1, symptoms_count, symptom_type___1, symptom_type___2, symptom_type___3, symptom_type___4, symptom_type___5, symptom_type___6,
                             symptom_type___7, symptom_type___8, symptom_type___9, symptom_type___10, symptom_type___11,  symptom_type___12,
                             symptom_type___13, symptom_type___14,  symptom_type___15, symptom_type___16, other_sx_spec)
rascount <- exp_catdata %>% pfreq (rashlesion_loc_count_ge1, rashlesion_loc_count, lesion_location___1 , lesion_location___2 , lesion_location___3, lesion_location___4 , lesion_location___5 , lesion_location___6 , lesion_location___7 ,
                                      lesion_location___8, lesion_location___9, lesion_location___10 , lesion_location___11 , lesion_location___12 , lesion_location___13)

concheck <-exp_catdata %>% pfreq (any_contact_confprob, contact_mpx_dx_case, contact_mpx_dx_control, contact_mpx_symp_case,contact_mpx_symp_control )
concheck2 <-exp_catdata %>% pfreq (all_contact_ge1,nonint_contact_ge1,int_contact_ge1)
concheck3 <-exp_catdata %>% pfreq (any_contact_confprob, all_sympdxcontact_ge1)

rascount2 <- exp_catdata %>% pfreq (lesion_location___13,lesion_location_other_spec)

racecheck <- exp_catdata %>% pfreq(race_cat,race___1, race___2, race___3, race___4, race___5, race___6)
raceeth <- exp_catdata %>% pfreq( race_ethnicity,race_cat,ethnicity)

ni_oth <-exp_catdata %>% pfreq (mpx_symp_intimate_other)
ni_types<-raw %>% pfreq (mpx_symp_nonintimate___1, mpx_symp_nonintimate___2,mpx_symp_nonintimate___3,
                         mpx_symp_nonintimate___4,mpx_symp_nonintimate___5,mpx_symp_nonintimate___6,
                         mpx_symp_nonintimate___7)


ni_count <- exp_catdata %>% pfreq (nonint_contact_ge1, nonint_contact_count, nonint_inhomecare, 
                                     nonint_shareddishes,  nonint_sharedlinens ,nonint_sharddrugeq,nonint_ftfcontact,
                                     nonint_other,   nonint_none)
contact_check <- raw %>% pfreq (contact_mpx_dx_case,  contact_mpx_dx_control,
                                contact_mpx_symp_case,
                                contact_mpx_symp_control)


#var list; 
contact_mpx_dx_case
contact_mpx_dx_control

