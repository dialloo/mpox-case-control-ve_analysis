#=========================================================================================
# Author(s): Alpha Oumar Diallo
# Collaborators: MPX-VE Case-Control Team - Leora Feldstein, Alex Dalton,
#                Dani Moulia, Anna Chard, Nick Deputy, Amy Fothergill, and Jurisdictions
# Program: 00_REDCap-API-linkage
# Project: MPX Multi-Jurisdictional Vaccine Effectiveness Case-Control Evaluation 
# Objective: Evaluate the effectiveness of the JYNNEOS vaccine in preventing symptomatic
#           mpox disease among 18 to 49-year-old gay, bisexual, and other men who have 
#           sex with men, as well as transgender, non-binary, and gender-diverse persons. 
# Task: Import data using REDCap API token
# Data in: 
# Initial program date: 2022-12-15
# Last modified date: 2022-12-19
#===========================================================================================

# Load packages -----------------------------------------------------------------
pacman::p_load(httr, # Useful tools for working with HTTP organised by HTTP verbs (GET(), POST(), etc).
               xml2   # Work with XML files using a simple, consistent interface
)


# Import data -------------------------------------------------------------------
  h <- handle("https://auth.cdc.gov/siteminderagent/forms/AuthNService/login.fcc?USER=SYS-XXXX&PASSWORD=XXXX") 
  
  GET(handle = h) 
  
  url <- "https://rdcp.cdc.gov/api/"
  
  result <- POST(url, handle = h, body = list(token='XXXX', content='record',
                                              format='csv', type='flat', rawOrLabel='raw', rawOrLabelHeaders='raw',
                                              exportCheckboxLabel='true', exportSurveyFields='false',
                                              exportDataAccessGroups='true', returnFormat='csv'))
  raw <- read.table(text = content(result, "text"), sep =",", header = TRUE, stringsAsFactors = FALSE)

# Remove h and result lists and the url
  
  remove(h); remove(result); remove(url)
  
  #!/usr/bin/env Rscript
  token <- "1B09A6365E53B73E414DAC4175991F0C"
  url <- "https://rdcp.cdc.gov/api/"
  formData <- list("token"=token,
                   content='project',
                   format='csv',
                   returnFormat='csv'
  )
  response <- httr::POST(url, body = formData, encode = "form")
  result <- httr::content(response)
  print(result)
