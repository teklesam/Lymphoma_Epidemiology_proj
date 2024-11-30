###########################
# Epidemiology of Lymphoma
##########################

# 
# Written by : Samuel Tekle,MD
# Copyright (c) - 2024 Samuel Tekle
# 
# Started on : 12.9.2024
# Version control:
####################################

# Step 1 - Loading Necessary Packages ====

library(pacman)             # "pacman" package manager - Helps to load multiple packages at a time

pacman::p_load(
  
  # project and file management
  #############################
  here,     # file paths relative to R project root folder ( to prevent complications from file paths that are specific to one computer)
  rio,      # import/export of many types of data
  
  # General data management
  #########################
  tidyverse,    # includes many packages for tidy data wrangling and presentation
  janitor,      # tables and data cleaning
  stringr,      # Deals with strings
  labelled      # Labels variables
)  


# Step 2 - Importing, cleaning and Sorting data ====

data_v1 <- "Lymphoma version 1.csv"

preliminary_data <- import (here ("1. Data",data_v1))


# cleaning variable names

preliminary_data <- preliminary_data %>% 
  clean_names()


preliminary_data$case_id <- seq(1,336,1)

preliminary_data <- preliminary_data %>% 
  select(case_id, -c(total_aar,female_aar,male_aar,wt, total_cir,
                     female_cir, male_cir,year_binned, name), 
         age, 
         age_bined, 
         sex, 
         date_of_diagnosis, 
         adress,
         adress_by_zone,
         sample_taken_from, 
         sample_taken_site_anatomy, 
         type_of_sample, 
         reffering_hospital, 
         duration_of_symptome, 
         duration_of_symptome_in_days,
         diagnosis, 
         diagnosis_binned, 
         comment, 
         grade_of_disease, 
         clinical_history, 
         clinical_history_lymphadenopathy, 
         clinical_history_splenomegaly, 
         clinical_history_hepatomegaly, 
         history_fever, 
         history_of_night_sweating, 
         history_of_cough, 
         histor_of_gbw, 
         history_of_wt_loss
  )
# Making the variable values uniform
# variables needing uniformity = 1. comment

# standardizing the variables codes

preliminary_data$date_of_diagnosis <- as.Date(preliminary_data$date_of_diagnosis, 
                                              format = "%m/%d/%Y")

preliminary_data <- preliminary_data %>% 
  mutate(
    case_id                        = as.character (case_id),
    age                            = as.numeric (age),
    age_group                      =  case_when (                                                     # recoding gender
      age_bined == 1   ~   "</= 4 years",
      age_bined == 2   ~   "5-9 years",
      age_bined == 3   ~   "10-19 years",
      age_bined == 4   ~   "20-29 years",
      age_bined == 5   ~   "30-39 years",
      age_bined == 6   ~   "40-49 years",
      age_bined == 7  ~   "50-59 years",
      age_bined == 8   ~   "60-69 years",
      age_bined == 9   ~   "70 + years",
    ) ,
    age_bined                      = as.factor (age_bined),
    sex                            = case_when (                                                     # recoding gender
      sex == 0   ~   "Male",
      sex == 1   ~   "Female"),
    sex                            = as.factor (sex),
    adress_by_zone                 = case_when (                                                  # recoding gender
      adress_by_zone == 0   ~   "Maekel",
      adress_by_zone == 1   ~   "Debub",
      adress_by_zone == 2   ~   "NRS",
      adress_by_zone == 3   ~   "Anseba",
      adress_by_zone == 4   ~   "Gash-Barka",
      adress_by_zone == 5   ~   "SRS"
    ),
    adress_by_zone                 = as.factor (adress_by_zone),                                                   
    sample_taken_from               = as.factor(sample_taken_from),
    sample_taken_site_anatomy       = as.factor(sample_taken_site_anatomy),
    type_of_sample                  = case_when (                                                     
      type_of_sample == 0   ~   "Body Fluid",
      type_of_sample == 1   ~   "Biopsy",
      type_of_sample == 2   ~   "FNA"
    ),
    type_of_sample                  = as.factor (type_of_sample),
    reffering_hospital              = as.factor (reffering_hospital),
    duration_of_symptome            = as.character (duration_of_symptome), 
    duration_of_symptome_in_days    = as.numeric (duration_of_symptome_in_days),
    diagnosis                       = as.character(diagnosis),
    diagnosis_binned                = case_when(
      diagnosis_binned == 0  ~  "Hodgkins Lymphoma",
      diagnosis_binned == 1  ~  "non-Hodgkins Lymphoma",
      diagnosis_binned == 2  ~  "Unclassified Lymphoma",
      diagnosis_binned == 3  ~  "Suspected Lymphoma"),
    diagnosis_binned                = as.factor (diagnosis_binned),
    comment                         = as.factor (comment),
    grade_of_disease                = case_when (
      grade_of_disease == 0  ~  "Low-grade",
      grade_of_disease == 1  ~  "High-grade",
      grade_of_disease == 2  ~  "Unspecified",
      grade_of_disease == 3  ~  "Moderate"),
    grade_of_disease                = as.factor (grade_of_disease),
    clinical_history                = as.character(clinical_history), 
    clinical_history_lymphadenopathy= case_when (
      clinical_history_lymphadenopathy == 0  ~  "Single lymphadenopathy",
      clinical_history_lymphadenopathy == 1  ~  "Multiple lymphadenopathy",
      clinical_history_lymphadenopathy == 2  ~  "No lymphadenopathy"),
    clinical_history_lymphadenopathy = as.factor (clinical_history_lymphadenopathy),
    clinical_history_splenomegaly    = case_when (
      clinical_history_splenomegaly == 0  ~  "Splenomegaly",
      clinical_history_splenomegaly == 1  ~  "No Splenomegaly"),
    clinical_history_splenomegaly    = as.factor(clinical_history_splenomegaly),
    clinical_history_hepatomegaly    = case_when (
      clinical_history_hepatomegaly == 0  ~  "Hepatomegaly",
      clinical_history_hepatomegaly == 1  ~  "No Hepatomegaly"), 
    clinical_history_hepatomegaly    = as.factor(clinical_history_hepatomegaly),
    history_fever                   = case_when (
      history_fever == 0  ~  "Low-grade",
      history_fever == 1  ~  "High-grade",
      history_fever == 2  ~  "Unspecified",
      history_fever == 3  ~  "No/undocumented"),
    history_fever                   = as.factor (history_fever),
    history_of_night_sweating       = case_when (
      history_of_night_sweating == 0  ~  "Night sweats",
      history_of_night_sweating == 1  ~  "No night sweats",
      history_of_night_sweating == 2  ~  "Unspecified"),
    history_of_night_sweating       = as.factor (history_of_night_sweating),
    history_of_cough                = case_when (
      history_of_cough == 0  ~  "cough",
      history_of_cough == 1  ~  "No cough",
      history_of_cough == 2  ~  "Unspecified"), 
    history_of_cough       = as.factor (history_of_cough),
    histor_of_gbw                   = case_when (
      histor_of_gbw == 0  ~  "Generalized body weakness",
      histor_of_gbw == 1  ~  "No GBW",
      histor_of_gbw == 2  ~  "Unspecified"), 
    histor_of_gbw       = as.factor (histor_of_gbw),
    history_of_wt_loss              = case_when (
      history_of_wt_loss == 0  ~  "Positive weight loss",
      history_of_wt_loss == 1  ~  "No weight loss",
      history_of_wt_loss == 2  ~  "Unspecified"), 
    history_of_wt_loss       = as.factor (history_of_wt_loss)
  )



# Sample dataset with inconsistent values
unclean_coments <- data.frame(
  Recommendations = c("biopsy", "biospy", "bone marrow biopsy", 
                      "excisional biopcy", "EXCISIONAL BIOPSY", "EXCISONAL BIOPSY", 
                      "flow cytometry,immuniohistochemistry, molecular analysis",
                      "imminiohistochemistry", "imminiohistochemistry,cytogentic test", 
                      "imminiohistochemistrya and immuniophenotyping", 
                      "immuiohistochemistry", "immuniohistochemistry", 
                      "immuniohistochemistry and molecualr study", 
                      "immuniohsitochemistry", "immunohistochemistry", 
                      "lymphnode biopcy")
)

# Define a cleaning function to standardize the recommendations
clean_recommendations <- function(comment) {
  # List of terms related to biopsy
  biopsy_terms <- c("biopsy", "biospy", "bone marrow biopsy", "excisional biopcy", 
                    "EXCISIONAL BIOPSY", "EXCISONAL BIOPSY", "lymphnode biopcy")
  
  # List of terms related to immunohistochemistry
  immunohistochemistry_terms <- c("flow cytometry,immuniohistochemistry, molecular analysis",
                                  "imminiohistochemistry", "imminiohistochemistry,cytogentic test", 
                                  "imminiohistochemistrya and immuniophenotyping", 
                                  "immuiohistochemistry", "immuniohistochemistry", 
                                  "immuniohistochemistry and molecualr study", 
                                  "immuniohsitochemistry", "immunohistochemistry")
  
  # Assign "biopsy" for biopsy-related terms
  if (comment %in% biopsy_terms) {
    return("biopsy")
  }
  
  # Assign "immunohistochemistry" for immunohistochemistry-related terms
  if (comment %in% immunohistochemistry_terms) {
    return("immunohistochemistry")
  }else{
    return(NA)
  }
  
  
  # If not matched, return original recommendation (optional)
  return(comment)
}

# Apply the cleaning function to the 'Recommendations' column
preliminary_data <- preliminary_data %>%
  mutate(comment = sapply(comment, clean_recommendations))

# Show cleaned data
print(preliminary_data)
preliminary_data[4,]




# library(dplyr)
# library(purrr)
# 
# `%||%` <- function(lhs, rhs) {
#   if (is.null(lhs) || length(lhs) == 0) rhs else lhs
# }
# 
# 
# clean_anatomy <- function(sample_taken_site_anatomy) {
#   # Define terms related to lymph nodes and other regions
#   axillary_terms <- c("AXILLA", "AXILLA AND INGUINAL", "CERVICAL AND AXILLA", "INGUINAL AND AXILLA LY",
#                       "SUPRACLAVICULAR AND AXILLA", "CERVICAL,AXILLA AND INGUINAL", "CERVICAL,SUPRACLAVICULAR AND AXILLARY")
#   
#   cervical_terms <- c("CERVICAL LY", "CERVICAL AND AXILLA", "CERVICAL AND INGUINAL", "CERVICAL AND SKIN", 
#                       "CERVICAL,AXILLA AND INGUINAL", "CERVICAL,SUPRACLAVICULAR AND AXILLARY")
#   
#   inguinal_terms <- c("INGUINAL LY", "AXILLA AND INGUINAL", "CERVICAL AND INGUINAL", "INGUINAL AND AXILLA LY", 
#                       "GROIN LY", "CERVICAL,AXILLA AND INGUINAL")
#   
#   supraclavicular_terms <- c("SUPRACLAVICULAR LY", "SUPRACLAVICULAR AND AXILLA", "CERVICAL,SUPRACLAVICULAR AND AXILLARY")
#   
#   # Define a list for other regions
#   other_terms <- list(
#     "ABDOMEN" = "ABDOMEN",
#     "BLADDER MASS" = "BLADDER",
#     "BOWEL MASS" = "BOWEL",
#     "BREAST" = "BREAST",
#     "CHEST WALL" = "CHEST WALL",
#     "ENDOMETRIUM" = "ENDOMETRIUM",
#     "GASTRIC" = "GASTRIC",
#     "HEAD MASS" = "HEAD",
#     "KNEE" = "KNEE",
#     "NASOPHARYNX" = "NASOPHARYNX",
#     "OVARY" = "OVARY",
#     "PANCREASE" = "PANCREAS",
#     "PAROTID" = "PAROTID",
#     "PERICARDIAL FLUID" = "PERICARDIAL FLUID",
#     "PLEURAL EFFUSION" = "PLEURAL EFFUSION",
#     "SKIN" = "SKIN",
#     "STERNAM" = "STERNUM",
#     "TESTICLE" = "TESTICLE",
#     "THIGH" = "THIGH",
#     "THYROID" = "THYROID",
#     "TONSIL" = "TONSIL",
#     "UNSPECIFIED LY" = "UNSPECIFIED LYMPH NODE",
#     "UNSPECIFIED WOUND" = "UNSPECIFIED WOUND",
#     "VULVA" = "VULVA",
#     "UTERUS AND OVARY" = "UTERUS AND OVARY"
#   )
#   
#   # Initialize an empty list to hold lymph node regions
#   involved_regions <- list()
#   
#   # Check for involvement of lymph node regions
#   if (any(sample_taken_site_anatomy %in% axillary_terms)) {
#     involved_regions <- c(involved_regions, "AXILLA")
#   }
#   if (any(sample_taken_site_anatomy %in% cervical_terms)) {
#     involved_regions <- c(involved_regions, "CERVICAL")
#   }
#   if (any(sample_taken_site_anatomy %in% inguinal_terms)) {
#     involved_regions <- c(involved_regions, "INGUINAL")
#   }
#   if (any(sample_taken_site_anatomy %in% supraclavicular_terms)) {
#     involved_regions <- c(involved_regions, "SUPRACLAVICULAR")
#   }
#   
#   # Handle the number of involved regions
#   if (length(involved_regions) == 1) {
#     return(involved_regions[[1]])  # Only one region
#   } else if (length(involved_regions) == 2) {
#     return(paste(involved_regions, collapse = " AND "))  # Two regions involved
#   } else if (length(involved_regions) > 2) {
#     return("Multiple regions involved")  # More than two regions
#   }
#   
#   # Debugging: Print the problematic sample_taken_site_anatomy
#   if (!sample_taken_site_anatomy %in% names(other_terms)) {
#     message(paste("Unrecognized term:", sample_taken_site_anatomy))
#   }
#   
#   # Safely handle the other terms (non-lymph node sites)
#   return(other_terms[[sample_taken_site_anatomy]] %||% sample_taken_site_anatomy)
# }
# 
# # Apply the cleaning function using mutate and map_chr
# cleaned_data <- preliminary_data %>%
#   mutate(sample_taken_site_anatomy = map_chr(sample_taken_site_anatomy, clean_anatomy))
# 
# # Show cleaned data
# print(cleaned_data)



# Labeling the variables

preliminary_data <- preliminary_data %>% 
  labelled::set_variable_labels(
    case_id              = "Primary identifier",
    age                  = "Age (years)",
    age_bined            = "Age in 5-yr band",
    sex                  = "Gender",
    date_of_diagnosis    = "Calender date",
    adress               = "Address (town)",
    adress_by_zone       = "Address by zone",
    sample_taken_from    = "Body site of the sample",
    sample_taken_site_anatomy = "Classified body site",
    type_of_sample       = "FNA/Biopsy",
    reffering_hospital   = "Hospital",
    duration_of_symptome = "Duration with different units",
    duration_of_symptome_in_days = "Uniform duration in days",
    diagnosis            = "Diagnosis as in paper",
    diagnosis_binned     = "Diagnosis dichotomized",
    comment              = "Recommendations",
    grade_of_disease     = "Grade of the disease",
    clinical_history     = "Clinical history",
    clinical_history_lymphadenopathy =  "+/- Lymphadenopathy",
    clinical_history_splenomegaly    =  "+/- Splenomegaly",
    clinical_history_hepatomegaly    =  "+/- Hepatomegaly",
    history_fever                    =  "+/- fever",
    history_of_night_sweating        =  "+/- Night Sweats",
    history_of_cough                 =  "+/- Cough",
    histor_of_gbw                    =  "+/- Generalized body weakness",
    history_of_wt_loss               =  "+/- weight loss"
  )

# Step 3 - Exporting in RDS and excel===========================================

rio::export(preliminary_data,here("3. Output",
                                  "cleaned_preliminary_data.rds"))
rio::export(preliminary_data,here("3. Output",
                                  "cleaned_preliminary_data.xlsx"))

