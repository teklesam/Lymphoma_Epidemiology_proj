#  NHL lymphoma Analysis--------------------------------------------------------

## Title:-----------------------------------------------------------------------

# Analysis of epidemiology of Lymphoma in NHL, Asmara, Eritrea 


# Written by : Samuel Tekle,MD--------------------------------------------------
# Copyright (c) - 2024 Samuel Tekle

# Version control:--------------------------------------------------------------
# Started on : 19.09.2024

# Install required packages
library(pacman)

pacman::p_load(
  tidyr,
  dplyr,
  ggplot2,
  readr,
  epitools,
  here,
  rio
)



# Importing the data


lymphoma_data_incidence <-  rio::import(here("3. Output",
                                             "cleaned_preliminary_data.rds")) %>% 
  select(case_id,age,age_group,sex, date_of_diagnosis, diagnosis_binned)


eri_population <- import(here("1. Data",
                              "Eritrea_2015_population.csv"))


standard_who <- import(here("1. Data",
                            "who_standard.csv"))


# Summarize lymphoma cases by age group and sex
lymphoma_summary <- lymphoma_data_incidence %>%
  group_by(age_group, sex) %>%
  summarise(
    Count = n(), .groups = "drop"
  )

# Summarize total cases per age group
lymphoma_total <- lymphoma_summary %>%
  group_by(age_group) %>%
  summarise(
    Total = sum(Count), .groups = "drop"
  )

# Merge lymphoma data with population data
merged_data <- lymphoma_summary %>%
  full_join(eri_population, by = "age_group") %>%
  group_by(age_group) %>%
  mutate(
    Incidence_Rate = ifelse(
      sex == "Male",
      (Count / Male) * 100000,
      (Count / Female) * 100000
    )
  ) %>%
  left_join(lymphoma_total, by = "age_group") %>%
  ungroup()

# Add Male-to-Female Ratio
final_output <- merged_data %>%
  group_by(age_group) %>%
  summarise(
    Male_Count = sum(Count[sex == "Male"], na.rm = TRUE),
    Female_Count = sum(Count[sex == "Female"], na.rm = TRUE),
    Total = sum(Total, na.rm = TRUE),
    Male_to_Female_Ratio = ifelse(
      Female_Count > 0,
      Male_Count / Female_Count,
      NA
    ),
    Percentage = round((Total / sum(Total)) * 100, 2)
  )

# Display final output
print(final_output)


#R Code for Yearly CIR and ASR

# Extract the year from the date_of_diagnosis
lymphoma_data_incidence <- lymphoma_data_incidence %>%
  mutate(year_of_diagnosis = format(as.Date(date_of_diagnosis), "%Y"))


# Calculate crude incidence rate (CIR) by year
yearly_summary <- lymphoma_data_incidence %>%
  group_by(year_of_diagnosis, sex) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  )

# Add total cases for both sexes
yearly_total <- yearly_summary %>%
  group_by(year_of_diagnosis) %>%
  summarise(
    Total_Count = sum(Count),
    .groups = "drop"
  )



# Calculate Crude Incidence Rate (CIR) yearly
yearly_cir <- yearly_summary %>%
  left_join(eri_population %>% summarise(Population = sum(Male + Female)), by = character()) %>%
  mutate(
    CIR = (Count / Population) * 100000
  )

# Calculate Age-Standardized Rate (ASR)
yearly_asr <- lymphoma_data_incidence %>%
  group_by(year_of_diagnosis, age_group, sex) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  ) %>%
  left_join(eri_population, by = "age_group") %>%
  left_join(standard_who, by = "age_group") %>%
  mutate(
    Weighted_Rate = (Count / (Male + Female)) * weight
  ) %>%
  group_by(year_of_diagnosis, sex) %>%
  summarise(
    ASR = sum(Weighted_Rate, na.rm = TRUE) * 100000,
    .groups = "drop"
  )

# Combine CIR and ASR for final output
final_yearly_output <- yearly_cir %>%
  left_join(yearly_asr, by = c("year_of_diagnosis", "sex")) %>%
  group_by(year_of_diagnosis) %>%
  summarise(
    Male_CIR = CIR[sex == "Male"],
    Female_CIR = CIR[sex == "Female"],
    Total_CIR = sum(CIR),
    Male_ASR = ASR[sex == "Male"],
    Female_ASR = ASR[sex == "Female"],
    Total_ASR = sum(ASR),
    Male_to_Female_Ratio = ifelse(
      CIR[sex == "Female"] > 0,
      CIR[sex == "Male"] / CIR[sex == "Female"],
      NA
    )
  )

# Display final output
print(final_yearly_output)


# Summary of Crude incidence rate and ASR


# Summarize data by classification, year, and sex
lymphoma_summary_diagnosis <- lymphoma_data_incidence %>%
  group_by(year_of_diagnosis, diagnosis_binned, sex, age_group) %>%
  summarise(
    Count = n(), .groups = "drop"
  )

# Add population and standard weights
merged_data <- lymphoma_summary_diagnosis %>%
  left_join(eri_population, by = "age_group") %>%
  left_join(standard_who, by = "age_group") %>%
  mutate(
    Population = ifelse(sex == "Male", Male, Female),
    Weighted_Rate = (Count / Population) * weight
  )

# Calculate ASR for each lymphoma type, gender, and year
asr_summary <- merged_data %>%
  group_by(year_of_diagnosis, diagnosis_binned, sex) %>%
  summarise(
    ASR = sum(Weighted_Rate, na.rm = TRUE) * 100000,
    .groups = "drop"
  )

# Calculate CIR
cir_summary <- lymphoma_summary_diagnosis %>%
  left_join(
    eri_population %>% summarise(Total_Population = sum(Male + Female)),
    by = character()
  ) %>%
  mutate(
    CIR = (Count / Total_Population) * 100000
  ) %>%
  group_by(year_of_diagnosis, diagnosis_binned, sex) %>%
  summarise(
    CIR = sum(CIR, na.rm = TRUE),
    .groups = "drop"
  )

# Combine ASR and CIR
combined_summary <- asr_summary %>%
  full_join(cir_summary, by = c("year_of_diagnosis", "diagnosis_binned", "sex")) %>%
  pivot_wider(
    names_from = diagnosis_binned,
    values_from = c(CIR, ASR),
    names_glue = "{.value}_{diagnosis_binned}"
  )
colnames(combined_summary)

# Restructure for the final summary table
final_summary_diagnosis <- combined_summary %>%
  group_by(year_of_diagnosis) %>%
  summarise(
    Female_ASR_NHL = sum(`ASR_non-Hodgkins Lymphoma`[sex == "Female"], na.rm = TRUE),
    Male_ASR_NHL = sum(`ASR_non-Hodgkins Lymphoma`[sex == "Male"], na.rm = TRUE),
    Total_ASR_NHL = sum(`ASR_non-Hodgkins Lymphoma`, na.rm = TRUE),
    Female_ASR_HL = sum(`ASR_Hodgkins Lymphoma`[sex == "Female"], na.rm = TRUE),
    Male_ASR_HL = sum(`ASR_Hodgkins Lymphoma`[sex == "Male"], na.rm = TRUE),
    Total_ASR_HL = sum(`ASR_Hodgkins Lymphoma`, na.rm = TRUE),
    Female_ASR_UL = sum(`ASR_Unclassified Lymphoma`[sex == "Female"], na.rm = TRUE),
    Male_ASR_UL = sum(`ASR_Unclassified Lymphoma`[sex == "Male"], na.rm = TRUE),
    Total_ASR_UL = sum(`ASR_Unclassified Lymphoma`, na.rm = TRUE),
    .groups = "drop"
  )

# Print final summary
print(final_summary_diagnosis)

# Create the data frame
data <- data.frame(
  Year = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023),
  Total_ASR = c(2.58, 1.61, 1.04, 1.19, 2.56, 0.34, 1.46, 1.19, 3.28, 1.19, 2.32, 19.42, 7.54, 6.00)
)

# Add the log of Total ASR
data$Log_Total_ASR <- log(data$Total_ASR)

# Linear regression
model <- lm(Log_Total_ASR ~ Year, data = data)

# View the model summary
summary(model)

# Extract the slope from the model
slope <- coef(model)[2]

# Calculate the AAPC for the slope
AAPC <- (exp(slope) - 1) * 100

# Extract the 95% Confidence Interval for the slope (Year)
conf_int <- confint(model, level = 0.95)

# The confidence interval for the slope (Year) is in the second row
CI_slope <- conf_int[2, ]

# Calculate AAPC for the lower and upper bounds of the confidence interval
AAPC_lower <- (exp(CI_slope[1]) - 1) * 100
AAPC_upper <- (exp(CI_slope[2]) - 1) * 100

# Print the AAPC and its 95% Confidence Interval
cat("AAPC:", round(AAPC, 2), "%\n")
cat("95% CI for AAPC: [", round(AAPC_lower, 2), "%, ", round(AAPC_upper, 2), "%]\n")


#P-value of the trend of the cases



# Extract the p-value for the slope (Year)
p_value <- summary(model)$coefficients[2, 4]

# Print the p-value
cat("p-value for the trend:", round(p_value, 4), "\n")





