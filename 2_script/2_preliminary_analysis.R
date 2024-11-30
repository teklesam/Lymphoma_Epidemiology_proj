#  NHL Blood AMR Analysis--------------------------------------------------------

## Title:-----------------------------------------------------------------------

# Analysis of epidemiology of Lymphoma in NHL, Asmara, Eritrea 


# Written by : Samuel Tekle,MD--------------------------------------------------
# Copyright (c) - 2024 Samuel Tekle

# Version control:--------------------------------------------------------------
# Started on : 14.09.2024

#-------------------------------------------------------------------------------

# 0. Data Preparation ----------------------------------------------------------

## 0.1 Load packages------------------------------------------------------------

library(pacman)

pacman::p_load(
  gtsummary,            # Tabulation of data in publication-friendly format
  tidyverse,            # data manipulation and visualization
  labelled,             # Labelling the variables or column names
  usethis,        
  here,             
  rio,              
  skimr,
  stringr,
  forcats,
  janitor,
  ggplot2,
  dplyr,
  lubridate,
  scales,
  incidence
)

## 0.2 Import data--------------------------------------------------------------

lymphoma_data <-  rio::import(here("3. Output",
                                   "cleaned_preliminary_data.rds")) %>% 
  select(-c(case_id, adress, sample_taken_from, duration_of_symptome,
            diagnosis, clinical_history))

colnames(lymphoma_data)

# Step 3 - Summary Statistics ==================================================

age_summary<- lymphoma_data %>%
  rstatix::get_summary_stats(age)
# Check the structure and summary of the imported data
glimpse(lymphoma_data)
skimr::skim(lymphoma_data)

# Histogram for a continuous variable (e.g., age)
ggplot(lymphoma_data, aes(x = age)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1.2, adjust = 1) +  # Density line to highlight peaks
  labs(title = "Age Distribution of Lymphoma Cases with Density Curve",
       x = "Age",
       y = "Density") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(lymphoma_data$age, na.rm = TRUE), by = 5)) +
  scale_y_continuous(labels = scales::percent)  # Optional: shows density


# Assume 'lymphoma_data' has columns 'age' and 'lymphoma_type'


# Define custom colors for lymphoma types
custom_colors <- c(
  "non-Hodgkins Lymphoma" = "#FF6F61",   # Coral
  "Hodgkins Lymphoma" = "#6B5B95",   # Purple
  "Unclassified Lymphoma" = "#88B04B"   # Green
  
)

ggplot(lymphoma_data, aes(x = age, fill = diagnosis_binned)) +
  geom_histogram(binwidth = 5, position = "stack", color = "black") +
  labs(
    title = "Age Distribution of Lymphoma Cases by Type",
    x = "Age",
    y = "Count",
    fill = "Lymphoma Type"
  ) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    legend.position = "right"
  )


# Create a histogram to identify the age distribution for males and females

library(ggplot2)
library(RColorBrewer)  # For attractive color palettes

# Create a histogram for age distribution by gender
ggplot(data = lymphoma_data, aes(x = age, fill = sex)) +
  geom_histogram(binwidth = 2, position = "dodge", 
                 alpha = 0.8, color = "white") +  # Use dodge for side-by-side bars
  labs(title = "Age Distribution of Incident Lymphoma Cases by Gender",
       x = "Age (Years)",
       y = "Count") +
  scale_fill_manual(values = brewer.pal(3, "Set1")[1:2]) +  # Attractive color palette
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Center and bold the title
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

library(ggplot2)
library(RColorBrewer)  # For attractive color palettes

# Create a histogram for age distribution by gender with trend lines
ggplot(data = lymphoma_data, aes(x = age, fill = sex)) +
  geom_histogram(binwidth = 2, position = "dodge", 
                 alpha = 0.5, color = "white") +  # Use dodge for side-by-side bars
  labs(title = "Age Distribution of Incident Lymphoma Cases by Gender",
       x = "Age (Years)",
       y = "Count") +
  scale_fill_manual(values = brewer.pal(3, "Set1")[1:2]) +  # Attractive color palette
  geom_density(aes(y = ..count.., color = sex), 
               alpha = 0.5, size = 1, adjust = 1.5, show.legend = TRUE) +  # Add density lines
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Center and bold the title
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))


### # Group ages into ranges

lymphoma_data <- lymphoma_data %>%
  mutate(age_group = cut(age, 
                         breaks = seq(0, 105, by = 5),  # Adjusted to create 21 intervals
                         right = FALSE, 
                         labels = paste(seq(0, 100, by = 5), 
                                        seq(4, 104, by = 5), sep = "-")))

# Count cases for each age group and gender
age_specific_rates <- lymphoma_data %>%
  group_by(age_group, sex) %>%
  summarize(count = n(), .groups = 'drop')


#Importing data of 2015
population_data <- import(here("1. Data","Eritrea-2015_population.csv"))



overall_Summary <- lymphoma_data %>%
  gtsummary::tbl_summary() %>% 
  bold_labels() 



Table_1_lymphoma_type <- lymphoma_data %>% 
  select(-date_of_diagnosis) %>% 
  gtsummary::tbl_summary(
    type = c(age) ~ "continuous",
    by = diagnosis_binned,
    digits = everything() ~ 0) %>% 
  add_overall() %>% 
  add_p() %>%
  add_stat_label() %>% 
  add_q(method = 'fdr') %>% 
  bold_p() %>% 
  bold_labels()


Table_gender <- lymphoma_data %>% 
  select(-date_of_diagnosis) %>% 
  gtsummary::tbl_summary(by = "sex") %>% 
  add_overall() %>% 
  bold_labels() %>% 
  add_p() %>% 
  bold_p()

# Plotting the cases with time

df <- lymphoma_data %>% 
  mutate(date_of_diagnosis = as.Date(date_of_diagnosis)) %>%  # Ensure the dates are in Date format
  arrange(date_of_diagnosis)  # Sort data by Date of Diagnosis

# Create a cumulative count of cases
df_cumulative <- df %>%
  group_by(date_of_diagnosis) %>%
  summarise(Daily_Cases = n()) %>%
  mutate(Cumulative_Cases = cumsum(Daily_Cases))  # Compute cumulative cases

# Frequency histogram

ggplot(df, aes(x = date_of_diagnosis)) +
  geom_histogram(binwidth = 30, fill = "skyblue", color = "black", boundary = 0.5) +
  labs(
    title = "Frequency Histogram of Lymphoma Incident Cases",
    x = "Calendar Date",
    y = "Number of Incident Cases"
  ) +
  theme_minimal() +  # Clean theme
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )
# Create a ggplot for cumulative cases over time

ggplot(df_cumulative, aes(x = date_of_diagnosis, y = Cumulative_Cases)) +
  geom_line(color = "blue", size = 1) +  # Line for cumulative cases
  geom_point(color = "red", size = 2) +  # Optional points on the line
  labs(
    title = "Cumulative Incidence of Lymphoma Cases",
    x = "Date of Diagnosis",
    y = "Cumulative Number of Cases"
  ) +
  theme_minimal() +  # Clean theme
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

ggplot(df_cumulative, aes(x = date_of_diagnosis, y = Cumulative_Cases)) +
  geom_line(color = "blue", size = 1) +
  geom_smooth(method = "loess", color = "green", size = 1, se = FALSE) +  # Add smoothed line
  geom_point(color = "red", size = 2) +
  labs(
    title = "Cumulative Incidence of Lymphoma Cases",
    x = "Date of Diagnosis",
    y = "Cumulative Number of Cases"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

#Summary statistics of the data points:
summary_statistics <- df_cumulative %>%
  summarise(
    Total_Daily_Cases = sum(Daily_Cases, na.rm = TRUE),
    Mean_Daily_Cases = mean(Daily_Cases, na.rm = TRUE),
    Median_Daily_Cases = median(Daily_Cases, na.rm = TRUE),
    SD_Daily_Cases = sd(Daily_Cases, na.rm = TRUE),
    Min_Daily_Cases = min(Daily_Cases, na.rm = TRUE),
    Max_Daily_Cases = max(Daily_Cases, na.rm = TRUE),
    Total_Cumulative_Cases = max(Cumulative_Cases, na.rm = TRUE),
    Mean_Cumulative_Cases = mean(Cumulative_Cases, na.rm = TRUE),
    Median_Cumulative_Cases = median(Cumulative_Cases, na.rm = TRUE),
    SD_Cumulative_Cases = sd(Cumulative_Cases, na.rm = TRUE),
    Min_Cumulative_Cases = min(Cumulative_Cases, na.rm = TRUE),
    Max_Cumulative_Cases = max(Cumulative_Cases, na.rm = TRUE)
  )

print(summary_statistics)


# Print summary statistics
print(summary_statistics)

# Print summary statistics
print(summary_statistics)
# Load necessary library


# Frequecy Histogram
# Ensure the required packages are loaded
library(ggplot2)
library(scales)  # For date formatting

# Create the histogram of incident cases with custom x-axis breaks and labels

ggplot(df, aes(x = date_of_diagnosis)) +
  geom_histogram(binwidth = 365, fill = "lightgreen", color = "black") +  # Yearly bins
  labs(
    title = "Incident Cases of Lymphoma",
    x = "Calendar Date",
    y = "Number of Cases"
  ) +
  scale_x_date(
    breaks = date_breaks("1 year"),  # Breaks every 1 year
    labels = date_format("%Y")  # Format as "Year" (e.g., "2024")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)  # Rotate x-axis labels vertically
  )

# Create the histogram of incident cases with yearly breaks and superimposed density line
ggplot(df, aes(x = date_of_diagnosis)) +
  geom_histogram(binwidth = 365, fill = "lightgreen", color = "black", alpha = 0.5) +  # Yearly bins, transparent
  geom_density(aes(y = ..count.. * 365), color = "blue", size = 1.5) +  # Smoothed density line
  labs(
    title = "Incident Cases of Lymphoma with Smoothed Density Line",
    x = "Calendar Date",
    y = "Number of Cases"
  ) +
  scale_x_date(
    breaks = date_breaks("1 year"),  # Breaks every 1 year
    labels = date_format("%Y")  # Format as "Year" (e.g., "2024")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)  # Rotate x-axis labels vertically
  )

# Create the plot with a histogram and a density line
ggplot(data = df, aes(x = date_of_diagnosis)) +
  theme_bw() +  # Use a white background theme
  geom_histogram(
    binwidth = 45,  # Approximate monthly bins (30 days)
    colour = "gray", 
    fill = "darkblue", 
    linewidth = 0.1  # Histogram bars
  ) +
  geom_density(
    aes(y = after_stat(density) * (nrow(df) * 30)),  # Adjusting for monthly bins
    colour = "red"  # Density line
  ) +
  scale_x_date(
    breaks = date_breaks("6 months"),  # Breaks every 6 months
    labels = date_format("%b %Y"),  # Format as "Month Year" (e.g., "Sep 2024")
    name = "Date"  # X-axis label
  ) +
  scale_y_continuous(
    breaks = seq(from = 0, to = 12, by = 2),  # Y-axis breaks every 2 units
    limits = c(0, 12),  # Y-axis range from 0 to 14
    name = "Number of cases"  # Y-axis label
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)  # Rotate x-axis labels vertically
  )

# Create the plot with a histogram, density line, and facet grid for gender
ggplot(data = df, aes(x = date_of_diagnosis)) +
  theme_bw() +  # Use a white background theme
  geom_histogram(
    binwidth = 45,  # Approximate monthly bins (30 days)
    colour = "gray", 
    fill = "darkblue", 
    linewidth = 0.1  # Histogram bars
  ) +
  geom_density(
    aes(y = after_stat(density) * (nrow(df) * 30)),  # Adjusting for monthly bins
    colour = "red"  # Density line
  ) +
  scale_x_date(
    breaks = date_breaks("6 months"),  # Breaks every 6 months
    labels = date_format("%b %Y"),  # Format as "Month Year" (e.g., "Sep 2024")
    name = "Date"  # X-axis label
  ) +
  scale_y_continuous(
    breaks = seq(from = 0, to = 8, by = 1),  # Y-axis breaks every 2 units
    limits = c(0, 8),  # Y-axis range from 0 to 19
    name = "Number of cases"  # Y-axis label
  ) +
  facet_grid(sex ~ ., scales = "free_y") +  # Separate plots for each gender
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels vertically
    strip.text.y = element_text(size = 12)  # Increase size of facet labels
  )

ggplot(data = df, aes(x = date_of_diagnosis)) +
  theme_bw() +  # Use a white background theme
  geom_histogram(
    binwidth = 45,  # Approximate monthly bins (30 days)
    colour = "gray", 
    fill = "darkblue", 
    linewidth = 0.1  # Histogram bars
  ) +
  geom_density(
    aes(y = after_stat(density) * (nrow(df) * 30)),  # Adjusting for monthly bins
    colour = "red"  # Density line
  ) +
  scale_x_date(
    breaks = date_breaks("6 months"),  # Breaks every 6 months
    labels = date_format("%b %Y"),  # Format as "Month Year" (e.g., "Sep 2024")
    name = "Date"  # X-axis label
  ) +
  scale_y_continuous(
    breaks = seq(from = 0, to = 8, by = 1),  # Y-axis breaks every 2 units
    limits = c(0, 8),  # Y-axis range from 0 to 19
    name = "Number of cases"  # Y-axis label
  ) +
  facet_grid(diagnosis_binned ~ ., scales = "free_y") +  # Separate plots for each gender
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels vertically
    strip.text.y = element_text(size = 12)  # Increase size of facet labels
  )

# Ensure date_of_diagnosis is in Date format
df <- df %>%
  mutate(date_of_diagnosis = as.Date(date_of_diagnosis))

# Calculate the number of cases by year
df_summary <- df %>%
  mutate(year = floor_date(date_of_diagnosis, unit = "year")) %>%
  group_by(year) %>%
  summarise(ncas = n(), .groups = 'drop')

# Calculate cumulative cases
df_summary <- df_summary %>%
  arrange(year) %>%
  mutate(cum_cases = cumsum(ncas))

# Plot
ggplot() +
  theme_bw() +
  geom_histogram(data = df_summary, mapping = aes(x = year, weight = ncas), binwidth = 365, fill = "lightgreen", color = "black", linewidth = 0.1) +
  geom_line(data = df_summary, mapping = aes(x = year, y = cum_cases / 15), color = "red") + 
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y"), name = "Calendar Date") +
  scale_y_continuous(
    limits = c(0, max(df_summary$ncas) + 10), 
    name = "Number of Cases",
    sec.axis = sec_axis(~ . * 15, name = "Cumulative Number of Cases")
  ) +
  labs(
    title = "Incident Cases of Lymphoma",
    x = "Calendar Date",
    y = "Number of Cases"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)  # Rotate x-axis labels vertically
  )

# Ensure date_of_diagnosis is in Date format
df <- df %>%
  mutate(date_of_diagnosis = as.Date(date_of_diagnosis))

# Calculate the number of cases by quarter
df_summary <- df %>%
  mutate(quarter = floor_date(date_of_diagnosis, unit = "quarter")) %>%
  group_by(quarter) %>%
  summarise(ncas = n(), .groups = 'drop')

# Calculate cumulative cases
df_summary <- df_summary %>%
  arrange(quarter) %>%
  mutate(cum_cases = cumsum(ncas))

# Plot
ggplot() +
  theme_bw() +
  geom_histogram(data = df_summary, mapping = aes(x = quarter, weight = ncas), binwidth = 90, fill = "lightgreen", color = "black", linewidth = 0.1) +
  geom_line(data = df_summary, mapping = aes(x = quarter, y = cum_cases / 15), color = "red") + 
  scale_x_date(
    breaks = date_breaks("3 months"),  # Breaks every 3 months for quarterly intervals
    labels = date_format("%b %Y"),     # Format as "Month Year" (e.g., "Jan 2024")
    name = "Calendar Date"
  ) +
  scale_y_continuous(
    limits = c(0, max(df_summary$ncas) + 2), 
    name = "Number of Cases",
    sec.axis = sec_axis(~ . * 15, name = "Cumulative Number of Cases")
  ) +
  labs(
    title = "Incident Cases of Lymphoma",
    x = "Calendar Date",
    y = "Number of Cases"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)  # Rotate x-axis labels vertically
  )

