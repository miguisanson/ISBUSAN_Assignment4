# R Script for Analyzing 120 Years of Olympic History
#
# Instructions for RStudio:
# 1. Make sure you have the 'athlete_events.csv' file in the specified folder.
# 2. Install the necessary package by running this line in the RStudio console:
#    install.packages("tidyverse")
# 3. Run this script line-by-line by pressing Ctrl+Enter (or Cmd+Enter on Mac) on each line,
#    or run the whole script at once.

#--------------------------------------------------------------------------
# PHASE 1: DATA UNDERSTANDING & PREPARATION
#--------------------------------------------------------------------------

# Load the tidyverse library, which includes powerful tools for data manipulation (dplyr)
# and visualization (ggplot2).
library(tidyverse)

# Load the dataset from the CSV file using the full path you provided.
# Note: R uses forward slashes "/" for file paths, even on Windows.
olympics_data <- read_csv("E:/Github School Projects/ISBUSAN_Assignment4/athlete_events.csv")

# --- Initial Exploration ---

# Look at the first few rows of the data to understand its structure
head(olympics_data)

# Get a summary of the dataset, including column names, data types, and some sample data.
glimpse(olympics_data)

# Get a statistical summary of numerical columns and see where we have missing values (NA's).
summary(olympics_data)


# --- Data Cleaning ---

# The 'Medal' column has NA for non-medalists. This is okay, we will filter these out
# when we need to. The 'Age', 'Height', and 'Weight' columns also have NAs. We will handle
# these on a question-by-question basis.

# For this analysis, the dataset is quite clean, so no major cleaning is needed upfront.
# We will proceed to answer the questions.


#--------------------------------------------------------------------------
# PHASE 2: ANSWERING YOUR KEY QUESTIONS
#--------------------------------------------------------------------------

# --- Question 1: Which country has won the most gold medals? ---

gold_medals_by_country <- olympics_data %>%
  # Step 1: Filter to only include rows where the Medal is 'Gold'.
  # The '==' is for exact matching. We use !is.na() to first make sure the medal is not a missing value.
  filter(!is.na(Medal) & Medal == "Gold") %>%
  
  # Step 2: Group the data by the 'NOC' column (National Olympic Committee).
  group_by(NOC) %>%
  
  # Step 3: Count the number of gold medals for each country. We'll name the new count column 'GoldMedalCount'.
  summarise(GoldMedalCount = n()) %>%
  
  # Step 4: Sort the results in descending order to see the top countries.
  arrange(desc(GoldMedalCount))

# Print the top 10 countries with the most gold medals
print("Top 10 Countries by Gold Medals:")
print(head(gold_medals_by_country, 10))


# --- Question 2: Who are the top 5 athletes who have won the most medals (of any kind)? ---

top_athletes <- olympics_data %>%
  # Step 1: Filter out any rows where the Medal is a missing value (NA).
  filter(!is.na(Medal)) %>%
  
  # Step 2: Group by the athlete's name.
  group_by(Name) %>%
  
  # Step 3: Count the total number of medals for each athlete.
  summarise(TotalMedals = n()) %>%
  
  # Step 4: Arrange in descending order and select the top 5.
  arrange(desc(TotalMedals)) %>%
  head(5)

# Print the top 5 athletes
print("Top 5 Athletes by Total Medals:")
print(top_athletes)


# --- Question 3: How has the number of female participants changed over time? ---

female_participants_over_time <- olympics_data %>%
  # Step 1: Filter for female athletes.
  filter(Sex == "F") %>%
  
  # Step 2: Get unique athletes for each year to avoid counting an athlete multiple times
  # if they participated in multiple events in the same year.
  distinct(Name, Year) %>%
  
  # Step 3: Group by Year.
  group_by(Year) %>%
  
  # Step 4: Count the number of unique female athletes per year.
  summarise(FemaleParticipantCount = n()) %>%
  
  # Step 5: Sort by year to see the trend clearly.
  arrange(Year)

# Print the first 10 and last 10 years to see the change
print("Female Participants Over Time (First 10 records):")
print(head(female_participants_over_time, 10))
print("Female Participants Over Time (Last 10 records):")
print(tail(female_participants_over_time, 10))


# --- Question 4: What is the distribution of medals for a specific country? (e.g., USA) ---

# You can change "USA" to any other NOC code (e.g., "CHN", "GBR", "CAN")
country_to_analyze <- "USA"

medal_distribution <- olympics_data %>%
  # Step 2: Filter for the specific country's NOC.
  filter(NOC == country_to_analyze) %>%
  
  # Step 3: Filter out non-medal winners.
  filter(!is.na(Medal)) %>%
  
  # Step 4: Group by the Medal type.
  group_by(Medal) %>%
  
  # Step 5: Count the number of medals in each category.
  summarise(Count = n()) %>%
  
  # Optional: Reorder the medals logically instead of alphabetically
  mutate(Medal = factor(Medal, levels = c("Gold", "Silver", "Bronze"))) %>%
  arrange(Medal)


# Print the medal distribution for the chosen country
print(paste("Medal Distribution for", country_to_analyze, ":"))
print(medal_distribution)


# --- Question 5: What is the average age of athletes for each sport? ---

avg_age_by_sport <- olympics_data %>%
  # Step 1: Filter out rows where Age is NA, as they can't be used in an average.
  filter(!is.na(Age)) %>%
  
  # Step 2: Group by Sport.
  group_by(Sport) %>%
  
  # Step 3: Calculate the average age for each sport and round it to 1 decimal place.
  summarise(AverageAge = round(mean(Age), 1)) %>%
  
  # Step 4: Sort by average age.
  arrange(desc(AverageAge))

# Print the sports with the highest and lowest average ages
print("Top 5 Sports with Highest Average Athlete Age:")
print(head(avg_age_by_sport, 5))

print("Top 5 Sports with Lowest Average Athlete Age:")
print(tail(avg_age_by_sport, 5))


#--------------------------------------------------------------------------
# PHASE 3: EXPORTING DATA FOR TABLEAU
#--------------------------------------------------------------------------

# Use the write_csv() function to save our summary data frames to new files.
# These files will be saved in the specific directory you provided.

write_csv(gold_medals_by_country, "E:/Github School Projects/ISBUSAN_Assignment4/gold_medals_by_country.csv")
write_csv(top_athletes, "E:/Github School Projects/ISBUSAN_Assignment4/top_5_athletes.csv")
write_csv(female_participants_over_time, "E:/Github School Projects/ISBUSAN_Assignment4/female_participants_over_time.csv")
write_csv(medal_distribution, "E:/Github School Projects/ISBUSAN_Assignment4/usa_medal_distribution.csv")
write_csv(avg_age_by_sport, "E:/Github School Projects/ISBUSAN_Assignment4/average_age_by_sport.csv")

# Print a message to confirm that the files have been saved.
print("Successfully exported 5 summary tables to CSV files for Tableau.")
