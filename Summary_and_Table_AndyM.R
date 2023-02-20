# Load libraries
library(dplyr)
library(knitr)

# Load the dataframe of the crashes into a variable "crashes_df"
crashes_df <- read.csv("Desktop/INFO201/exploratory-analysis-Andy427/Airplane_Crashes_and_Fatalities_Since_1908.csv")

# Creates the summary of variables
summary_info <- list()
summary_info$num_crashes <- nrow(crashes_df)
summary_info$max_fatalities <- crashes_df %>%
  summarise(Fatalities = max(Fatalities, na.rm = T)) %>% pull(Fatalities)
summary_info$avg_fatalities <- crashes_df %>%
  summarise(Fatalities = mean(Fatalities, na.rm = T)) %>% pull(Fatalities)
summary_info$avg_fatality_rate <- crashes_df %>% summarise(avg_fatality_rate = sum(Fatalities, na.rm = T)/sum(Aboard, na.rm = T)) %>% pull(avg_fatality_rate)
summary_info$unqiue_operators <- crashes_df %>%
  summarise(Operator = n_distinct(Operator, na.rm = T)) %>%
  pull(Operator)


# Creates a table of the top 10 fatalities and the type of airplane it occurred in.
table <- crashes_df %>% group_by(Type, Fatalities) %>% summarise(Fatalities = max(Fatalities)) %>% arrange(desc(Fatalities)) %>% head(10)

# Display the table
kable(table)
