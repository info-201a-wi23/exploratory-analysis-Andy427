library("ggplot2")
library("dplyr")

data = read.csv("Airplane_Crashes_and_Fatalities_Since_1908.csv",
                stringsAsFactors = FALSE)

# In your report you must describe why you included the chart (e.g., 
# what it attempts to seeks to express), and what information it reveals.
# My chart is going to look at each year's fatalities.

# Add a date column for only year
data <- data %>% mutate(crash_years = substring(Date, 7))

# Create a new data frame for grouping all fatalities by year.
fatailites_by_year <- data %>% group_by(crash_years) %>% summarise("Total_fatalities" = sum(Fatalities, na.rm = TRUE))

# Graph
ggplot(data = fatailites_by_year, aes(x = crash_years, y = Total_fatalities, group = 1)) +
  geom_point(aes(color = "Actual")) +
  geom_smooth(aes(color = "Estimate / Trend")) +
  labs(title = "Total fatalities from year 1908 to 2009",
       x = "Years",
       y = "Total fatalities",
       color = "Lines") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(breaks = seq(1908, 2009, 5)) +
  scale_y_continuous(breaks = seq(0, 3000, 500))
  

  