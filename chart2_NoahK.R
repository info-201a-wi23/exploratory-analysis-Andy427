# Chart 2
# Loading libraries
library("dplyr")
library("stringr")
library("ggplot2")
library("scales")

data = read.csv("Airplane_Crashes_and_Fatalities_Since_1908.csv", stringsAsFactors = FALSE)

# In your report you must describe why you included the chart (e.g., 
# what it attempts to seeks to express), and what information it reveals.
# This chart is going to look at the different aircraft types and determine which has the most recorded fatalities.

####

# Add a new column only including the aircraft "make" of each type
data <- data %>% mutate(Make = sub(" .*", "", tolower(Type)))

# Create a new data frame of the total fatalities for each aircraft make
fatalities_per_make <- data %>% group_by(Make) %>% summarize(total_fatalities = sum(Fatalities, na.rm = TRUE))

# Create a new data frame of the total fatalities for each aircraft type
fatalities_per_type <- data %>% group_by(Type) %>% summarize(total_fatalities = sum(Fatalities, na.rm = TRUE))

####

# Create a new data frame of the top 20 aircraft makes with the most fatalities
top20_make <- fatalities_per_make %>% arrange(desc(total_fatalities)) %>% slice(1:20)

# Create a new data frame of the top 20 aircraft types with the most fatalities
top20_type <- fatalities_per_type %>% arrange(desc(total_fatalities)) %>% slice(1:20)

####

#Pulling the aircraft make with the most fatalities
make_highest_fatalities <- fatalities_per_make %>% filter(total_fatalities == max(total_fatalities)) %>% pull(Make)

# Pulling the aircraft type with the most fatalities
type_highest_fatalities <- fatalities_per_type %>% filter(total_fatalities == max(total_fatalities)) %>% pull(Type)

####

#Graph
ggplot(data = top20_make, aes(x = total_fatalities, y = Make)) +
  geom_bar(stat="identity", width=0.75) + labs(title = "Top 20 Aircraft Makes with the most Deaths", x = "Total Deaths", y = "Aircraft Makes") + scale_x_continuous(labels = label_number_si()) + theme_minimal()

ggplot(data = top20_type, aes(x = total_fatalities, y = Type)) +
   geom_bar(stat="identity", width=0.75) + labs(title = "Top 20 Aircraft Types # with the most Deaths", x = "Total Deaths", y = "Aircraft Types") + scale_x_continuous(labels = label_number_si()) + theme_minimal()



