install.packages("treemap")
library(treemap)
library(dplyr)
library(stringr)

data = read.csv("C:\\Users\\14254\\OneDrive\\Desktop\\info201\\exploratory-analysis-Andy427\\Airplane_Crashes_and_Fatalities_Since_1908.csv",
                stringsAsFactors = FALSE)
# In your report you must describe why you included the chart (e.g., 
# what it attempts to seeks to express), and what information it reveals.
# My chart is going to be a treemap which allows you to see where a plane had the most frequent crashes in. Only the top 40 countries were accounted for with the most frequent crashes since the treemap would be difficult to view from all of the countries as there were around a hundred.

# create a list of states to replace all states with Unites States in the data for data preprocessing
states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland","Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

state_acronyms <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")


# Extract the country using str_extract()
# assuming the data is in the format "city, country" This code extracts the last section of the alphabetic section "country", ie all characters after the comma. 
countries <- str_extract(data$Location, "[[:alpha:]]+$")

# Replace matching strings in countries with "United States" that match with states
countries <- str_replace_all(countries, setNames(rep("United States", length(states)), states))
# Replace matching strings in countries with "United States" that match with state_acronyms
countries <- str_replace_all(countries, setNames(rep("United States", length(state_acronyms)), state_acronyms))

# Count the number of occurrences of each location
freq <- table(countries)




df <- data.frame(table(countries))

top_n <- df %>%
  arrange(desc(Freq)) %>%
  top_n(40)

# create a treemap with the frequency of each country
treemap(top_n,
        index=c("countries"),
        vSize="Freq",
        type="index",
        title = "Top 40 Countries of Aviation Crashes Based on Frequecny",
        fontsize.labels = 12)

#It is important to note that it is beyond my scope to parce out the "York" in the chart which is due to some issues in terms of formatting for New York within the data when filtering to the extent of my knowledge from this class was utilized. Further there are some missing label names within the treemap and once again, beyond the scope of what I have learned in order to make those appear on the treemap.

