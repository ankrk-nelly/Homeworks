# Part 1: 
# Load the dataset
crime_data <- read.csv("C:\\Users\\nelly\\Desktop\\Classes\\Masters degree\\Spring 2025\\Data Visualization\\Homeworks\\crime_data.csv")

# Check the first 5 rows
head(crime_data, 5)

# Identify columns with missing values and their counts
missing_values <- colSums(is.na(crime_data))
print(missing_values)

# Drop columns where more than 50% of the data is missing
threshold <- nrow(crime_data) * 0.5
crime_cleaned <- crime_data[, colSums(is.na(crime_data)) < threshold]

# Save the cleaned dataset
write.csv(crime_cleaned, "crime_dataset_cleaned.csv", row.names = FALSE)

# Print the cleaned dataset (first 5 rows)
head(crime_cleaned, 5)

# Convert DATE.OCC to datetime
crime_cleaned$DATE.OCC <- as.POSIXct(crime_cleaned$DATE.OCC, format="%m/%d/%Y %I:%M:%S %p")

# Extract year, month, and day
crime_cleaned$Year <- format(crime_cleaned$DATE.OCC, "%Y")
crime_cleaned$Month <- format(crime_cleaned$DATE.OCC, "%m")
crime_cleaned$Day <- format(crime_cleaned$DATE.OCC, "%d")

# Create a new column for the hour
crime_cleaned$Hour <- substr(sprintf("%04d", crime_cleaned$TIME.OCC), 1, 2)

# Save the updated dataset
write.csv(crime_cleaned, "crime_dataset_cleaned.csv", row.names = FALSE)

# Print the updated dataset (first 5 rows)
head(crime_cleaned, 5)

# Filter for crimes in 2023
crime_2023 <- subset(crime_cleaned, Year == 2023)

# Further filter for BURGLARY crimes
burglary_2023 <- subset(crime_2023, Crm.Cd.Desc == "BURGLARY")

# Save the filtered dataset
write.csv(burglary_2023, "burglary_2023.csv", row.names = FALSE)
head(burglary_2023, 5)


# Group by AREA NAME and calculate total crimes and average victim age
crime_summary <- crime_cleaned %>%
  group_by(AREA.NAME) %>%
  summarise(
    Total_Crimes = n(),
    Avg_Victim_Age = mean(Vict.Age, na.rm = TRUE) # Handle missing victim age
  ) %>%
  arrange(desc(Total_Crimes))  # Sort by total crimes in descending order

# Save the summarized data
write.csv(crime_summary, "crime_summary_by_area.csv", row.names = FALSE)

# Print the summary table
print(crime_summary)


# Part 3: 
# Group by Month and count crimes
monthly_crimes <- crime_cleaned %>%
  group_by(Month) %>%
  summarise(Crime_Count = n())

# Print the results
print(monthly_crimes)

# Count crimes where a weapon was used
weapon_crimes <- sum(!is.na(crime_cleaned$Weapon_Used_Cd))

# Print the result
print(weapon_crimes)

# Group by Premis Desc and count crimes
premis_stats <- crime_cleaned %>%
  group_by(Premis.Desc) %>%
  summarise(Crime_Count = n())

print(premis_stats)

# Part 4:
# Convert Crm.Cd.Desc to character if needed
crime_cleaned$Crm.Cd.Desc <- as.character(crime_cleaned$Crm.Cd.Desc)

# Create Severity Score column
crime_cleaned$Severity_Score <- 1
crime_cleaned$Severity_Score[!is.na(crime_cleaned$Weapon_Used_Cd)] <- 6
crime_cleaned$Severity_Score[crime_cleaned$Crm.Cd.Desc == "BURGLARY"] <- 4

# Check column names
names(crime_cleaned)

# Group by AREA.NAME and calculate total severity score
severity_by_area <- crime_cleaned %>%
  group_by(AREA.NAME) %>%
  summarise(Total_Severity = sum(Severity_Score, na.rm = TRUE)) # Ignore missing values

# Save the results
write.csv(severity_by_area, "severity_by_area.csv", row.names = FALSE)
print(severity_by_area)