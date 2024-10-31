
# Install packages
install.packages("readxl")
install.packages("dplyr") 
install.packages("lubridate") 
install.packages("tidyr")
install.packages("git2r")


# Load the packages
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(git2r)

# Import the specific sheet from the Excel file
file_path <- "/Users/bkb17/Library/CloudStorage/OneDrive-DukeUniversity/Just Environments Program/Projects/covid_mororatia/jowers_state_evic_policies.xlsx" # Update the file path to the correct location
sheet_name <- "Clean Data for Import"

# Read the sheet into a data frame
evic_policy_data <- read_excel(
                               path = file_path, 
                               sheet = sheet_name, 
                               col_types = c(
                                 "text",
                                 "text", 
                                 "text", 
                                 "text",
                                 "text", 
                                 "text", 
                                 "date",   
                                 "date",   
                                 "date" 
                               ))

# View the data
head(evic_policy_data)

# Rename columns to make them easier to use
evic_policy_data <- evic_policy_data %>%
  rename(
    Adoption_Date = `Adoption Date if Retroactive`,
    First_Date_Effect = `Updated First Date of Effect`,
    Last_Date_Effect = `Updated Last Date of Effect`
  )

# Clean the data by converting renamed columns to Date format
evic_policy_data <- evic_policy_data %>%
  mutate(
    # Convert 'Adoption_Date', 'First_Date_Effect', and 'Last_Date_Effect' to Date format
    Adoption_Date = as.Date(Adoption_Date, format = "%Y-%m-%d"),
    First_Date_Effect = as.Date(First_Date_Effect, format = "%Y-%m-%d"),
    Last_Date_Effect = as.Date(Last_Date_Effect, format = "%Y-%m-%d")
  )

# Use case_when to conditionally update 'First_Date_Effect' where 'Retroactive' == "R"
evic_policy_data <- evic_policy_data %>%
  mutate(
    First_Date_Effect = case_when(
      Retroactive == "R" & !is.na(Retroactive) ~ Adoption_Date, # Replace with 'Adoption_Date' if Retroactive is "R"
      TRUE ~ First_Date_Effect # Keep original value otherwise
    )
  )

# View the cleaned and updated data
head(evic_policy_data)

# Drop the Retroactive and Adoption Date variables
evic_policy_data <- evic_policy_data %>%
  select(-Retroactive, -Adoption_Date)

# Define a helper function to check all pairs within each state group
flag_gaps_and_overlaps <- function(df) {
  n <- nrow(df)
  # Initialize flag columns with 0s
  df$flag <- 0
  df$component <- 0
  
  if (n <= 1) return(df)
  
  # First sort by First_Date_Effect to ensure chronological order
  df <- df[order(df$First_Date_Effect), ]
  
  # Assign components
  current_component <- 1
  df$component[1] <- current_component
  
  for (i in 2:n) {
    # Check relationship with previous policy periods
    gaps <- numeric()
    for (j in 1:(i-1)) {
      if (!is.na(df$Last_Date_Effect[j]) && !is.na(df$First_Date_Effect[i])) {
        gap <- as.numeric(difftime(df$First_Date_Effect[i], df$Last_Date_Effect[j], units = "days"))
        gaps <- c(gaps, gap)
      }
    }
    
    # If any previous period overlaps or is consecutive, use its component
    if (any(gaps <= 1, na.rm = TRUE)) {
      prev_components <- unique(df$component[1:(i-1)][gaps <= 1])
      df$component[i] <- min(prev_components)
      # Update all related components to the minimum component number
      df$component[df$component %in% prev_components] <- min(prev_components)
    } else {
      # If there's a gap > 1 day, start a new component
      current_component <- current_component + 1
      df$component[i] <- current_component
    }
  }
  
  # Flag rows that are part of a component with more than one policy
  component_counts <- table(df$component)
  df$flag <- ifelse(component_counts[as.character(df$component)] > 1, 1, 0)
  
  return(df)
}


# Process the overlaps and consecutive day gaps in the policy application data in two steps
# Step 1: Flag all connected periods
flagged_data <- evic_policy_data %>%
  group_by(State, `State Abbreviation`, `State FIPS Code`) %>%
  group_modify(~flag_gaps_and_overlaps(.x)) %>%
  ungroup()

# Step 2: Create collapsed rows for each component
final_evic_data <- flagged_data %>%
  group_by(State, `State Abbreviation`, `State FIPS Code`, component) %>%
  summarise(
    First_Date_Effect = min(First_Date_Effect, na.rm = TRUE),
    Last_Date_Effect = max(Last_Date_Effect, na.rm = TRUE),
    policies_collapsed = n(),
    should_collapse = any(flag == 1),
    .groups = 'drop'
  ) %>%
  # Replace Inf or -Inf with NA, using if_else to preserve Date class
  mutate(
    First_Date_Effect = if_else(is.infinite(First_Date_Effect), as.Date(NA), First_Date_Effect),
    Last_Date_Effect = if_else(is.infinite(Last_Date_Effect), as.Date(NA), Last_Date_Effect)
  ) %>%
  # Only collapse rows where flag = 1, keep others as is
  group_by(State, `State Abbreviation`, `State FIPS Code`) %>%
  mutate(
    policies_collapsed = if_else(should_collapse, policies_collapsed, 1)
  ) %>%
  select(-component, -should_collapse) %>%
  arrange(State, First_Date_Effect)


# View the updated data
head(final_evic_data)

# Add a variable to indicate if the row is the first, second, etc., policy in each state using the state abbreviation
new_evic_policy_data <- final_evic_data %>%
  # Group by State (this ensures row numbering restarts within each state)
  group_by(State, `State Abbreviation`, `State FIPS Code`) %>%
  # Create a policy number like 'CA-1', 'CA-2', etc., specific to each state
  mutate(policy_number = paste0(row_number())) %>%
  # Ungroup after creating the policy number
  ungroup() %>%
  select(-policies_collapsed)

# View the updated data with the state-based policy number
head(new_evic_policy_data)

# Check the structure to verify the added column
str(new_evic_policy_data)

# Save new_evic_policy_data as CSV
write.csv(new_evic_policy_data, "new_evic_policy_data.csv", row.names = FALSE)

# Pivot to wide format, spreading policies across columns
wide_data <- new_evic_policy_data %>%
  pivot_wider(
    names_from = policy_number,    # Use numeric policy number (1, 2, 3) as the column suffix
    values_from = c(First_Date_Effect, Last_Date_Effect) # Values to spread
  )

# View the transformed wide data
head(wide_data)

# Check the structure to verify the wide format
str(wide_data)

# Save wide_data as CSV
write.csv(wide_data, "wide_data.csv", row.names = FALSE)


##### Git Operations #####
# Initialize repository connection
repo <- repository(".")

# Stage changes
add(repo, ".")

# Commit changes
commit(repo, "Updated eviction policy data analysis")

# Push to GitHub using upstream credentials
push(repo, "origin", "refs/heads/master", set_upstream = TRUE)