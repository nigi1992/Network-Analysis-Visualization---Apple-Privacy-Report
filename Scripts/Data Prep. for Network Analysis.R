# installing packages
install.packages("ndjson")
install.packages("dplyr")
install.packages("ggplot2")
library(ndjson)
library(dplyr)
library(ggplot2)

# Define the file path
file_path <- "/Users/nicolaswaser/New-project-GitHub-first/R/Network-Analysis-Visualization---Apple-Privacy-Report/Input Data"

# Load the NDJSON file into R
data <- ndjson::stream_in(paste0(file_path, "/App_Privacy_Report_v4_2024-10-19T11_08_58.ndjson"))

# Display the first few rows of the loaded data
head(data)
class(data)

# Tibble data
library(tibble)
is_tibble(data) # FALSE
data_tibble <- as_tibble(data)
is_tibble(data_tibble)
head(data_tibble)
class(data_tibble)

# Unnest data
install.packages("tidyr")
library(tidyr)
column_names <- names(data_tibble)
print(column_names)
data_unnested <- unnest(data_tibble, cols = c("accessCount", "accessor.identifier", "accessor.identifierType", "category", 
                                              "identifier", "kind", "timeStamp", "type", "outOfProcess", "bundleID", "context", 
                                              "contextVerificationType", "domain", "domainClassification", "domainOwner", "domainType", 
                                              "firstTimeStamp", "hits", "initiatedType"))
head(data_unnested)
class(data_unnested)
summary(data_unnested)
glimpse(data_unnested)

# Convert the data to a list
#data_list <- as.list(data)
#head(data_list)
# Flatten the data if needed
#flat_data <- ndjson::flatten(data)
# Inspect the structure of the flattened data
#str(flat_data)

## Filtering and aggregating the data
# Application/Website with the most accesses
data_unnested %>%
  group_by(accessor.identifier) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

# type of event: "access" or "networkActivity"
data_unnested %>%
  group_by(type) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

# specified domain accessed during network activity
data_unnested %>%
  group_by(domain) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

# application responsible for network activity
data_unnested %>%
  group_by(bundleID) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

# domain owner
data_unnested %>%
  group_by(domainOwner) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

# Next up:
# filter out NA's
# filter out variables that are not needed
# create a matrix of the data??
# Identify Entities and Relationships
# Structure Data for Network Analysis
# Analyze the Network
# Visualize the Network

