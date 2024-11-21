# Preparation -------------------------------------------------------------

# installing packages
install.packages("ndjson")
install.packages("dplyr")
install.packages("ggplot2")
library(ndjson)
library(dplyr)
library(ggplot2)
library(tidyverse) # For data manipulation

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

# Showing the Rankings for overview --------------------------------

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


# Visualization of  Top20's -----------------------------------------------

# Plot the number of accesses by domain
# specified domain accessed during network activity
domain_plot <- data_unnested %>%
  group_by(domain) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  filter(is.na(domain) == FALSE) %>% # filter out NA's
  filter(total_accesses >= 5) %>% # filter domains with less than 5 accesses
  ggplot(aes(x = reorder(domain, total_accesses), y = total_accesses)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Number of accesses by domain",
       x = "Domain",
       y = "Number of accesses") +
  theme_bw() 
#ggsave("Output/Number of accesses by domain.png", plot=domain_plot, width = 16, height = 12)

# only the top 20 domains
domain_plot_limited <- data_unnested %>%
  group_by(domain) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  filter(is.na(domain) == FALSE) %>%
  filter(total_accesses >= 5) %>%
  head(20) %>%  # Only take top 20
  ggplot(aes(x = reorder(domain, total_accesses), y = total_accesses)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 20 domains by number of accesses",
       x = "Domain",
       y = "Number of accesses") +
  theme_bw()
ggsave("Output/Top_20_domains_by_accesses.png", plot = domain_plot_limited, width = 16, height = 12)

# application responsible for network activity
bundleID_plot <- data_unnested %>%
  group_by(bundleID) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  filter(is.na(bundleID) == FALSE) %>% # filter out NA's
  filter(total_accesses >= 10) %>% # filter domains with less than 10 accesses
  ggplot(aes(x = reorder(bundleID, total_accesses), y = total_accesses)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Number of accesses by application",
       x = "Application",
       y = "Number of accesses") +
  theme_bw() 
#ggsave("Output/Number of accesses by app.png", plot=bundleID_plot, width = 16, height = 12)

# only the top 20 domains
bundleID_plot_limited <- data_unnested %>%
  group_by(bundleID) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  filter(is.na(bundleID) == FALSE) %>%
  filter(total_accesses >= 10) %>%
  head(20) %>%  # Only take top 20
  ggplot(aes(x = reorder(bundleID, total_accesses), y = total_accesses)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 20 Apps by number of accesses",
       x = "App",
       y = "Number of accesses") +
  theme_bw()
ggsave("Output/Top_20_apps_by_accesses.png", plot = bundleID_plot_limited, width = 16, height = 12)

# domain owner
domainOwner_plot <- data_unnested %>%
  group_by(domainOwner) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  filter(!is.na(domainOwner) & domainOwner != "") %>% # filter out NA's and empty strings
  filter(is.na(domainOwner) == FALSE & total_accesses >= 1) %>% # filter out NA's and domains with less than 5 accesses)
  ggplot(aes(x = reorder(domainOwner, total_accesses), y = total_accesses)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Number of accesses by domain owner",
       x = "Domain Owner",
       y = "Number of accesses") +
  theme_bw()
#ggsave("Output/Number of access by dOwner.png", plot=domainOwner_plot, width = 16, height = 12)
