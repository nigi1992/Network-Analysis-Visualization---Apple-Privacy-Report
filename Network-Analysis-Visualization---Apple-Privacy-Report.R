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
  print(n=164)

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


# First Vizualization
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
ggsave("Output/Number of accesses by domain.png", plot=domain_plot, width = 16, height = 12)

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
ggsave("Output/Number of accesses by app.png", plot=bundleID_plot, width = 16, height = 12)

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
ggsave("Output/Number of access by dOwner.png", plot=domainOwner_plot, width = 16, height = 12)


# Creating new data sets that split into type of event
data_unnested_type_access <- data_unnested %>%
  filter(type == "access") %>%
  select_if(~ !all(is.na(.)))
  
data_unnested_type_networkActivity <- data_unnested %>%
  filter(type == "networkActivity") %>%
  select_if(~ !all(is.na(.)))

# df only with columns "firstTimeStamp", "timeStamp", "domain", "domainOwner", "bundleID", "hits"
data_unnested_type_networkActivity_small <- data_unnested %>%
  filter(type == "networkActivity") %>%
  select(firstTimeStamp, timeStamp, bundleID, domain, domainOwner, hits) #%>%

# Check the structure of the new data sets
glimpse(data_unnested_type_networkActivity_small)

# number of unique domains
length(unique(data_unnested_type_networkActivity_small$domain))
# number of unique bundleID
length(unique(data_unnested_type_networkActivity_small$bundleID))
# number of unique domainOwner
length(unique(data_unnested_type_networkActivity_small$domainOwner))


getwd()
# Save as a tab-delimited text file
write.table(data_unnested_type_networkActivity_small, "Input Data/NetworkAct_df.txt", sep = "\t", row.names = FALSE)
write.table(data_unnested_type_networkActivity_small, "Input Data/NetworkAct_df.csv", sep = ",", row.names = FALSE)

# Next up:
# filter out NA's?
# create a matrix of the data??
# Identify Entities and Relationships
# Structure Data for Network Analysis
# Nodes + Edges
# Analyze the Network
# Visualize the Network

library(igraph)
install.packages("ggraph")# For network graph creation
library(ggraph)    # For network graph visualization
library(tidyverse) # For data manipulation

# Data preparation
# Select relevant columns
network_data <- data_unnested_type_networkActivity_small %>%
  select(bundleID, domain, hits)

# Create edges between user, bundleID, and domain
edges_user_app <- network_data %>%
  distinct(bundleID) %>%
  mutate(from = "User", to = bundleID) %>%
  select(from, to)
  
edges_app_domain <- network_data %>%
  distinct(bundleID, domain) %>%
  rename(from = bundleID, to = domain)

# Combine all edges
all_edges <- bind_rows(edges_user_app, edges_app_domain)

# Ensure column names are consistent
#colnames(all_edges) <- c("from", "to")

# Create a graph object
g <- graph_from_data_frame(all_edges, directed = FALSE)

print(g)

# Verify edges in the graph
edge_list <- as_data_frame(g, what = "edges")

# Prepare edge weights
edge_weights <- network_data %>%
  group_by(bundleID, domain) %>%
  summarise(weight = sum(hits), .groups = "drop")

# Match edge weights to the graph edges
edge_list <- edge_list %>%
  left_join(edge_weights, by = c("from" = "bundleID", "to" = "domain"))

# Replace missing weights with 1
edge_list <- edge_list %>%
  mutate(weight = ifelse(is.na(weight), 1, weight))

# Assign weights to the graph
E(g)$weight <- edge_list$weight

# Verify the updated graph
print(E(g)$weight)

# first try at network visualization
ggraph(g, layout = "fr") + 
  geom_edge_link(aes(width = weight), alpha = 0.7) + 
  geom_node_point(size = 1) + 
  geom_node_text(aes(label = name), repel = TRUE, size = 3) + 
  theme_void() + 
  theme(legend.position = "bottom") +
  labs(title = "Network Activity Visualization (Applications and Domains)")


# corrections
update.packages("ggplot2")
update.packages("ggraph")
library(ggplot2)
library(ggraph)

#increase overlaps
#geom_node_text(aes(label = name), repel = TRUE, size = 2, max.overlaps = 200)

# Filter out nodes with less than 5 connections
ggraph(g, layout = "fr") + 
  geom_edge_link(aes(width = weight), alpha = 0.7) + 
  geom_node_point(size = 1) + 
  geom_node_text(
    aes(label = ifelse(degree(g) > 5, name, "")), 
    repel = TRUE, size = 3
  ) + 
  theme_void() + 
  theme(legend.position = "bottom") +
  labs(title = "Filtered Network Visualization")

#increase canvas size
ggraph(g, layout = "fr") + 
  geom_edge_link(aes(width = weight), alpha = 0.7) + 
  geom_node_point(size = 1) + 
  geom_node_text(aes(label = name), repel = TRUE, size = 3, max.overlaps = 200) + 
  theme_void() + 
  theme(legend.position = "bottom") +
  labs(title = "Network Activity Visualization") #+
  #ggsave("network_plot.png", width = 16, height = 12)


### Next:
# Analyze the Network
# see if you have to change data set some more
# (ie. manually work on the data set to match domain with a DomainOwner: Facebook, Google, etc.)
# bei domains nachsehen, wie viel unique values
# vielleicht bei Domains oder bei bundleID auf Top20, 25 oder 50 reduzieren
# domains die nur Ip Adressen sind, rausfiltern, anderes unbrauchbares ebenfalls
# Fabian mundt schreiben
# besseres GPT Model benutzen

### again with only domains with more than 2 accesses  
# Count occurrences of each domain and filter
data_unnested_type_networkActivity_filtered <- data_unnested_type_networkActivity_small %>%
  group_by(domain) %>%
  filter(n() >= 2) %>%
  ungroup()

# Data preparation
# Select relevant columns
network_data2 <- data_unnested_type_networkActivity_filtered %>%
  select(bundleID, domain, hits)

# Create edges between user, bundleID, and domain
edges_user_app2 <- network_data2 %>%
  distinct(bundleID) %>%
  mutate(from = "User", to = bundleID) %>%
  select(from, to)

edges_app_domain2 <- network_data2 %>%
  distinct(bundleID, domain) %>%
  rename(from = bundleID, to = domain)

# Combine all edges
all_edges2 <- bind_rows(edges_user_app2, edges_app_domain2)

# Ensure column names are consistent
#colnames(all_edges2) <- c("from", "to")

# Create a graph object
g2 <- graph_from_data_frame(all_edges2, directed = FALSE)

print(g2)

# Verify edges in the graph
edge_list2 <- as_data_frame(g2, what = "edges")

# Prepare edge weights
edge_weights2 <- network_data2 %>%
  group_by(bundleID, domain) %>%
  summarise(weight = sum(hits), .groups = "drop")

# Match edge weights to the graph edges
edge_list2 <- edge_list2 %>%
  left_join(edge_weights2, by = c("from" = "bundleID", "to" = "domain"))

# Replace missing weights with 1
edge_list2 <- edge_list2 %>%
  mutate(weight = ifelse(is.na(weight), 1, weight))

# Assign weights to the graph
E(g2)$weight <- edge_list2$weight

# Verify the updated graph
print(E(g2)$weight)

# first try at network visualization
ggraph(g2, layout = "fr") + 
  geom_edge_link(aes(width = weight), alpha = 0.2) + 
  geom_node_point(size = 1) + 
  geom_node_text(aes(label = name), repel = TRUE, size = 3, max.overlaps = 200) + 
  theme_void() + 
  theme(legend.position = "bottom") +
  labs(title = "Network Activity Visualization (Applications and Domains)")

# Filter out nodes with less than 5 connections
ggraph(g2, layout = "fr") + 
  geom_edge_link(aes(width = weight), alpha = 0.2) + 
  geom_node_point(size = 1) + 
  geom_node_text(
    aes(label = ifelse(degree(g2) > 3, name, "")), 
    repel = TRUE, size = 3
  ) + 
  theme_void() + 
  theme(legend.position = "bottom") +
  labs(title = "Filtered Network Visualization")



### again with only top24 apps
data_unnested_type_networkActivity_top24apps <- data_unnested_type_networkActivity_small %>%
  group_by(bundleID) %>%
  filter(n() >= 20) %>%
  ungroup()

# Count occurrences of each domain and filter
Top24apps_no_Unique_Domains <- data_unnested_type_networkActivity_top24apps %>%
  group_by(domain) %>%
  filter(n() >= 2) %>%
  ungroup()

write.table(Top24apps_no_Unique_Domains, "Input Data/Top24_df.csv", sep = ",", row.names = FALSE)

# renaming the bundleID values to something more readable
Top24apps_no_Unique_Domains <- Top24apps_no_Unique_Domains %>%
  mutate(bundleID = case_when(
    bundleID == "com.9gag.ios.mobile" ~ "9GAG",
    bundleID == "ch.migros.m-go" ~ "Migros",
    bundleID == "ch.tutti.iphone" ~ "Tutti",
    bundleID == "co.bird.Ride" ~ "Bird",
    bundleID == "com.adobe.Adobe-Reader" ~ "Adobe Reader",
    bundleID == "com.apple.mobilemail" ~ "Apple Mail",
    bundleID == "com.burbn.instagram" ~ "Instagram",
    bundleID == "com.datacamp" ~ "DataCamp",
    bundleID == "com.duolingo.DuolingoMobile" ~ "Duolingo",
    bundleID == "com.google.Gmail" ~ "Gmail",
    bundleID == "com.google.ios.youtube" ~ "Youtube",
    bundleID == "com.google.Maps" ~ "Google Maps",
    bundleID == "com.linkedin.LinkedIn" ~ "LinkedIn",
    bundleID == "com.shallotgames.coffeegolf" ~ "Coffee Golf",
    bundleID == "com.spotify.client" ~ "Spotify",
    bundleID == "com.strava.stravaride" ~ "Strava",
    bundleID == "com.toyopagroup.picaboo" ~ "Snapchat",
    bundleID == "com.tripodsocial.apps.tandem" ~ "Tandem",
    bundleID == "company.thebrowser.ArcMobile2" ~ "Arc Search",
    bundleID == "de.spiegel.spon" ~ "DER SPIEGEL",
    bundleID == "net.whatsapp.WhatsApp" ~ "WhatsApp",
    bundleID == "org.whispersystems.signal" ~ "Signal",
    bundleID == "swiss.ricardo.iphone" ~ "Ricardo",
    bundleID == "tv.sf.iapp" ~ "Play SRF",
    TRUE ~ bundleID
  ))

# Data preparation
# Select relevant columns
network_data3 <- Top24apps_no_Unique_Domains %>%
  select(bundleID, domain, hits)

# Create edges between user, bundleID, and domain
edges_user_app3 <- network_data3 %>%
  distinct(bundleID) %>%
  mutate(from = "User", to = bundleID) %>%
  select(from, to)

edges_app_domain3 <- network_data3 %>%
  distinct(bundleID, domain) %>%
  rename(from = bundleID, to = domain)

# Combine all edges
all_edges3 <- bind_rows(edges_user_app3, edges_app_domain3)

# Create a graph object
g3 <- graph_from_data_frame(all_edges3, directed = FALSE)

print(g3)

# Verify edges in the graph
edge_list3 <- as_data_frame(g3, what = "edges")

# Prepare edge weights
edge_weights3 <- network_data3 %>%
  group_by(bundleID, domain) %>%
  summarise(weight = sum(hits), .groups = "drop")

# Match edge weights to the graph edges
edge_list3 <- edge_list3 %>%
  left_join(edge_weights3, by = c("from" = "bundleID", "to" = "domain"))

# Replace missing weights with 1
edge_list3 <- edge_list3 %>%
  mutate(weight = ifelse(is.na(weight), 1, weight))

# Assign weights to the graph
E(g3)$weight <- edge_list3$weight

# Verify the updated graph
print(E(g3)$weight)

# first try at network visualization
ggraph(g3, layout = "fr") + 
  geom_edge_link(aes(width = weight), alpha = 0.2) + 
  geom_node_point(size = 1) + 
  geom_node_text(aes(label = name), repel = TRUE, size = 3, max.overlaps = 200) + 
  theme_void() + 
  theme(legend.position = "bottom") +
  labs(title = "Network Activity Visualization (Applications and Domains)")

# Filter out nodes with less than 5 connections
ggraph(g3, layout = "fr") + 
  geom_edge_link(aes(width = weight), alpha = 0.2) + 
  geom_node_point(size = 1) + 
  geom_node_text(
    aes(label = ifelse(degree(g3) > 3, name, "")), 
    repel = TRUE, size = 3
  ) + 
  theme_void() + 
  theme(legend.position = "bottom") +
  labs(title = "Filtered Network Visualization")

# change coloring in Plot
# Create a visualization with different node colors
ggraph(g3, layout = "fr") + 
  geom_edge_link(aes(width = weight), alpha = 0.2) + 
  geom_node_point(aes(color = name == "User"), size = 1) +  # Color based on whether node is "User"
  scale_color_manual(values = c("orange", "red")) +  # False = orange, True = red
  geom_node_text(aes(label = name), repel = TRUE, size = 3, max.overlaps = 200) + 
  theme_void() + 
  theme(legend.position = "none") +  # Remove the legend
  labs(title = "Network Activity Visualization (Applications and Domains)")

# more advanced coloring
# Create a color mapping for nodes
V(g3)$node_color <- ifelse(V(g3)$name == "User", "red",  # User in red
                           ifelse(V(g3)$name %in% unique(edges_user_app3$to), "orange",  # Apps in orange
                                  "grey"))  # Domains in grey

# Create the visualization with the custom color mapping
ggraph(g3, layout = "fr") + 
  geom_edge_link(aes(width = weight), alpha = 0.2) + 
  geom_node_point(aes(color = node_color), size = 1) +
  scale_color_identity() +  # Use the actual colors specified
  geom_node_text(aes(label = name), repel = TRUE, size = 3, max.overlaps = 200) + 
  theme_void() + 
  theme(legend.position = "none") +
  labs(title = "Network Activity Visualization (Applications and Domains)")


# Network with center, middle ring and outer ring

############################################# Code rest #########################################
# Add hits as edge weights for the app-domain connections
edge_weights <- network_data %>%
  group_by(bundleID, domain) %>%
  summarise(weight = sum(hits), .groups = "drop")

E(g)$weight <- ifelse(is.na(match(paste(E(g)$from, E(g)$to), 
                                  paste(edge_weights$bundleID, edge_weights$domain))),
                      1, edge_weights$weight)

# Plot the network
ggraph(g, layout = "fr") + # "fr" layout for a force-directed graph
  geom_edge_link(aes(width = weight), alpha = 0.7) + # Edge thickness based on hits
  geom_node_point(size = 5) + # Node size
  geom_node_text(aes(label = name), repel = TRUE, size = 3) + # Node labels
  theme_void() + 
  theme(legend.position = "bottom") +
  labs(title = "Network Activity Visualization (Applications and Domains)")

### create matrix from website
matrix_networkActivity <- crossprod(table(data_unnested_type_networkActivity_small[2:3]))
diag(matrix_networkActivity) <- 0
matrix_df <- as.data.frame(matrix_networkActivity)
## useless?


#### trying with igraph!!!
# Create the test_nodes data frame with the name column as vertex names
test_nodes <- data.frame(name = c("A", "B", "C"), id = 1:3)
# Create the test_edges data frame
test_edges <- data.frame(from = c("A", "B"), to = c("B", "C"))
# Create the graph using the corrected test_nodes and test_edges data frames
graph_test <- graph_from_data_frame(d = test_edges, vertices = test_nodes, directed = FALSE)
# Plot the graph
plot(graph_test)

# Create nodes with unique vertex names
nodes <- data_unnested_type_networkActivity_small %>%
  select(bundleID, domain) %>%
  distinct() %>%
  unite(vertex_name, bundleID, domain, sep = "_") %>%
  mutate(id = row_number()) %>%
  select(vertex_name, id)

# Create edges with unique vertex names
edges <- data_unnested_type_networkActivity_small %>%
  select(bundleID, domain, hits) %>%
  unite(vertex_name, bundleID, domain, sep = "_") %>%
  group_by(vertex_name) %>%
  summarise(weight = sum(hits, na.rm = TRUE)) %>%
  ungroup()


# Ensure all vertex names in edges are present in nodes
edges <- edges %>%
  inner_join(nodes, by = "vertex_name")

# Create the graph
network_graph <- graph_from_data_frame(d = edges, directed = FALSE, vertices = nodes)

# Plot the graph
plot(network_graph)


#create nodes
nodes <- data_unnested_type_networkActivity_small %>%
  select(bundleID, domain) %>%
  distinct() %>%
  mutate(id = row_number()) %>%
  select(bundleID, domain, id)

str(nodes)

nodes_unique <- nodes %>%
  select(bundleID, domain) %>%
  distinct()

if (nrow(nodes_unique) != nrow(nodes)) {
  print("Duplicate entries exist in nodes.")
}

# create edges
edges <- data_unnested_type_networkActivity_small %>%
  select(bundleID, domain, hits) %>%
  group_by(bundleID, domain) %>%
  summarise(weight = sum(hits, na.rm = TRUE))

str(edges)

# Find `bundleID` values in edges that are not in nodes
unmatched_bundleID <- setdiff(edges$bundleID, nodes$bundleID)
print(unmatched_bundleID)

# Find `domain` values in edges that are not in nodes
unmatched_domain <- setdiff(edges$domain, nodes$domain)
print(unmatched_domain)

edges <- edges %>%
  mutate(across(everything(), as.character))

nodes <- nodes %>%
  mutate(across(everything(), as.character))

# Create a network object
install.packages("igraph")
library(igraph)
network_graph <- graph_from_data_frame(d=edges, directed = FALSE, vertices = nodes)



nodes <- data_unnested_type_networkActivity_small %>%
  select(bundleID, domain, domainOwner) %>%
  pivot_longer(cols = c(bundleID, domain, domainOwner), names_to = "type", values_to = "entity") %>%
  distinct(entity)
# create edges
edges <- data_unnested_type_networkActivity_small %>%
  select(bundleID, domain, hits) %>%
  group_by(bundleID, domain) %>%
  summarise(weight = sum(hits, na.rm = TRUE))

# Create a network object
install.packages("igraph")
library(igraph)
network_graph <- graph_from_data_frame(d=edges, directed = FALSE, vertices = nodes)






