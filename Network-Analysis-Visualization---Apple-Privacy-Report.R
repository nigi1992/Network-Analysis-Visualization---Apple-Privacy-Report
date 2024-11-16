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
  print(n=50)

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
library(ggplot2)
# specified domain accessed during network activity
data_unnested %>%
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
  theme_minimal()
  #print(n=20)

# application responsible for network activity
data_unnested %>%
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
  theme_minimal()



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
  
  #filter(is.na(domain) == FALSE) %>%
  #filter(is.na(timeStamp) == FALSE) %>%# filter out NA's
  #filter(is.na(bundleID) == FALSE) %>%
  #filter(is.na(domainOwner) == FALSE) %>%
  #filter(is.na(hits) == FALSE) %>%
  #filter(is.na(firstTimeStamp) == FALSE)

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
geom_node_text(aes(label = name), repel = TRUE, size = 2, max.overlaps = 200)

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
# (ie. manually work on the data set to add domain to a DomainOwner: Facebook, Google, etc.)
# bei domains nachsehen, wie viel unique values
# domains die nur Ip Adressen sind, rausfiltern, anderes unbrauchbares ebenfalls
# Fabian mundt schreiben
# besseres GPT Model benutzen








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






