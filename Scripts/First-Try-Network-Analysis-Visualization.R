# New Datasets for Visualization ------------------------------------------

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

# create new data frame with new column "DomainOwnerName" that renames all the domains that contain strings like "google", "apple", "facebook", "amazon", to that string
library(tidyverse) # For data manipulation
data_unnested_type_networkActivity_small <- data_unnested_type_networkActivity_small %>%
  mutate(DomainOwnerName = case_when(
    str_detect(domain, "google") ~ "Google",
    str_detect(domain, "youtube") ~ "Google", 
    str_detect(domain, "gstatic") ~ "Google",
    str_detect(domain, "apple") ~ "Apple",
    str_detect(domain, "mzstatic") ~ "Apple",
    str_detect(domain, "icloud") ~ "Apple",
    str_detect(domain, "facebook") ~ "Facebook",
    str_detect(domain, "amazon") ~ "Amazon",
    str_detect(domain, "microsoft") ~ "Microsoft",
    str_detect(domain, "linkedin") ~ "Microsoft",
    str_detect(domain, "bing") ~ "Microsoft",
    str_detect(domain, "adobe") ~ "Adobe",
    str_detect(domainOwner, "Microsoft Corporation") ~ "Microsoft",
    str_detect(domain, "appsflyer") ~ "AppsFlyer",
    str_detect(domain, "unity") ~ "Unity Software",
    str_detect(domain, "ingest.sentry.io") ~ "Functional Software",
    str_detect(domainOwner, "Adobe Inc.", ) ~ "Adobe",
    str_detect(domainOwner, "AddApptr GmbH") ~ "AddApptr",
    str_detect(domainOwner, "Google LLC") ~ "Google",
    
    TRUE ~ "Other"
  ))

getwd()
# Save as a tab-delimited text file
write.table(data_unnested_type_networkActivity_small, "Input Data/NetworkAct_df.txt", sep = "\t", row.names = FALSE)
write.table(data_unnested_type_networkActivity_small, "Input Data/NetworkAct_df.csv", sep = ",", row.names = FALSE)


# First Attempt at Network Visual -----------------------------------------

library(igraph)
install.packages("ggraph")# For network graph creation
library(ggraph)    # For network graph visualization

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

# Plot Corrections --------------------------------------------------------

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


# Second attempt with only domains with more than 2 occurences ------------

# Count occurrences of each domain and filter
data_unnested_type_networkActivity_filtered <- data_unnested_type_networkActivity_small %>%
  group_by(domain) %>%
  filter(n() >= 2) %>%
  ungroup()

# create new data frame with new column "DomainOwnerName" that renames all the domains that contain strings like "google", "apple", "facebook", "amazon", to that string
data_unnested_type_networkActivity_filtered <- data_unnested_type_networkActivity_filtered %>%
  mutate(DomainOwnerName = case_when(
    str_detect(domain, "google") ~ "Google",
    str_detect(domain, "youtube") ~ "Google", 
    str_detect(domain, "gstatic") ~ "Google",
    str_detect(domain, "apple") ~ "Apple",
    str_detect(domain, "mzstatic") ~ "Apple",
    str_detect(domain, "icloud") ~ "Apple",
    str_detect(domain, "facebook") ~ "Facebook",
    str_detect(domain, "amazon") ~ "Amazon",
    str_detect(domain, "microsoft") ~ "Microsoft",
    str_detect(domain, "linkedin") ~ "Microsoft",
    str_detect(domain, "bing") ~ "Microsoft",
    str_detect(domain, "adobe") ~ "Adobe",
    str_detect(domainOwner, "Microsoft Corporation") ~ "Microsoft",
    str_detect(domain, "appsflyer") ~ "AppsFlyer",
    str_detect(domain, "unity") ~ "Unity Software",
    str_detect(domain, "ingest.sentry.io") ~ "Functional Software",
    str_detect(domainOwner, "Adobe Inc.") ~ "Adobe",
    str_detect(domainOwner, "AddApptr GmbH") ~ "AddApptr",
    str_detect(domainOwner, "Google LLC") ~ "Google",
    
    TRUE ~ "Other"
  ))

# Show Top DomainOwnerName
data_unnested_type_networkActivity_filtered %>%
  count(DomainOwnerName, sort = TRUE)

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

# second try at network visualization
ggraph(g2, layout = "fr") + 
  geom_edge_link(aes(width = weight), alpha = 0.2) + 
  geom_node_point(size = 1) + 
  geom_node_text(aes(label = name), repel = TRUE, size = 3, max.overlaps = 200) + 
  theme_void() + 
  theme(legend.position = "bottom") +
  labs(title = "Network Activity Visualization (Applications and Domains)")


# Plot Corrections for second attempt -------------------------------------

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

# Third attempt with only Top24 Apps --------------------------------------
data_unnested_type_networkActivity_top24apps <- data_unnested_type_networkActivity_small %>%
  group_by(bundleID) %>%
  filter(n() >= 20) %>%
  ungroup()

# Count occurrences of each domain and filter
Top24apps_no_Unique_Domains <- data_unnested_type_networkActivity_top24apps %>%
  group_by(domain) %>%
  filter(n() >= 2) %>%
  ungroup()

# renaming the bundleID values to something more readable
Top24apps_no_Unique_Domains <- Top24apps_no_Unique_Domains %>%
  mutate(bundleID = case_when(
    bundleID == "com.9gag.ios.mobile" ~ "9GAG",
    bundleID == "ch.migros.m-go" ~ "Migros",
    bundleID == "ch.tutti.iphone" ~ "Tutti",
    bundleID == "co.bird.Ride" ~ "Bird",
    bundleID == "com.adobe.Adobe-Reader" ~ "Acrobat",
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

# create new data frame with new column "DomainOwnerName" that renames all the domains that contain strings like "google", "apple", "facebook", "amazon", to that string
Top24apps_no_Unique_Domains <- Top24apps_no_Unique_Domains %>%
  mutate(DomainOwnerName = case_when(
    str_detect(domain, "google") ~ "Google",
    str_detect(domain, "youtube") ~ "Google", 
    str_detect(domain, "gstatic") ~ "Google",
    str_detect(domain, "apple") ~ "Apple",
    str_detect(domain, "mzstatic") ~ "Apple",
    str_detect(domain, "icloud") ~ "Apple",
    str_detect(domain, "facebook") ~ "Facebook",
    str_detect(domain, "amazon") ~ "Amazon",
    str_detect(domain, "microsoft") ~ "Microsoft",
    str_detect(domain, "linkedin") ~ "Microsoft",
    str_detect(domain, "bing") ~ "Microsoft",
    str_detect(domain, "adobe") ~ "Adobe",
    str_detect(domain, "appsflyer") ~ "AppsFlyer",
    str_detect(domain, "unity") ~ "Unity Inc.",
    str_detect(domain, "ingest.sentry.io") ~ "Functional Inc.",
    str_detect(domainOwner, "Adobe Inc.") ~ "Adobe",
    str_detect(domainOwner, "AddApptr GmbH") ~ "AddApptr",
    str_detect(domainOwner, "Google LLC") ~ "Google",
    str_detect(domainOwner, "Microsoft Corporation") ~ "Microsoft",
    str_detect(domainOwner, "Facebook, Inc.") ~ "Facebook",
    TRUE ~ "Other"
  ))

# Select relevant columns
Top24apps_no_Unique_Domains <- Top24apps_no_Unique_Domains %>%
  select(bundleID, domain, hits, DomainOwnerName)
         
write.table(Top24apps_no_Unique_Domains, "Input Data/Top24_df.csv", sep = ",", row.names = FALSE)

# Show Top DomainOwnerNames
Top24apps_no_Unique_Domains %>%
  group_by(DomainOwnerName) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# Show Domains with more than 2 occurrences
Top24apps_no_Unique_Domains %>%
  group_by(domain) %>%
  filter(n() >= 2) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  print(n = 1000)

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


# Plots for third attempt -------------------------------------------------

# third try at network visualization
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
NAVfirst <- ggraph(g3, layout = "fr") + 
  geom_edge_link(aes(width = weight), alpha = 0.2) + 
  geom_node_point(aes(color = node_color), size = 1) +
  scale_color_identity() +  # Use the actual colors specified
  geom_node_text(aes(label = name), repel = TRUE, size = 3, max.overlaps = 200) + 
  #theme_void() + 
  theme(legend.position = "none") +
  labs(title = "Network Activity Visualization (Applications and Domains)")
NAVfirst
ggsave("Output/First Network Analysis Visualization.png", plot=NAVfirst, width = 16, height = 12)

# Fourth Attempt - Network with concentric Rings --------------------------

########### Network graph with center ("User"), middle ring (apps, "bundleID") and outer ring (domains)
# and maybe even outer outer ring ("domainOwner")

# Convert the graph to a directed one
g3_directed <- as_directed(g3, mode = "arbitrary")

# Create hierarchical levels for nodes
V(g3_directed)$level <- ifelse(
  V(g3_directed)$name == "User", 1,  # Central node
  ifelse(V(g3_directed)$name %in% unique(edges_user_app3$to), 2, 3)  # Middle and outer rings
)

# Sort edges to ensure direction flows from the center outward
# "User" -> bundleID -> domain
all_edges3 <- all_edges3 %>%
  arrange(match(from, c("User", unique(edges_user_app3$to))))

# Recreate graph to ensure hierarchical structure
g3_directed <- graph_from_data_frame(all_edges3, directed = TRUE)

# Create a radial tree layout
layout_radial <- create_layout(g3_directed, layout = "dendrogram", circular = TRUE)

# Create the visualization
ggraph(g3, layout = manual_coords %>% select(x, y)) + 
  geom_edge_link(aes(width = weight), alpha = 0.2) + 
  geom_node_point(aes(color = name == "User"), size = 3) +
  scale_color_manual(values = c("orange", "red")) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3, max.overlaps = 200) + 
  theme_void() + 
  theme(legend.position = "none") +
  labs(title = "Hierarchical Network Activity Visualization") +
  coord_fixed()

# Create a custom layout function for hierarchical circular layout
create_radial_layout <- function(graph) {
  # Get vertices
  vertices <- V(graph)$name
  
  # Identify types of nodes
  is_user <- vertices == "User"
  is_app <- vertices %in% unique(edges_user_app3$to)
  is_domain <- !is_user & !is_app
  
  # Count of each type
  n_apps <- sum(is_app)
  n_domains <- sum(is_domain)
  
  # Create coordinates
  coords <- matrix(0, nrow = length(vertices), ncol = 2)
  
  # User at center
  coords[is_user, ] <- c(0, 0)
  
  # Apps in middle circle
  app_angles <- seq(0, 2*pi, length.out = n_apps + 1)[-(n_apps + 1)]
  coords[is_app, ] <- cbind(cos(app_angles), sin(app_angles))
  
  # Domains in outer circle
  domain_angles <- seq(0, 2*pi, length.out = n_domains + 1)[-(n_domains + 1)]
  coords[is_domain, ] <- cbind(2 * cos(domain_angles), 2 * sin(domain_angles))
  
  return(coords)
}

# Alternative version with different colors for each level
V(g3)$node_type <- case_when(
  V(g3)$name == "User" ~ "user",
  V(g3)$name %in% unique(edges_user_app3$to) ~ "app",
  TRUE ~ "domain"
)

######Fail!!!!

# Code Rest ---------------------------------------------------------------
############################################# Code rest #########################################

### create matrix from website
matrix_networkActivity <- crossprod(table(data_unnested_type_networkActivity_small[2:3]))
diag(matrix_networkActivity) <- 0
matrix_df <- as.data.frame(matrix_networkActivity)
## useless?


# Create the test_nodes data frame with the name column as vertex names
test_nodes <- data.frame(name = c("A", "B", "C"), id = 1:3)
# Create the test_edges data frame
test_edges <- data.frame(from = c("A", "B"), to = c("B", "C"))
# Create the graph using the corrected test_nodes and test_edges data frames
graph_test <- graph_from_data_frame(d = test_edges, vertices = test_nodes, directed = FALSE)
# Plot the graph
plot(graph_test)

