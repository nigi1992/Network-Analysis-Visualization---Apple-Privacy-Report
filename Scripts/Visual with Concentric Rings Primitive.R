# Visual with 'kk' layout --------------------------------------------------------

df <- read.csv("Input Data/Top24_df.csv")

# Create graph structure
create_multi_layer_graph <- function(data) {
  # Get unique apps and their total hits
  app_hits <- data %>%
    group_by(bundleID) %>%
    summarise(total_hits = sum(hits))
  
  # Create edges from Apps to Domains
  domain_edges <- data %>%
    group_by(bundleID, domain) %>%
    summarise(weight = sum(hits), .groups = 'drop') %>%
    select(from = bundleID, to = domain, weight)
  
  # Create edges from Domains to Domain Owners
  owner_edges <- data %>%
    group_by(domain, DomainOwnerName) %>%
    summarise(weight = sum(hits), .groups = 'drop') %>%
    select(from = domain, to = DomainOwnerName, weight)
  
  # Combine edges
  all_edges <- rbind(domain_edges, owner_edges)
  
  # Create graph
  graph <- graph_from_data_frame(all_edges, directed = TRUE)
  
  return(graph)
}

# Generate the graph
network_graph <- create_multi_layer_graph(df)

# Visualize
plot <- ggraph(network_graph, layout = 'kk') + 
  geom_edge_link(aes(width = weight), alpha = 0.5) +
  geom_node_point(aes(size = degree(network_graph)), color = 'steelblue') +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_void() +
  labs(title = "App Domain Network Analysis")
plot

plot2 <- ggraph(network_graph, layout = 'kk') + 
  geom_edge_link(aes(width = weight), alpha = 0.2) +
  geom_node_point(aes(size = degree(network_graph)), color = 'steelblue') +
  geom_node_text(aes(label = name), repel = TRUE, size = 3, max.overlaps=025) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "App Domain Network Analysis")
plot2

# Save the plot
#ggsave("Output/app_domain_network1.png", plot2, width = 16, height = 12)

# Optional: Print graph statistics
print(paste("Total Nodes:", vcount(network_graph)))
print(paste("Total Edges:", ecount(network_graph)))


# First Visual with concentric Rings --------------------------------------

# Create graph structure 2
create_multi_layer_graph2 <- function(data) {
  # Central node
  user_node <- "User"
  
  # Edges from User to bundleID
  app_edges <- data.frame(
    from = user_node,
    to = unique(data$bundleID)
  )
  
  # Edges from bundleID to domain
  domain_edges <- data %>%
    select(from = bundleID, to = domain)
  
  # Edges from domain to DomainOwnerName
  owner_edges <- data %>%
    select(from = domain, to = DomainOwnerName)
  
  # Combine all edges
  all_edges <- bind_rows(app_edges, domain_edges, owner_edges)
  
  # Create directed graph
  graph <- graph_from_data_frame(all_edges, directed = TRUE)
  
  return(graph)
}

# Generate the graph
network_graph2 <- create_multi_layer_graph2(df)

# Add custom attributes for visualization
V(network_graph2)$layer <- ifelse(V(network_graph2)$name == "User", 1, 
                                 ifelse(V(network_graph2)$name %in% df$bundleID, 2, 
                                        ifelse(V(network_graph2)$name %in% df$domain, 3, 4)))

# Use partition layout for concentric circles
plot3 <- ggraph(network_graph2, layout = 'partition', circular = TRUE) + 
  geom_edge_link(alpha = 0.5) +
  geom_node_point(aes(color = as.factor(layer), size = layer)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_color_manual(values = c("black", "red", "orange", "yellow")) +
  #theme_void() +
  labs(title = "Concentric Network: User, Bundle, Domain, and Domain Owners")
plot3

# Save the plot
ggsave("Output/First Concentric Network.png", plot3, width = 16, height = 12)

# Optional: Print graph statistics
print(paste("Total Nodes:", vcount(network_graph)))
print(paste("Total Edges:", ecount(network_graph)))

# Treehouse Graph ---------------------------------------------------------

# Create concentric graph with correct edge weights
create_concentric_graph3 <- function(data) {
  # Define node types
  user_node <- "User"
  bundle_nodes <- unique(data$bundleID)
  domain_nodes <- unique(data$domain)
  owner_nodes <- unique(data$DomainOwnerName)
  
  # Create edges
  user_edges <- data.frame(from = user_node, to = bundle_nodes, weight = 1)
  domain_edges <- data %>%
    group_by(bundleID, domain) %>%
    summarise(weight = sum(hits), .groups = "drop") %>%
    select(from = bundleID, to = domain, weight)
  owner_edges <- data %>%
    group_by(domain, DomainOwnerName) %>%
    summarise(weight = sum(hits), .groups = "drop") %>%
    select(from = domain, to = DomainOwnerName, weight)
  
  # Combine edges
  all_edges <- bind_rows(user_edges, domain_edges, owner_edges)
  
  # Create directed graph
  graph <- graph_from_data_frame(all_edges, directed = TRUE)
  
  # Add node attributes for coloring
  V(graph)$type <- case_when(
    V(graph)$name == user_node ~ "User",
    V(graph)$name %in% bundle_nodes ~ "bundleID",
    V(graph)$name %in% domain_nodes ~ "domain",
    V(graph)$name %in% owner_nodes ~ "DomainOwnerName",
    TRUE ~ "Other"
  )
  # Assign edge weights
  E(graph)$weight <- all_edges$weight
  print(all_edges)
  return(graph)
}

# Generate the graph
network_graph3 <- create_concentric_graph3(df)
print(E(network_graph3)$weight)

# Visualize concentric graph
plot_treehouse_graph <- function(graph) {
  ggraph(graph, layout = "partition") + 
    geom_edge_link(width = 1, alpha = 0.5) +
    geom_node_point(aes(color = type), size = 5) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    scale_color_manual(values = c(
      "User" = "black",
      "bundleID" = "red",
      "domain" = "orange",
      "DomainOwnerName" = "yellow"
    )) +
    theme_void() +
    labs(title = "Concentric App-Domain Network")
}
plot_treehouse_graph(network_graph3)