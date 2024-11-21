# 1. Refining Concentric Network Visual --------------------------------------

# Loading necessary libraries
library(igraph)

# Filtering out rows where 'DomainOwnerName' is 'Other'
filtered_df <- df #subset(df, DomainOwnerName != "Other")

# Creating an empty graph
g4 <- make_empty_graph(directed = FALSE)

# Adding the 'User' node
g4 <- add_vertices(g4, 1, name = "User", color = "black", layer = 0)

# Adding 'bundleID' nodes and connecting to 'User' with weight 1
unique_bundle_ids <- unique(filtered_df$bundleID)
for (bundle in unique_bundle_ids) {
  if (!(bundle %in% V(g4)$name)) {
    g4 <- add_vertices(g4, 1, name = bundle, color = "red", layer = 1)
  }
  g4 <- add_edges(g4, c("User", bundle), weight = 1)
}

# Adding 'domain' nodes and connecting to 'bundleID' nodes using 'hits' as weights
for (i in 1:nrow(filtered_df)) {
  domain <- filtered_df$domain[i]
  bundle <- filtered_df$bundleID[i]
  hits <- filtered_df$hits[i]
  if (!(domain %in% V(g4)$name)) {
    g4 <- add_vertices(g4, 1, name = domain, color = "orange", layer = 2)
  }
  g4 <- add_edges(g4, c(bundle, domain), weight = hits)
}

# Adding 'DomainOwnerName' nodes and connecting to 'domain' nodes with weight 1
for (i in 1:nrow(filtered_df)) {
  domain <- filtered_df$domain[i]
  domain_owner <- filtered_df$DomainOwnerName[i]
  if (!(domain_owner %in% V(g4)$name)) {
    g4 <- add_vertices(g4, 1, name = domain_owner, color = "yellow", layer = 3)
  }
  g4 <- add_edges(g4, c(domain, domain_owner), weight = 1)
}

# Extracting node attributes
V(g4)$color <- sapply(V(g4)$name, function(name) {
  if (name == "User") {
    return("white")
  } else if (name %in% unique_bundle_ids) {
    return("red")
  } else if (name %in% filtered_df$domain) {
    return("orange")
  } else {
    return("yellow")
  }
})

layout_concentric2 <- function(g4) {
  layers <- split(V(g4), V(g4)$layer)
  positions <- list()
  positions[["User"]] <- c(0, 0)  # Place 'User' at the center
  for (i in seq_along(layers)) {
    if (i == 1) next  # Skip layer 0 since 'User' is already placed
    layer_size <- length(layers[[i]])
    radius <- i * 1.5
    angles <- seq(0, 2 * pi, length.out = layer_size + 1)[-1]
    for (j in seq_along(layers[[i]])) {
      positions[[layers[[i]][j]$name]] <- c(radius * cos(angles[j]), radius * sin(angles[j]))
    }
  }
  return(do.call(rbind, positions))
}

layout2 <- layout_concentric2(g4)

#png("Output/concentric_network-primitve.png", width = 800, height = 800)
plot(g4, 
     vertex.color = V(g4)$color, 
     vertex.label = V(g4)$name, 
     vertex.label.color = "black", 
     vertex.size = 20,
     edge.width = E(g4)$weight, 
     layout = layout2, 
     main = "Concentric Network Visualization")
#dev.off()

# 2. Normalized edges --------------------------------------

# Adjust vertex sizes and labels
vertex.size.values <- ifelse(V(g4)$layer == 0, 25, ifelse(V(g4)$layer == 1, 15, ifelse(V(g4)$layer == 2, 10, ifelse(V(g4)$layer == 3, 15, 8))))
vertex.label.values <- ifelse(V(g4)$layer == 0 | V(g4)$layer == 1 | V(g4)$layer == 3, V(g4)$name, NA)

# Plotting the graph using concentric circles with 'User' at the center
layout_concentric2 <- function(g4) {
  layers <- split(V(g4), V(g4)$layer)
  positions <- list()
  positions[["User"]] <- c(0, 0)  # Place 'User' at the center
  for (i in seq_along(layers)) {
    if (i == 1) next  # Skip layer 0 since 'User' is already placed
    layer_size <- length(layers[[i]])
    radius <- i * 2.5  # Increase spacing between layers
    angles <- seq(0, 2 * pi, length.out = layer_size + 1)[-1]
    for (j in seq_along(layers[[i]])) {
      positions[[layers[[i]][j]$name]] <- c(radius * cos(angles[j]), radius * sin(angles[j]))
    }
  }
  return(do.call(rbind, positions))
}

layout2 <- layout_concentric2(g4)

png("Output/concentric_network_adjusted1.png", width = 1000, height = 1000)
plot(g4, 
     vertex.color = V(g4)$color, 
     vertex.label = vertex.label.values, 
     vertex.label.color = "black", 
     vertex.size = vertex.size.values, 
     edge.width = E(g4)$weight / max(E(g4)$weight) * 5,  # Normalize edge width
     layout = layout2, 
     main = "Concentric Network Visualization with Normalized Edges")
dev.off()

# 3. Edges are further adjusted + Plot Visual Refinement ---------------------

# Extracting node attributes
V(g4)$color <- sapply(V(g4)$name, function(name) {
  if (name == "User") {
    return("lightskyblue1")
  } else if (name %in% unique_bundle_ids) {
    return("tomato1")
  } else if (name %in% filtered_df$domain) {
    return("orange")
  } else {
    return("gold")
  }
})

# Adjust vertex sizes and labels
vertex.size.values <- ifelse(V(g4)$layer == 0, 20, ifelse(V(g4)$layer == 1, 12, ifelse(V(g4)$layer == 2, 1, ifelse(V(g4)$layer == 3, 25, 8))))
vertex.label.values <- ifelse(V(g4)$layer == 0 | V(g4)$layer == 1 | V(g4)$layer == 3, V(g4)$name, NA)
#vertex.label.size.values <- ifelse(V(g4)$layer == 0, 3, ifelse(V(g4)$layer == 1, 1, ifelse(V(g4)$layer == 2, 1, ifelse(V(g4)$layer == 3, 3, 1))))

# Plotting the graph using concentric circles with 'User' at the center
layout_concentric2 <- function(g4) {
  layers <- split(V(g4), V(g4)$layer)
  positions <- list()
  positions[["User"]] <- c(0, 0)  # Place 'User' at the center
  for (i in seq_along(layers)) {
    if (i == 1) next  # Skip layer 0 since 'User' is already placed
    layer_size <- length(layers[[i]])
    radius <- i * 2.5  # Increase spacing between layers
    angles <- seq(0, 2 * pi, length.out = layer_size + 1)[-1]
    for (j in seq_along(layers[[i]])) {
      positions[[layers[[i]][j]$name]] <- c(radius * cos(angles[j]), radius * sin(angles[j]))
    }
  }
  return(do.call(rbind, positions))
}

layout2 <- layout_concentric2(g4)

# Save the plot to a file
png("Output/concentric_network_adjusted2.png", width = 1000, height = 1000)
plot(g4, 
     vertex.color = V(g4)$color, 
     vertex.label = vertex.label.values, 
     vertex.label.color = "black",
     #vertex.label.size = vertex.label.size.values,
     vertex.size = vertex.size.values, 
     edge.width = E(g4)$weight / max(E(g4)$weight) * 20,  # Normalize edge width
     layout = layout2, 
     main = "Adjusted Concentric Network Visualization")
dev.off()

# 4. Plot displaying domain with more than 3 ---------------------------------

vertex.size.values <- ifelse(V(g4)$layer == 0, 20, ifelse(V(g4)$layer == 1, 12, ifelse(V(g4)$layer == 2, 1, ifelse(V(g4)$layer == 3, 25, 8))))
vertex.label.values <- sapply(V(g4)$name, function(name) {
  if (V(g4)$layer[V(g4)$name == name] == 2) {
    domain_count <- sum(filtered_df$domain == name)
    if (domain_count >= 3) {
      return(name)
    }
    return(NA)
  }
  return(name)
})

layout_concentric2 <- function(g4) {
  layers <- split(V(g4), V(g4)$layer)
  positions <- list()
  positions[["User"]] <- c(0, 0)  # Place 'User' at the center
  for (i in seq_along(layers)) {
    if (i == 1) next  # Skip layer 0 since 'User' is already placed
    layer_size <- length(layers[[i]])
    radius <- i * 1.5
    angles <- seq(0, 2 * pi, length.out = layer_size + 1)[-1]
    for (j in seq_along(layers[[i]])) {
      positions[[layers[[i]][j]$name]] <- c(radius * cos(angles[j]), radius * sin(angles[j]))
    }
  }
  return(do.call(rbind, positions))
}

layout2 <- layout_concentric2(g4)

png("Output/concentric_network_adjusted3.png", width = 1000, height = 1000)
plot(g4, 
     vertex.color = V(g4)$color, 
     vertex.label = vertex.label.values, 
     vertex.label.color = "black",
     #vertex.label.size = vertex.label.size.values,
     vertex.size = vertex.size.values, 
     edge.width = E(g4)$weight / max(E(g4)$weight) * 20,  # Normalize edge width
     layout = layout2, 
     main = "Concentric Network Visualization with Domains with 3 or more occurrences")
dev.off()

# 5. Plot displaying domains with high weights -------------------------------

# Extracting node attributes
V(g4)$color <- sapply(V(g4)$name, function(name) {
  if (name == "User") {
    return("lightblue1")
  } else if (name %in% unique_bundle_ids) {
    return("tomato1")
  } else if (name %in% filtered_df$domain) {
    return("snow2")
  } else {
    return("gold")
  }
})

vertex.size.values <- ifelse(V(g4)$layer == 0, 20, ifelse(V(g4)$layer == 1, 13.5, ifelse(V(g4)$layer == 2, 1, ifelse(V(g4)$layer == 3, 25, 8))))
vertex.label.values <- sapply(V(g4)$name, function(name) {
  if (V(g4)$layer[V(g4)$name == name] == 2) {
    domain_count <- sum(filtered_df$domain == name)
    if (domain_count >= 1) {
      return(name)
    }
    return(NA)
  }
  return(name)
})

weight_threshold <- 20  # Set the threshold for minimum edge weight
vertex.label.values <- sapply(V(g4)$name, function(name) {
  if (name %in% filtered_df$domain) {
    total_weight <- sum(E(g4)[.from(name)]$weight)
    if (total_weight < weight_threshold) {
      return(NA)
    }
  }
  return(name)
})

layout_concentric2 <- function(g4) {
  layers <- split(V(g4), V(g4)$layer)
  positions <- list()
  positions[["User"]] <- c(0, 0)  # Place 'User' at the center
  for (i in seq_along(layers)) {
    if (i == 1) next  # Skip layer 0 since 'User' is already placed
    layer_size <- length(layers[[i]])
    radius <- i * 1.5
    angles <- seq(0, 2 * pi, length.out = layer_size + 1)[-1]
    for (j in seq_along(layers[[i]])) {
      positions[[layers[[i]][j]$name]] <- c(radius * cos(angles[j]), radius * sin(angles[j]))
    }
  }
  return(do.call(rbind, positions))
}

layout2 <- layout_concentric2(g4)

png("Output/concentric_network_adjusted4.png", width = 1500, height = 1500)
plot(g4, 
     vertex.color = V(g4)$color, 
     vertex.label = vertex.label.values, 
     vertex.label.color = "black",
     #vertex.label.size = vertex.label.size.values,
     vertex.size = vertex.size.values, 
     edge.width = E(g4)$weight / max(E(g4)$weight) * 20,  # Normalize edge width
     layout = layout2, 
     main = "Concentric Network Visualization with Domains with High Weights")
dev.off()
