# Show Top DomainOwnerNames
Top24apps_no_Unique_Domains %>%
  group_by(DomainOwnerName) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  print(n = 1000) %>%
  write.csv("Output/Most_common_Domain_Owners.csv")

# Show Domains with more than 2 occurrences
Top24apps_no_Unique_Domains %>%
  group_by(domain) %>%
  filter(n() >= 2) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  print(n = 1000) %>%
  write.csv("Output/Most_accessed_Domains.csv")

# Show Domains used by bundleID "DER SPIEGEL" ranked by hits
Top24apps_no_Unique_Domains %>%
  filter(bundleID == "DER SPIEGEL") %>%
  group_by(domain) %>%
  summarise(hits = sum(hits)) %>%
  arrange(desc(hits)) %>%
  print(n = 1000)
  write.csv("Output/DER_SPIEGEL_Domains.csv")
  
# Show all Domains used by bundleID "DER SPIEGEL" ranked by hits
data_unnested_type_networkActivity %>%
  filter(bundleID == "de.spiegel.spon") %>%
  group_by(domain) %>%
  summarise(hits = sum(hits)) %>%
  arrange(desc(hits)) %>%
  print(n = 1000) %>%
  write.csv("Output/DER_SPIEGEL_Domains_complete.csv")
