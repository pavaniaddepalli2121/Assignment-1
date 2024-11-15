# This script was primarily created by Pavani Addepalli and edited by Yasmine Hezema

## INSTALLING AND LOADING NEEDED PACKAGES:
# Uncomment these lines to install the required packages if they are not already installed.
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("readr")


library(dplyr) # For data manipulation
library(ggplot2) # For data visualization
library(readr) # For reading the TSV files


### LOADING DATA FILES AND CHECKING DATA PARSING:
# 2-RETREIVING DATA FROM BOLD ----
# LOADING DATA FILES AND CHECKING DATA PARSING:

# # BOLD website updated recently; updated URL accordingly.
# The data were retrieved on Nov 12, 2024
# Retrieve butterfly data from BOLD for Canada:
# canada_data <- suppressWarnings(read_tsv("http://bench.boldsystems.org/index.php/API_Public/combined?taxon=Lepidoptera&geo=Canada&format=tsv"))

# Retrieve butterfly data from BOLD for Norway:
# norway_data <- suppressWarnings(read_tsv("http://bench.boldsystems.org/index.php/API_Public/combined?taxon=Lepidoptera&geo=Norway&format=tsv"))

# "suppressWarnings" was used to avoid unnecessary warnings related to incomplete data or connection issues.

# Avoid repeated downloads by saving the datasets locally if they haven't been saved already.

# Download and save the datasets to the local directory only if they haven't already been saved.
# if (!file.exists("canada_data.tsv")) {
# write_tsv(canada_data, "canada_data.tsv")
#  }
#  if (!file.exists("norway_data.tsv")) {
#    write_tsv(norway_data, "norway_data.tsv")
#  }

# Read the datasets from the local directory
canada_data <- read_tsv("canada_data.tsv")
norway_data <- read_tsv("norway_data.tsv")

# 3-Data exploration and manipulation ----

# Removed, the summary is enough
## Summary of datasets to check for missing values and understand the structure.
summary(canada_data)
summary(norway_data)
names(canada_data)
names(norway_data)

# A function is created to clean and filter the datasets
# Create a function to select relevant columns for analysis  and remove NAs

clean_lepidoptera_data <- function(data, country) {
  data %>%
    filter(order_name == "Lepidoptera") %>%
    select(processid, species_name, lat, bin_uri) %>%
    mutate(country = country)
}
# Clean Lepidoptera data function with NA removal and print statement
clean_lepidoptera_data <- function(data, country) {
  total_nas_before <- sum(is.na(data))
  data_cleaned <- data %>%
    filter(order_name == "Lepidoptera") %>%
    select(processid, species_name, lat, bin_uri) %>%
    mutate(country = country) %>%
    na.omit()
  total_nas_removed <- total_nas_before - sum(is.na(data_cleaned))
  cat("Total number of NAs removed:", total_nas_removed, "\n")
  return(data_cleaned)
}
# Apply cleaning function to both datasets
canada_lepidoptera <- clean_lepidoptera_data(canada_data, "Canada")
# Total number of NAs removed: 5528717
norway_lepidoptera <- clean_lepidoptera_data(norway_data, "Norway")
# Total number of NAs removed: 375104

# Sampleid, province_state, region and lon have been removed as they have not been used in the analysis, as the main objective of the study was to explore the geographical distribution of butterfly species in Canada and Norway and assess the distribution patterns of butterfly species along latitudinal gradients. bin_uri has been added to dataframe to explore the BIN distribution by country (Figure 1 in the report).

# See names of the selected variable in each country
names(canada_lepidoptera)
names(norway_lepidoptera)

# Combine the Data:
combined_data <- bind_rows(canada_lepidoptera, norway_lepidoptera)
# The code has been improved for brevity.

# check data summary 
summary(combined_data)

# Verify there are no NA values left
anyNA(combined_data)
# It returned "FALSE", which means all NA values have been successfully removed.

## Figure 1: Boxplot to show the distribution of latitude by country----
ggplot(combined_data, aes(x = country, y = lat, fill = country)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Distribution of Latitude by Country",
    x = "Country",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    text = element_text(size = 14), # Increase font size for all text
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_blank(), # Remove x-axis labels (Canada and Norway)
    axis.text.y = element_text(size = 12)
  ) +
  scale_fill_manual(values = c("Canada" = "lightblue", "Norway" = "lightcoral"))

# However, Canada has several outliers at high latitudes, possibly beyond the typical butterfly habitats, these outliers might represent arctic butterflies that might be valuable data points that should be retained to understand species adaptation and geographic ranges.

# 4-DATA ANALYSIS AND VISUALIZATION -----
## Figure 2: BIN distribution by country ----
# Grouping by region was unnecessary since countries are already defined. Grouped bins by country instead.

# Count the number of BINs by country
bins_by_country <- combined_data %>%
  group_by(country) %>%
  summarise(num_bins = n_distinct(bin_uri), .groups = "drop")

# Print the result
print(bins_by_country)

# Plot BIN count by country
ggplot(bins_by_country, aes(x = country, y = num_bins, fill = country)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(
    title = "Number of BINs in Canada vs Norway",
    x = "Country ",
    y = "Number of BINs",
    fill = "Country"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16), # Make title bold and adjust size
    text = element_text(size = 14), # Increase font size for all text
    axis.title.x = element_text(face = "bold"), # Bold x-axis label
    axis.title.y = element_text(face = "bold"), # Bold y-axis label
    axis.text.x = element_blank(), # Remove x-axis labels (Canada and Norway)
    axis.text.y = element_text(size = 12) # Increase font size for y-axis labels
  ) +
  scale_fill_manual(values = c("Canada" = "lightblue", "Norway" = "lightcoral"))
# The figure has been improved by modifying formats.

## Figure 3: Species distribution by latitude ----
# The original plot is a histogram showing the frequency of latitude ranges. To explore species richness across latitude, the number of unique species was used.
# Summarize the number of unique species by latitude and country
species_by_lat <- combined_data %>%
  mutate(lat_rounded = round(lat, 0)) %>% # Round latitude to the nearest integer
  group_by(lat_rounded, country) %>%
  summarise(num_species = n_distinct(species_name), .groups = "drop")
# Plot species richness by latitude
ggplot(species_by_lat, aes(x = lat_rounded, y = num_species, fill = country)) +
  geom_col(position = "stack", width = 0.5) +
  labs(
    title = "Butterfly Species Distribution across Latitudinal in Canada and Norway", ("bold"),
    x = "Latitude",
    y = "Number of Unique Species",
    fill = "Country"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16), # Make title bold and adjust size
    text = element_text(size = 14),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) +
  scale_fill_manual(values = c("Canada" = "lightblue", "Norway" = "lightcoral"))
# The figure have been improved by modifying some font formats and using the appropriate data.

# species_by_region was not identified in the original script

# bin_uri was not included in the original script

# BIN counts by country was calculated and moved above in the appropriated analysis flow

# Plots were improved and moved to the appropriate sequence
