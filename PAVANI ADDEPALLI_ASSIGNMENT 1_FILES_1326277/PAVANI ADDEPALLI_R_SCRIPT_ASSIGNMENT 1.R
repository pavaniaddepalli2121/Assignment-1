

###INSTALLING AND LOADING NEEDED PACKAGES:
  
  install.packages("dplyr")
  install.packages("ggplot2")
  install.packages("bold")
  install.packages("readr")
  

  library(dplyr)   # For data manipulation 
  library(ggplot2) # For data visualization
  library(readr)   # For reading the TSV files
  library(bold)    # To  trieve data from the BOLD database

 ### LOADING DATA FILES AND CHECKING DATA PARSING:

   # This file was downloaded from BOLD

   # Retrieve butterfly data from BOLD for Canada: 
     canada_data <- read_tsv("http://www.boldsystems.org/index.php/API_Public/combined?taxon=Lepidoptera&geo=Canada&format=tsv")

   # Retrieve butterfly data from BOLD for Norway:
     norway_data <- read_tsv("http://www.boldsystems.org/index.php/API_Public/combined?taxon=Lepidoptera&geo=Norway&format=tsv")

   # Check the structure of both datasets:
     str(canada_data)
     str(norway_data)
     
   # To get details about the parsing problems:
     problems(norway_data)

   # To investigate parsing issues in norway_data.
     
     problems(norway_data)      #To see the specific problems

     print(norway_data, n = 50) #to identify where the issues
    
     summary(norway_data)       #to get an overview of the data, including any missing values
    
 ### Filter for Lepidoptera in both datasets:
    
      canada_lepidoptera <- canada_data %>%
      filter(order_name == "Lepidoptera") %>%
      select(processid, sampleid, species_name, province_state, region, lat, lon)
    
      norway_lepidoptera <- norway_data %>%
      filter(order_name == "Lepidoptera") %>%
      select(processid, sampleid, species_name, province_state, region, lat, lon)
    
    # Combine the Data:
    
      combined_data <- bind_rows(
      canada_lepidoptera %>% mutate(country = "Canada"),
      norway_lepidoptera %>% mutate(country = "Norway")
    )
    
  ### To see how many unique species are present in each country:
    
      species_summary <- combined_data %>%
      group_by(country) %>%
      summarise(unique_species_count = n_distinct(species_name))
    
      print(species_summary) 
    
  ### Summary statistics on species distribution by latitude
    
    summary(combined_data$lat)  
    
   
   # To ensure that the bin_uri column is indeed present in the combined_data dataset.
    
      colnames(combined_data)
    
      print(species_by_region)    
    
   #  Print the result
      print(bins_by_region)

    
  ## Count BINs by region
      bins_by_region <- data_combined %>%
      group_by(region) %>%
      summarise(num_bins = n_distinct(bin_uri))
    
  ##  Plot BIN count by region
      ggplot(bins_by_region, aes(x = region, y = num_bins, fill = region)) +
      geom_bar(stat = 'identity') +
      labs(title = "Number of Butterfly BINs in Canada vs Norway", x = "Region", y = "Number of BINs") +
      theme_minimal()
    
  ##  Plot species distribution by latitude
      ggplot(data_combined, aes(x = latitude, fill = region)) +
      geom_histogram(binwidth = 5, alpha = 0.7) +
      labs(title = "Latitudinal Distribution of Butterfly Species", x = "Latitude", y = "Count") +
      theme_minimal()
    
      
      
