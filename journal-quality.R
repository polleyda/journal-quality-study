## Look up journals in SHERPA/RoMEO and DOAJ APIs

# install.packages("dplyr")
# install.packages("httr")
# install.packages("jsonlite")
# install.packages("magrittr")
# install.packages("XML")

# Set working directory
setwd("~/Desktop/journal-quality-study")

# Load data w/ journal titles formatted as API query strings
citation_data <- read.csv("sample-data.csv", strip.white = TRUE, stringsAsFactors = FALSE, na.strings = c("","NA"))

# initialize API URL
url <- "http://www.sherpa.ac.uk/romeo/api29.php"

# Query function
sherpa_search <- function(x){
  sherpa_search <- modify_url(url, query = x) %>% GET()
  parsed_content <- content(sherpa_search, as = "parsed", encoding = "ISO-8859-1") %>% xmlParse() %>% xmlToList()
  journal <- parsed_content[[2]] %>% as.data.frame()
  return(journal)
}

# Create and deduplicate a vector that holds SHERPA query strings
sherpa_query_vector <- c(citation_data$journal_query) %>% unique()

# Apply sherpa.search() to list of journal titles (formatted as API query strings)
sherpa_results <- lapply(sherpa_query_vector, sherpa_search)

# Combine list of dataframes into one dataframe
sherpa_results <- do.call("rbind", sherpa_results)

# Rename the columns of sherpa.results dataframe to facilite a join
colnames(sherpa_results) <- c("journal_controlled", "issn", "zetoc_pub", "romeo_pub")

# Combine dm.data and sherpa.results based on journal_controlled
data <- left_join(citation_data,sherpa_results, by = "journal_controlled")

# Copy the ISSN from dm.data into the data$issn column, if none are found in SHERPA
data$issn <- ifelse(is.na(data$issn), data$issn_dm, as.character(data$issn))

# Create new column in dataframe that will hold DOAJ query string
data$doaj_query <- ifelse (is.na(data$issn), data$issn, as.character(paste0("https://doaj.org/api/v1/search/journals/", data$issn)))

# DOAJ Search function
doaj_search <- function(x){
  doaj_search <- GET(x)
  parsed <- content(doaj_search, as = "parsed", encoding = "UTF-8")
  journal <- if(length(parsed$results) != 0){
    cbind.data.frame(parsed$results[[1]]$bibjson$title, 
                     parsed$results[[1]]$bibjson$plagiarism_detection$detection, 
                     parsed$results[[1]]$bibjson$plagiarism_detection$url, 
                     parsed$results[[1]]$bibjson$apc_url, 
                     parsed$results[[1]]$bibjson$link[[1]]$url)
  }else{
    return("NA")
  }
  return(journal)
}

# Create a vector that holds DOAJ query strings
doaj_query_vector <- c(data$doaj_query)

# Remove NA values from query vector
doaj_query_vector <- doaj_query_vector[which(data$doaj_query != "NA")]

# Apply DOAJ Search function to query vector
doaj_results <- lapply(doaj_query_vector, doaj_search)

# Remove NAs from list
doaj_results <- doaj_results[sapply(doaj_results, function(x) dim(x)[1]) > 0]

#combine list of dataframes into one dataframe
doaj_results <- do.call("rbind", doaj_results)
