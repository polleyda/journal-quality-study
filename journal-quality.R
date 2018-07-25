## Journal Quality

# install.packages("dplyr")
# install.packages("httr")
# install.packages("jsonlite")
# install.packages("magrittr")
# install.packages("XML")

# Set working directory
setwd("~/Desktop/journal-quality-study")

# Time script
ptm <- proc.time()

# Load data w/ journal titles formatted as API query strings
citation_data <- read.csv("Journals-Only-Quality-2017-analysis-REDACTED-20180430.csv", strip.white = TRUE, stringsAsFactors = FALSE, na.strings = c("","NA"))

# Takes care of null values in lists returned by SHERPA/RoMEO API
# https://stackoverflow.com/questions/22870198/is-there-a-more-efficient-way-to-replace-null-with-na-in-a-list
nullToNA <- function(x){
    x[sapply(x, is.null)] <- NA
    return(x)
  }

# Search SHERPA/RoMEO API 
sherpa_search <- function(x){
  sherpa_search <- GET(x)
  parsed_content <- content(sherpa_search, as = "parsed", encoding = "ISO-8859-1") %>% xmlParse() %>% xmlToList()
  if(length(parsed_content[[2]]) > 1){
    journal <- c(x,"Multiple Matches")
  }else{
  journal <- parsed_content[[2]]
  journal <- lapply(journal, nullToNA) %>% as.data.frame()
  }
  return(journal)
}

# Create and de-duplicate a vector that holds SHERPA query strings
sherpa_query_vector <- c(citation_data$sherpa_query) %>% unique()

# Apply sherpa_search() to list of journal titles (formatted as API query strings)
sherpa_results <- lapply(sherpa_query_vector, sherpa_search)

# Build the multiple matches in SHERPA into dataframe
multiple_matches <- sherpa_results[grep("GFuZz2ehaks", sherpa_results)]
multiple_matches <- do.call("rbind", multiple_matches) %>% as.data.frame()
colnames(multiple_matches) <- c("sherpa_query", "multiple_matches")

# Combine list of dataframes into one dataframe
sherpa_results <- sherpa_results[grep("journal.jtitle", sherpa_results)]
sherpa_results <- do.call("rbind", sherpa_results)

# Rename the columns of sherpa.results dataframe to facilite a join
colnames(sherpa_results) <- c("journal_controlled", "issn", "zetoc_pub", "romeo_pub")

# Combine citation_data and sherpa_results based on journal_controlled
data <- left_join(citation_data, sherpa_results, by = "journal_controlled")
data <- left_join(data, multiple_matches, by = "sherpa_query")

# Copy the ISSN from citation_data into the data$issn column, if none are found in SHERPA
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

# Apply doaj_search() function to query vector
doaj_results <- lapply(doaj_query_vector, doaj_search)

# Remove NAs from list
doaj_results <- doaj_results[which(doaj_results != "NA")]

# Combine list of dataframes into one dataframe
doaj_results <- do.call("rbind", doaj_results)

# Rename columns
colnames(doaj_results) <- c("journal_controlled", "plagiarism_detection_policy", "plagiarism_detection_policy_url", "apc_url", "journal_url")

# Combine results of DOAJ search with data by journal title
data <- left_join(data,doaj_results, by = "journal_controlled")

# Deduplicate data based on id
data <- data[!duplicated(data$id),]

# Write CSV file of results
write.csv(data, "journal-quality-results.csv", row.names = FALSE)
