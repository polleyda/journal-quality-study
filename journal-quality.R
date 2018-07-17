## Look up journals in SHERPA/RoMEO and DOAJ APIs

# install.packages("dplyr")
# install.packages("httr")
# install.packages("jsonlite")
# install.packages("magrittr")
# install.packages("XML")

# Set working directory
setwd("~/Desktop/journal-quality-study")

# Load data exported from Digital Measures w/ journal titles formatted as API query strings
dm.data <- read.csv("sample-data.csv", strip.white = TRUE, stringsAsFactors = FALSE, na.strings = c("","NA"))

# initialize API URL
url <- "http://www.sherpa.ac.uk/romeo/api29.php"

# Query function
sherpa.search <- function(x){
  sherpa.search <- modify_url(url, query = x) %>% GET()
  parsed.content <- content(sherpa.search, as = "parsed", encoding = "ISO-8859-1") %>% xmlParse() %>% xmlToList()
  journal <- parsed.content[[2]] %>% as.data.frame()
  return(journal)
}

# Create and deduplicate a vector that holds SHERPA query strings
query.vector <- c(dm.data$journal_query) %>% unique()

# Apply sherpa.search() to list of journal titles (formatted as API query strings)
sherpa.results <- lapply(query.vector, sherpa.search)

# Combine list of dataframes into one dataframe
sherpa.results <- do.call("rbind", sherpa.results)

# Rename the columns of sherpa.results dataframe to facilite a join
colnames(sherpa.results) <- c("journal_controlled", "issn", "zetoc_pub", "romeo_pub")

# Combine dm.data and sherpa.results based on journal_controlled
data <- left_join(dm.data,sherpa.results, by = "journal_controlled")

# Copy the ISSN from Digital Measures data into the data$issn column, if none are found in SHERPA
data$issn <- ifelse(is.na(data$issn), data$issn_dm, as.character(data$issn))

# Create new column in dataframe that will hold DOAJ query string
data$doaj_query <- ifelse(is.na(data$issn), data$issn, as.character(paste0("https://doaj.org/api/v1/search/journals/", data$issn)))

# DOAJ Search function
doaj.search <- function(x){
  doaj.search <- GET(x)
  parsed <- content(doaj.search, as = "parsed", encoding = "UTF-8")
  journal <- if(length(parsed$results) != 0){
    cbind.data.frame(parsed$results[[1]]$bibjson$title, 
                     parsed$results[[1]]$bibjson$plagiarism_detection$detection, 
                     parsed$results[[1]]$bibjson$plagiarism_detection$url, 
                     parsed$results[[1]]$bibjson$apc_url, 
                     parsed$results[[1]]$bibjson$link[[1]]$url)
  }
  else{
    return("NA")
  }
  return(journal)
}

# Create a vector that holds DOAJ query strings
query.vector <- c(data$doaj_query)

# Remove NA values from query vector
query.vector <- query.vector[which(data$doaj_query != "NA")]

# Apply DOAJ Search function to query vector
doaj.results <- lapply(query.vector, doaj.search)

#combine list of dataframes into one dataframe
doaj.results <- do.call("rbind", doaj.results)
