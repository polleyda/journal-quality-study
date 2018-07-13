## Look up journals in SHERPA/RoMEO API
# install.packages("dplyr")
# install.packages("httr")
# install.packages("magrittr")
# install.packages("XML")

# Set working directory
setwd("~/Desktop/journal-quality")

# Load data exported from Digital Measures w/ journal titles formatted as API query strings
dm.data <- read.csv("sample-data.csv", strip.white = TRUE, stringsAsFactors = FALSE, na.strings = c("","NA"))

# Save journal titles formatted as query strings as a vector, remove duplicates
query.vector <- c(dm.data$journal_query) %>% unique()

# initialize API URL
url <- "http://www.sherpa.ac.uk/romeo/api29.php"

# Query function
sherpa.search <- function(x){
  sherpa.search <- modify_url(url, query = x) %>% GET()
  parsed.content <- content(sherpa.search, as = "parsed", encoding = "ISO-8859-1") %>% xmlParse() %>% xmlToList()
  journal <- parsed.content[[2]] %>% as.data.frame()
  return(journal)
}

# Apply sherpa.search() to list of journal titles (formatted as API query strings)
sherpa.results <- lapply(query.vector, sherpa.search)

# Combine list of dataframes into one dataframe
sherpa.results <- do.call("rbind", sherpa.results)

# Rename the columns of sherpa.results dataframe
colnames(sherpa.results) <- c("journal_controlled", "issn", "zetoc_pub", "romeo_pub")

# Combine dm.data and sherpa.results based on journal_controlled
data <- left_join(dm.data,sherpa.results, by = "journal_controlled")