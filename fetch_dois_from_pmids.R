library(dplyr)
library(rentrez)
library(XML)
library(openalexR)
library(purrr)

num_controls <- 8400
batch_size <- 200

controls_pmids <- read.csv('cancer_pmids_shuf.txt',
                           header = FALSE,
                           nrows = num_controls,
                           col.names = 'pmid')

# Function to extract mini-data frame with pubmed ID and doi, from a single ArticleIDList node
extract_ids <- function(article_node) {
  pubmed <- xpathSApply(article_node, ".//ArticleId[@IdType='pubmed']", xmlValue)
  doi <- xpathSApply(article_node, ".//ArticleId[@IdType='doi']", xmlValue)
  data.frame(
    pubmed = ifelse(length(pubmed) > 0, pubmed, NA),
    doi = ifelse(length(doi) > 0, doi, NA),
    stringsAsFactors = FALSE
  )
}

pmid_doi_df <- data.frame()
for(starting_index in seq(1, num_controls, by = batch_size)) {
  print(paste0(as.character(starting_index),
               " through ",
               as.character(min(starting_index + batch_size - 1,
                                num_controls)),
               "..."))
  xml_data <- entrez_fetch(db = "pubmed",
                           id = controls_pmids$pmid[starting_index:min(starting_index + batch_size - 1,
                                                                       num_controls)],
                           rettype = "xml", parsed = TRUE)
  root_node <- xmlRoot(xml_data)
  #articleIdPubmeds <- xpathSApply(root_node, "//PubmedData/ArticleIdList/ArticleId[@IdType='pubmed']", xmlValue)
  articleIdLists <- getNodeSet(root_node, "//PubmedData/ArticleIdList") #[@IdType='doi']", xmlValue)
  pmid_doi_df <- rbind(pmid_doi_df, map_dfr(articleIdLists, extract_ids))
}

# Drop anything with no DOI.
pmid_doi_df <- pmid_doi_df %>%
  drop_na()

# Check for retracted, using OpenAlex
options(openalexR.mailto = "kerchner@gwu.edu")
controls_df <- oa_fetch(entity = 'works',
                        doi = pmid_doi_df$doi,
                        options = list(select = c('id', 'doi', 'is_retracted')),
                        verbose = TRUE)

# remove retracted
# 8400 yields 7443
controls_df <- controls_df %>%
  filter(!is_retracted)

# Filter pmid_doi_df based on dois in the filtered-down DOI list
controls_df2 <- pmid_doi_df %>%
  filter(paste0('https://doi.org/', doi) %in% controls_df$doi)

write.csv(controls_df2, 'controls_pmid_doi.csv')
