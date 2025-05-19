library(dplyr)
library(rentrez)
library(XML)

num_controls <- 7200
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
  print(paste0(as.character(starting_idex),
               " through ",
               as.character(min(starting_index + batch_size - 1,
                                num_controls)),
               "..."))
  xml_data <- entrez_fetch(db = "pubmed",
                           id = controls_pmids$pmid[starting_index:min(starting_index + batch_size - 1,
                                                                       num_controls)],
                           rettype = "xml", parsed = TRUE)
  root_node <- xmlRoot(xml_data)
  articleIdPubmeds <- xpathSApply(root_node, "//PubmedData/ArticleIdList/ArticleId[@IdType='pubmed']", xmlValue)
  articleIdLists <- getNodeSet(root_node, "//PubmedData/ArticleIdList") #[@IdType='doi']", xmlValue)
  pmid_doi_df <- rbind(pmid_doi_df, map_dfr(articleIdLists, extract_ids))
}


