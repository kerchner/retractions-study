library(openalexR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(crossref)

# Calculate H-index from a list (as a skinny data frame) of citation counts
h_index <- function(citation_counts_df) {
  h_ind <- citation_counts_df %>%
    arrange(desc(cited_by_count)) %>%
    mutate(article_number = row_number()) %>%
    filter(cited_by_count >= article_number) %>%
    nrow()
  
  return(h_ind)
}

# Populate info for retractions ----

retractions <- read.csv('Retraction_Clean_7.19 - RW_Clean_Data.csv') %>%
  select(-X) %>%
  filter(OriginalPaperPubMedID != 0) %>%
  filter(!(OriginalPaperDOI %in% c('unavailable', 'Unavailable', ''))) %>%
  drop_na(OriginalPaperDOI)

dois <- retractions$OriginalPaperDOI
RetractionDate <- retractions$RetractionDate

options(openalexR.mailto = "kerchner@gwu.edu")

# get info on the papers
works_df <- oa_fetch(entity = 'works',
                     doi = dois,
                   #  options = list(select = c('id', 'publication_date', 'authorships',
                   #                            'is_retracted')),
                     verbose = TRUE)

results_df <- data.frame()

for(i in 1:10) { #nrow(works_df)) {
  print(paste("Paper number", i))
  
  work <- works_df[i, ]
  
  # Get the openalex ID (e.g. W1234567890)
  id <- str_remove(work$id, 'https://openalex.org/')
  
  # Get authors info
  authors_df <- work$authorships[[1]]
  
  # Get first author
  first_author_id <- authors_df %>%
    filter(author_position == 'first') %>%
    pull(id)
  
  num_authors <- nrow(authors_df)
  
  first_author_id <- str_remove(first_author_id, 'https://openalex.org/')
  
  # Citation counts and h-index for first and last authors
  # Ref: https://github.com/ourresearch/openalex-api-tutorials/blob/main/notebooks/authors/hirsch-index.ipynb
  
  print("   Fetching first author info")
  first_author_citation_counts_df <- oa_fetch(entity = 'works',
                              author.id = first_author_id,
                              options = list(select = c('cited_by_count')),
                              #options = list(select = c('')),  # which columns do I want?
                              verbose = TRUE)
  
  first_author_h_index <- h_index(first_author_citation_counts_df)
  
  if('last' %in% authors_df$author_position) {
    # Get last author
    last_author_id <- authors_df %>%
      filter(author_position == 'last') %>%
      pull(id)
    
    last_author_id <- str_remove(last_author_id, 'https://openalex.org/')
 
    print("   Fetching last author info")   
    last_author_citation_counts_df <- oa_fetch(entity = 'works',
                                                author.id = last_author_id,
                                                options = list(select = c('cited_by_count')),
                                                #options = list(select = c('')),  # which columns do I want?
                                                verbose = TRUE)
    
    last_author_h_index <- h_index(last_author_citation_counts_df)
  } else {
    last_author_h_index <- first_author_h_index # since there's only 1 author
  }
  
  # Author countries
  authors_countries <- c()
  for (affiliation_df in authors_df$affiliations) {
    country_codes <- affiliation_df$country_code
    authors_countries <- unique(c(authors_countries, country_codes))
  }
  
  authors_countries_list <- paste(authors_countries, collapse = ';')
  
  retraction_date <- as.Date(RetractionDate[i], format = "%m/%d/%Y")
  
  # Get citations' publications dates, 
  # so that we can determine how many before vs. after retraction
  print("   Fetching citations info")
  if (work$cited_by_count > 0) {
    citations_publications_dates <-
      oa_fetch(entity = 'works',
               cites = id,
               options = list(select = c("publication_date")),
               verbose = TRUE) %>%
      pull(publication_date)
      
    citations_before_retraction <- sum(citations_publications_dates <= retraction_date)
    citations_after_retraction <- sum(citations_publications_dates > retraction_date)
  } else {
    citations_before_retraction <- NA
    citations_after_retraction <- NA
  }
  
  results_row_df <- data.frame(doi = work$doi,
                               num_authors,
                               #num_citations = work$cited_by_count,
                               first_author_h_index, last_author_h_index,
                               citations_before_retraction, citations_after_retraction,
                               authors_countries_list)
  results_df <- rbind(results_df, results_row_df)
}


# Tally up the weighted concept terms
concepts_list <- list()
levels_list <- list()
num_papers_list <- list()

for(i in 1:nrow(works_df)) {
  work <- works_df[i, ]
  concepts_i <- work$concepts[[1]]
  
  for (j in 1:nrow(concepts_i)) {
    concept_j <- concepts_i[j, ]
    if (concept_j$display_name %in% names(concepts_list)) {
      concepts_list[concept_j$display_name] <- concepts_list[concept_j$display_name][[1]] + concept_j$score
      num_papers_list[concept_j$display_name] <- num_papers_list[concept_j$display_name][[1]] + 1
    } else {
      concepts_list[concept_j$display_name] <- concept_j$score
      levels_list[concept_j$display_name] <- concept_j$level
      num_papers_list[concept_j$display_name] <- 1
    }
  }
}

concepts_df <- data.frame(concept = names(concepts_list),
                          level = unlist(levels_list),
                          total_score = unlist(concepts_list),
                          num_papers = unlist(num_papers_list)) %>%
  arrange(level, desc(total_score))


# Merge with retractions data
retractions <- retractions %>%
  mutate(OriginalPaperDOI_URL = paste0('https://doi.org/', OriginalPaperDOI))

results_full_df <- left_join(results_df, retractions, by = c('doi' = 'OriginalPaperDOI_URL'))
results_full_df2 <- left_join(retractions, results_df, by = c('OriginalPaperDOI_URL' = 'doi'))

# Populate topics
topics_df <- read.csv('Top 20 Cancers OpenAlex Topic IDs - Sheet1.csv')

works_df$cancers <- NA
for(i in 1:nrow(works_df)) {
  work <- works_df[i, ]
  topics_i <- work$topics[[1]]
  
  if(nrow(topics_i) == 0) {
    next
  }
  cancers <- c()
  for (j in 1:nrow(topics_i)) {
    topic_j <- topics_i[j, ]
    if (topic_j$id %in% topics_df$Topic.ID) {
      cancers <- union(cancers,
                   topics_df %>% filter(Topic.ID == topic_j$id) %>% pull(Cancer.Name))
    }
  }
  if (length(cancers) > 0) {
    works_df[i, ]$cancers <- paste(cancers, collapse=';')
  }
}

# Populate info for controls ----
controls_dois_df <- read.csv('controls_pmid_doi.csv')

controls_df <- oa_fetch(entity = 'works',
                     doi = controls_dois_df$doi,
                     #  options = list(select = c('id', 'publication_date', 'authorships',
                     #                            'is_retracted')),
                     verbose = TRUE)

# Determine cancer types
controls_df$cancers <- NA
for(i in 1:nrow(controls_df)) {
  work <- controls_df[i, ]
  topics_i <- work$topics[[1]]
  
  if(nrow(topics_i) == 0) {
    next
  }
  cancers <- c()
  for (j in 1:nrow(topics_i)) {
    topic_j <- topics_i[j, ]
    if (topic_j$id %in% topics_df$Topic.ID) {
      cancers <- union(cancers,
                       topics_df %>% filter(Topic.ID == topic_j$id) %>% pull(Cancer.Name))
    }
  }
  if (length(cancers) > 0) {
    controls_df[i, ]$cancers <- paste(cancers, collapse=';')
  }
}


controls_crossref_df <- cr_works(controls_dois_df$doi[1:100])
