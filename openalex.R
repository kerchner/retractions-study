# Setup ----

library(openalexR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(rcrossref)
library(tibble)
library(countrycode)
library(lubridate)

options(openalexR.mailto = "kerchner@gwu.edu")

# Function to calculate H-index from a list (as a skinny data frame) of citation counts ----
h_index <- function(citation_counts_df) {
  h_ind <- citation_counts_df %>%
    arrange(desc(cited_by_count)) %>%
    mutate(article_number = row_number()) %>%
    filter(cited_by_count >= article_number) %>%
    nrow()
  
  return(h_ind)
}

# Function to collect/compute author stats for works ----
author_and_citation_stats <- function(openalex_works_df, is_retractions = TRUE) {
  results_df <- data.frame()
  all_country_codes <- c()
  for(i in 1:nrow(openalex_works_df)) {
    print(paste("Paper number", i))
    
    work <- openalex_works_df[i, ]
    
    # Get the openalex ID (e.g. W1234567890)
    id <- str_remove(work$id, 'https://openalex.org/')
    
    # Get authors info
    authors_df <- work$authorships[[1]]
    if(nrow(authors_df) > 0) {
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
        if('NA' %in% country_codes) next
        authors_countries <- unique(c(authors_countries, country_codes))
      }
      
      authors_countries_list <- paste(authors_countries, collapse = ';')
      
      # First author countries
      first_author_df <- authors_df %>%
        filter(author_position == 'first')
      first_author_countries <- c()
      for (affiliation_df in first_author_df$affiliations) {
        country_codes <- affiliation_df$country_code
        if('NA' %in% country_codes) next
        first_author_countries <- unique(c(first_author_countries, country_codes))
      }
      first_author_countries_list <- paste(first_author_countries, collapse = ';')
      all_country_codes <- c(all_country_codes, first_author_countries)
      
      # Last author countries
      last_author_df <- authors_df %>%
        filter(author_position == 'last')
      last_author_countries <- c()
      for (affiliation_df in last_author_df$affiliations) {
        country_codes <- affiliation_df$country_code
        if('NA' %in% country_codes) next
        last_author_countries <- unique(c(last_author_countries, country_codes))
      }
      last_author_countries_list <- paste(last_author_countries, collapse = ';')
      all_country_codes <- c(all_country_codes, last_author_countries)
    } else {
      # We got no authorship info
      num_authors <- NA
      first_author_h_index <- NA
      last_author_h_index <- NA
      authors_countries_list <- NA
      first_author_countries_list <- NA
      last_author_countries_list < NA
    }
    
    # Citations info
    if (is_retractions) {
      retraction_date <- as.Date(work$RetractionDate, format = "%m/%d/%Y")
      print("   Fetching citations info and calculating citations before/after retraction")
      if (work$cited_by_count > 0) {
        citations_publications_dates <-
          # works citing this work
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
    } else {  # our works are not retracted so these are N/A
      citations_before_retraction <- NA
      citations_after_retraction <- NA 
    }
    
    results_row_df <- data.frame(doi = work$doi,
                                 issn = work$issn_l,
                                 num_authors,
                                 num_citations = work$cited_by_count,
                                 first_author_h_index, last_author_h_index,
                                 citations_before_retraction, citations_after_retraction,
                                 authors_countries_list,
                                 first_author_countries_list,
                                 last_author_countries_list)
    results_df <- rbind(results_df, results_row_df)
  }
  
  # Convert country names
  unique_country_codes <- unique(all_country_codes)
  country_names <- countrycode(unique_country_codes,
                               origin = 'iso2c', destination = 'country.name')
  country_convert_df <- data.frame(cbind(country_code = unique_country_codes,
                                         country_name = country_names))
  results_df <- left_join(results_df, country_convert_df,
                          by = c('first_author_countries_list' = 'country_code')) %>%
    rename(first_author_country_name = country_name)
  results_df <- left_join(results_df, country_convert_df,
                          by = c('last_author_countries_list' = 'country_code')) %>%
    rename(last_author_country_name = country_name)
  
  return(results_df)
}

# Populate info for retractions ----

retractions <- read.csv('Retraction_Clean_7.19 - RW_Clean_Data.csv') %>%
  select(-X) %>%
  filter(OriginalPaperPubMedID != 0) %>%
  filter(!(OriginalPaperDOI %in% c('unavailable', 'Unavailable', ''))) %>%
  drop_na(OriginalPaperDOI) %>%
  filter(RetractionNature == "Retraction") # This excludes Correction, Expression of concern, Reinstatement

retractions <- retractions %>%
  mutate(latency_days = as.numeric(as.Date(RetractionDate, format = "%m/%d/%Y") - 
           as.Date(OriginalPaperDate, format = "%m/%d/%Y")))

# Get rid of any records with DOIs that appear more than once.  This removes the following:
# 10.1007/s13277-014-2995-5
# 10.1016/j.lfs.2019.116709
retractions <- retractions %>%
  group_by(OriginalPaperDOI) %>%
  mutate(doi_count = n()) %>%
  ungroup() %>%
  filter(doi_count == 1) %>%
  select(-doi_count)

retractions <- retractions %>%
  mutate(OriginalPaperDOI_https = paste0('https://doi.org/', OriginalPaperDOI))

## get openalex info on the retraction papers ----
retractions_openalex_df <- oa_fetch(entity = 'works',
                                    doi = retractions$OriginalPaperDOI,
                                    #  options = list(select = c('id', 'publication_date', 'authorships',
                                    #                            'is_retracted')),
                                    verbose = TRUE)

# join on the retraction dates
retractions_openalex_df <- left_join(retractions_openalex_df,
                                     retractions %>% select(OriginalPaperDOI_https, RetractionDate),
                                     by = c("doi" = "OriginalPaperDOI_https"))

# populate first/last authors, countries, h-index values ----
retractions_author_stats_df <- author_and_citation_stats(openalex_works_df = retractions_openalex_df[3100:4000, ],
                                                         is_retractions = TRUE)
# Join on the new results
retractions_openalex_df <- left_join(retractions_openalex_df, retractions_author_stats_df)

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

results_full_df <- left_join(retractions_author_stats_df, retractions, by = c('doi' = 'OriginalPaperDOI_URL'))
results_full_df2 <- left_join(retractions, retractions_author_stats_df, by = c('OriginalPaperDOI_URL' = 'doi'))


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

controls_author_stats_df <- author_and_citation_stats(openalex_works_df = controls_df,
                                                      is_retractions = FALSE)

# Join on the new results
controls_df <- left_join(controls_df, controls_author_stats_df)

# Compile a list of ISSNs
all_issns <- unique(c(retractions_openalex_df$issn_l, controls_df$issn_l))
write.csv(x = data.frame(issn = all_issns) %>% filter(!is.na(issn)), file = 'all_issns.csv', row.names = FALSE, quote = FALSE, na = '')

## Determine cancer types ----
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

## Add author h-indices to controls_df ----



controls_crossref_df <- cr_works(controls_dois_df$doi[1:100])

# Table 2 - Reasons for retraction ----
reasons_df <- retractions %>%
  select(OriginalPaperDOI,Reason) %>%
  separate_longer_delim(Reason,";") %>%
  filter(str_trim(Reason)!="") %>%
  mutate(Reason=str_replace(Reason,"^\\+",""),
         value=1) %>%
  distinct() %>%
  pivot_wider(names_from=Reason,
              values_from=value)

reason_sums<-enframe(colSums(reasons_df%>%select(-OriginalPaperDOI),na.rm=TRUE),
                     name="Reason",value="n") %>%
  arrange(desc(n))

reason_sums%>%
  ggplot()+
  geom_col(aes(y=reorder(Reason,n),x=n))+
  labs(y="Reason for Retraction",
       x="Number of Publications")

write.csv(x = reason_sums, file = 'reason_sums.csv', row.names = FALSE)

# Table 2b - Article Types ----
article_types_df <- retractions %>%
  select(OriginalPaperDOI, ArticleType) %>%
  separate_longer_delim(ArticleType,";") %>%
  filter(str_trim(ArticleType) != "") %>%
  mutate(value = 1) %>%
  distinct() %>%
  pivot_wider(names_from=ArticleType,
              values_from=value)
articleType_sums<-enframe(colSums(article_types_df%>%select(-OriginalPaperDOI),na.rm=TRUE),
                     name="ArticleType",value="n") %>%
  arrange(desc(n))

articleType_sums %>%
  ggplot()+
  geom_col(aes(y=reorder(ArticleType,n),x=n))+
  labs(y="Article Type",
       x="Number of Publications")

write.csv(x = articleType_sums, file = 'articleType_sums.csv', row.names = FALSE)

retractions %>%
  ggplot() + 
  geom_histogram(aes(x = latency/365.25),
                 fill = 'white', color = 'black',
                 breaks = 0:35) +
  scale_y_log10()


retractions %>%
  ggplot() +
  geom_smooth(aes(x = as.Date(RetractionDate, format = "%m/%d/%Y"), y = latency)) +
  coord_cartesian(xlim = c(as.Date('1/1/2000', format = "%m/%d/%Y"),
                           as.Date('1/1/2025', format = "%m/%d/%Y")))

retractions %>%
  ggplot() +
  geom_histogram(aes(x = as.Date(OriginalPaperDate, format = "%m/%d/%Y")),
                 fill = 'white', color = 'black')

retractions %>%
  ggplot() +
  geom_histogram(aes(x = as.Date(RetractionDate, format = "%m/%d/%Y")),
                 fill = 'white', color = 'black')

range(as.Date(retractions$RetractionDate, format = "%m/%d/%Y"))
median(as.Date(retractions$RetractionDate, format = "%m/%d/%Y"))

range(as.Date(retractions$OriginalPaperDate, format = "%m/%d/%Y"))
median(as.Date(retractions$OriginalPaperDate, format = "%m/%d/%Y"))

# heat map of OriginalPaperDate vs. RetractionDate by year
#  format(as.Date('4/4/2024', format = "%m/%d/%Y"), '%Y')
