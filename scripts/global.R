library(stringr)
library(memoise)
library(tidytext)
library(RISmed)

load_pubmed <- memoise(function(word, tot_records){
  
  search_topic <- word
  
  search_query <- EUtilsSummary(word, type="esearch", db="pubmed", datetype='pdat', 
                                                mindate=1975, maxdate=2015, retmax=tot_records)
  
  records<- EUtilsGet(search_query, type="efetch", db="pubmed")
  
  pubmed_data<-data.frame('year'=records@YearPubmed, 'abstract'=records@AbstractText, 'query'= rep(records@Query, length(records@AbstractText)))
  pubmed_data <- pubmed_data[!(pubmed_data$abstract==""),]
  pubmed_data
  
})

getTerms <- memoise(function(dtset, year, rm_words, join_words) {
  
  df_articles <- dtset
  
  if (any(unique(as.numeric(df_articles$year)) %in% c(year)) == TRUE){
    
    df_articles <- df_articles[df_articles$year==c(year),]
    
    df_articles$year <-as.integer(df_articles$year)
    
    dataset_abstract <- data.frame(year = df_articles$year, abstract = df_articles$abstract, stringsAsFactors = FALSE)
    
    dataset_abstract <- tbl_df(dataset_abstract)
    dataset_abstract$abstract <- as.character(dataset_abstract$abstract)
    
    abstract_words <- dataset_abstract %>%
      unnest_tokens(word, abstract) %>%
      count(word, sort=TRUE) %>%
      filter(str_detect(word, "[a-z']$"),
             !word %in% stop_words$word) %>%
      ungroup()
    
    mystopwords <- data_frame(word = as.character(rm_words))
    
    abstract_words<-anti_join(abstract_words, mystopwords, by = "word")
    
    if(!is.null(join_words) & (length(join_words)!=1)){
      
      vct_words <- c(join_words)
      print(vct_words)
      
      add_abstract_words <- abstract_words %>%
        filter(word %in% vct_words) %>% summarize(n = sum(n)) %>% mutate(word = vct_words[1]) %>% select(word, n)
      
      abstract_words[abstract_words$word == vct_words[1],] <- add_abstract_words
      abstract_words <- abstract_words[!(abstract_words$word %in% vct_words[2:length(vct_words)]),]
      print(abstract_words)
      
    }
    
    abstract_words <- as.data.frame(abstract_words)
    
    freq_words <- as.numeric(abstract_words$n)
    names(freq_words) <- as.character(abstract_words$word)
    
    freq_words <<- freq_words
    freq_words
  }
  
})

load_corpus <- memoise(function(df_abs){
  
  corp <- Corpus(DataframeSource(df_abs))
  
  corp <- tm_map(corp, content_transformer(tolower))
  corp <- tm_map(corp, removeNumbers)
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, content_transformer(stripWhitespace))
  corp <- tm_map(corp, removeWords, stop_words$word)
  corp
  
})