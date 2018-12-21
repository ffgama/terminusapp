# load the libraries
library(beepr)
library(data.table)
library(DT)
library(ggplot2)
library(RefManageR)
library(dplyr)
library(tidyr)
library(widyr)
# tm: v.0.6.1
library(tm)
library(graph)
library(igraph)
library(ggraph)
library(corrplot)
library(Rgraphviz)
library(directlabels)
library(reshape2)
library(feather)
library(devtools)

# library(devtools)
# devtools::install_github("wesm/feather/R")

#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")

# setar no deploy
# options(repos = BiocInstaller::biocinstallRepos())
# getOption("repos")

server <- function(input, output, session) {
  
  Dataset <- reactive({
    
    try_test <- input$dtset_test
      
    if (try_test == TRUE)
    {
      req(try_test)
      source("df_articles.R")
      df_articles 
    }else{
      source("df_articles.R")
      df_articles
    }
    
  })
  
  word <- eventReactive(input$wordBtn, {input$text})
  
  record <- eventReactive(input$wordBtn, {input$num_records})
  
  load_data <- reactive({
    
    open_index<-Dataset()
    
    try_test <- input$dtset_test
    
    if(try_test == TRUE)
    {
      source("df_articles.R")
      df_articles 
      
    }else{
      input$wordBtn
      auth.pm <- load_pubmed(word(), record())
      df_articles <<- auth.pm
      df_articles
      
    }
    
  })
  
  output$show_query <- renderText({
    
    df_articles <- load_data()
    df_articles <- as.character(df_articles$query)
    unique(paste(df_articles))
    
  })
  
  terms <- reactive({
    
    open_index<-Dataset()
    
    df_articles <- load_data()
    
    df_articles <- df_articles[,c("year","abstract")]
    
    getTerms(df_articles, input$year, input$words, input$combining_words)
    
  })
  
  load_word <- reactive({ 
    
    df<-load_data()
    
    isolate({
      getTerms(df, c(df$year), input$words, input$combining_words)
    })
    
  })
  
  year <- reactive({
    
    df <- load_data()
    df_years<-NULL
    
    y <- df %>% select(year)
    y <- unique(y)
    
    for (i in 1:nrow(y)){
      get_terms<- (getTerms(df, y$year[i], " ", " "))
      df_years <- rbind(df_years, data.frame(word = names(get_terms), freq = as.numeric(get_terms), year = y$year[i]))
    }
    df_years
  })
  
  observeEvent(input$jumpToHelp, {
    updateTabsetPanel(session, "MainNav",
                      selected = "panel1")
  })
  
  observeEvent(input$jumpToHomePage, {
    updateTabsetPanel(session, "MainNav",
                      selected = "HomePage")
  })
  
  output$paper_year <- renderPlot({
    
    df <- load_data()
    
    y <- df %>% select(year)
    
    df_y<-table(y)
    
    df_y<-data.frame(ano = names(df_y), frequency = as.numeric(df_y))
    
    ggplot(df_y, aes(ano, frequency)) + 
      coord_flip() + geom_bar(stat="identity", position = 'dodge',  fill="steelblue") +
      geom_text(aes(label=frequency), position=position_dodge(width=1), hjust=-0.5, inherit.aes = TRUE) + 
      xlab("\nPublication Year") + ylab("Frequency") + 
      theme(text = element_text(size=20,face = "bold", family = 'Open Sans')) + ggtitle("\nPAPER FREQUENCY BY YEAR\n") +
      theme_classic(base_size = 20) + scale_fill_brewer(palette="Dark2")
    
  })
  
  output$flare <- renderUI({
    df <- load_data()
    includeHTML("www/index.html")
  })
  
  observe({
    
    df<-load_data()
    
    df_load_word <- year()
    df_load_word <- as.data.frame(table(df_load_word$year))
    colnames(df_load_word)<-c("id", "value")
    
    write.csv(df_load_word, file="www/words_year.csv", row.names = FALSE)
    
  })
  
  observe({
    updateSelectInput(session, inputId="words",
                      choices=names(load_word()), selected=NULL)
  })
  
  observe({
    updateSelectInput(session, inputId="combining_words",
                      choices=names(load_word()), selected=NULL)
  })
  
  output$slider <- renderUI({
    
    df<-load_data()
    
    sliderInput("year", label= "Publication Year:" ,  min = min(as.numeric(df$year)), max = max(as.numeric(df$year)), 
                value=min(as.numeric(df$year)), step = 2, sep = "")
  })
  
  wordcloud_rep <- repeatable(wordcloud2)
  
  output$word_cloud <- renderWordcloud2({
    
    v<-terms()
    
    df<-data.frame(word=names(v), freq=as.numeric(v))
    
    if ((!is.null(v)) && (!nrow(df)==0)){
      wordcloud_rep(df, size=0.9,color='random-dark', gridSize = 2.0)
    }else if(!is.null(Dataset()) && (is.null(v)) && (nrow(df)==0)){
      return()
    }
  })
  
  output$downloadDf <- renderUI({
    df <- load_data()
    downloadButton("downloadDataDf", "Download Abstracts")
  })
  
  dataframe_abstract <- reactive({
    
    df<-load_data()
    df <- df[,c("year","abstract")]
    return(df)
    
  })
  
  output$downloadDataDf <- downloadHandler(filename = function(){
      paste(input$file1, sep = "")
    }, content = function(file) {
      write.csv(dataframe_abstract(),file, row.names = FALSE)
    }
  )
  
  output$dataframe <- DT::renderDataTable(DT::datatable({
    
    dataframe_abstract()
    
  },  style = "bootstrap", rownames = FALSE, options = list(lengthMenu = c(5, 30, 50), pageLength = 5)))
  
  output$summary <- renderPrint({
    
    v <- terms() 
    
    df<-data.frame(frequency=as.numeric(v))    
    
    summary(df)
    
  })
  
  abstract_terms <- reactive({
    
    df <- load_data()
    
    df$abstract <- as.character(df$abstract)
    
    abstract_words <- df %>%
      unnest_tokens(word, abstract) %>%
      count(year, word, sort=TRUE) %>%
      filter(str_detect(word, "[a-z']$"),
             !word %in% stop_words$word) %>%
      ungroup()
    
    
  })
  
  output$sliderYear<-renderUI({
    
    df<-load_data()
    
    sliderInput("year_tf_idf", label= "Publication Year:" ,  min = min(as.numeric(df$year)), max = max(as.numeric(df$year)), 
                value=min(as.numeric(df$year)), step = 1, sep = "")  
    
  })
  
  observe({
    if (!is.null(names(load_word()))){
      updateSelectInput(session, inputId="select_words",
                        choices=names(load_word()), selected=names(load_word())[1])
    }
    else{
      return()
    }
  })
  
  output$word_freq <- renderPlot({
    
    abstract_words <- abstract_terms()
    df_words_year <- as.data.frame(abstract_words)
    split_year <- split(df_words_year, df_words_year$year)
    df_group_year<-rbindlist(split_year, fill=TRUE)
    
    entries<-c(input$select_words)
    
    select_year<-data.frame(year=as.numeric(unique(df_group_year$year)))
    
    join_df<-merge(select_year, as.data.frame(df_group_year[df_group_year$word %in% c(entries),]), all = TRUE)
    
    ggplot(join_df, aes(x = year, y = n, group = word, colour = word)) + 
      geom_point(size=2) +
      geom_line(size = 1) +
      labs(x = "publication year",  y = "frequency") +
      scale_colour_discrete(guide = 'none')  + 
      scale_x_continuous(breaks=seq(min(unique(as.numeric(join_df$year))),max(unique(as.numeric(join_df$year))), by = 1)) +          
      geom_dl(aes(label = word), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.7,vjust=-.8)) +
      geom_dl(aes(label = n), method = list(dl.trans(x = x - 0.2), "first.points", cex = 0.8, vjust=-.8)) + 
      theme_minimal(base_size = 14)
    
    
  })
  
  output$controlTermsTfIdf<-renderUI({
    
    df<-load_data()
    sliderInput(inputId = "bins", label = "Number of terms:", 
                min = 1,
                max = 50,
                value = 30)
  })
  
  output$control_min_max_freq <- renderUI({
    
    data_load<-load_data()
    
    sliderInput(inputId = "bins_min_max_freq", label = "Number Words:", 
                min = 1,
                max =  100,
                value = 30)
    
  })
  
  output$control_stacks <- renderUI({
    
    data_load<-load_data()
    
    sliderInput(inputId = "bins_stacks", label = "Number Stacks:", 
                min = 1,
                max =  100,
                value = 60)
  })
  
  output$sliderControlYear <- renderUI({
    
    abstract_words <- abstract_terms()
    df_words_year <- as.data.frame(abstract_words)
    
    df_words_year$year <- as.numeric(df_words_year$year)
    
    sliderInput("range", "Range of Years:",
                min = min(df_words_year$year), max = max(df_words_year$year),
                value = c(min(df_words_year$year), max(df_words_year$year)))
    
    
  })
  
  output$interest_words <- renderPlot({
    
    abstract_words <- abstract_terms()
    
    df_words_year <- as.data.frame(abstract_words)
    split_year <- split(df_words_year, df_words_year$year)
    df_group_year<-rbindlist(split_year, fill=TRUE)
    
    df_group_year$n <- as.numeric(df_group_year$n)
    df_group_year<-df_group_year %>% arrange(word)
    
    df_year <- cbind(df_group_year, dif=diff(c(NA,df_group_year$n)))
    
    df_count_word<-data.frame(table(df_year$word))
    colnames(df_count_word) <- c("word","tot_records")
    
    combining <- merge(df_count_word, df_year, all=TRUE)
    
    interval <- input$range
    combining<-combining[combining$year %in% interval[1]:interval[2],]
    
    if(input$opt_dif_occ == "difference"){
      
      combining <- combining[,c(1,3,5)]
      
      if(input$disp == "decreasing"){
        
        combining_melt <- melt(combining, id.vars = c("word","year"))
        combining_melt$word <- as.factor(combining_melt$word)
        combining_melt$year <- as.factor(combining_melt$year)
        combining_melt <- combining_melt[order(combining_melt$value),]
        combining_melt<-head(combining_melt, input$bins_min_max_freq)
        
        library(plyr)
        Data <- ddply(combining_melt, .(year), 
                      transform, pos = cumsum(value) * (0.5 * value)
        )
        detach("package:plyr")
        
        ggplot(Data, aes(reorder(word, value), value,  fill = forcats::fct_rev(year), label = value)) +
          geom_bar(stat = "identity") + 
          coord_flip() +
          
          geom_text(size = 3, position = position_stack(vjust = 0.5)) + 
          xlab('') + ylab('cumulative frequency') + 
          scale_fill_discrete(name="Year\n") +
          theme_minimal(base_size = 14)
        
        
      }else if(input$disp == "ascending"){
        
        combining_melt <- melt(combining, id.vars = c("word","year"))
        combining_melt$word <- as.factor(combining_melt$word)
        combining_melt$year <- as.factor(combining_melt$year)
        combining_melt <- combining_melt[order(-combining_melt$value),]
        combining_melt<-head(combining_melt, input$bins_min_max_freq)
        
        
        library(plyr)
        Data <- ddply(combining_melt, .(year), 
                      transform, pos = cumsum(value) * (0.5 * value)
        )
        detach("package:plyr")
        
        ggplot(Data, aes(reorder(word, -value), value,  fill = forcats::fct_rev(year), label = value)) +
          geom_bar(stat = "identity") + 
          coord_flip() +
          geom_text(size = 3, position = position_stack(vjust = 0.5)) + 
          xlab('') + ylab('cumulative frequency') + 
          scale_fill_discrete(name="Year\n") +
          theme_minimal(base_size = 14)
        
      }
    }else if (input$opt_dif_occ == "occurence"){
      
      combining <- combining[,c(1,2,3,4,5)]
      
      if(input$disp == "decreasing"){
        
        combining_melt <- melt(combining, id.vars = c("word","year"))
        combining_melt$word <- as.factor(combining_melt$word)
        combining_melt$year <- as.factor(combining_melt$year)
        combining_melt <- combining_melt[order(combining_melt$value),]
        combining_melt <- head(combining_melt, input$bins_stacks)
        
        library(plyr)
        Data <- ddply(combining_melt, .(year), 
                      transform, pos = cumsum(value) * (0.5 * value)
        )
        detach("package:plyr")
        
        ggplot(Data, aes(reorder(word, value), value,  fill = forcats::fct_rev(year), label = value)) +
          geom_bar(stat = "identity") + 
          coord_flip() +
          geom_text(size = 3, position = position_stack(vjust = 0.5)) + 
          xlab('') + ylab('cumulative frequency') + 
          scale_fill_discrete(name="Year\n") +
          theme_minimal(base_size = 14)
        
        
      }else if(input$disp == "ascending"){
        
        combining_melt <- melt(combining, id.vars = c("word","year"))
        combining_melt$word <- as.factor(combining_melt$word)
        combining_melt$year <- as.factor(combining_melt$year)
        combining_melt <- combining_melt[order(-combining_melt$value),]
        combining_melt <- head(combining_melt, input$bins_min_max_freq)
        
        library(plyr)
        Data <- ddply(combining_melt, .(year), 
                      transform, pos = cumsum(value) * (0.5 * value)
        )
        detach("package:plyr")
        
        ggplot(Data, aes(reorder(word, -value), value,  fill = forcats::fct_rev(year), label = value)) +
          geom_bar(stat = "identity") + 
          coord_flip() +
          geom_text(size = 3, position = position_stack(vjust = 0.5)) + 
          xlab('') + ylab('cumulative frequency') + 
          scale_fill_discrete(name="Year\n") +
          theme_minimal(base_size = 14)
        
      }
      
    }
    
  })
  
  output$tf_idf <- renderPlot({
    
    abstract_words <- abstract_terms()
    
    tf_idf <- abstract_words %>%
      bind_tf_idf(word, year, n) %>%
      arrange(year, desc(tf_idf))
    
    df_words_year <- as.data.frame(tf_idf)
    split_year <- split(df_words_year, df_words_year$year)
    
    df_group_year<-rbindlist(split_year, fill=TRUE)
    
    df_group_year<-as.data.frame(df_group_year[df_group_year$year %in% c(input$year_tf_idf),])
    
    df_group_year$word <- factor(df_group_year$word , levels = df_group_year$word)
    
    df_group_year <- tbl_df(df_group_year)
    
    options(warn = -1) 
    
    df_group_year$tf_idf <- round(as.numeric(df_group_year$tf_idf), digits = 3)
    
    df_group_year %>%
      filter(word == head(word,input$bins)) %>%
      ggplot(aes(x=reorder(word, tf_idf), tf_idf), fill=tf_idf) +  geom_bar(stat="identity") +
      geom_col() +
      geom_text(aes(label = tf_idf),  hjust = -0.5, size = 5, position = position_dodge(width = 1)) +
      theme_classic(base_size = 20) +
      ggtitle("\nTF-IDF measure: weight of words\n") + 
      labs(x = NULL, y = "tf_idf") + 
      coord_flip()
    
  })
  
  dataset_select<-reactive({
    
    df<-load_data()
    
    getTerms(df, input$yearBi, "", "")
    
  })
  
  output$sliderBigram <- renderUI({
    
    df<-load_data()
    
    sliderInput("yearBi", label= "Publication Year:" ,  min = min(as.numeric(df$year)), max = max(as.numeric(df$year)), 
                value=min(as.numeric(df$year)), step = 2, sep = "")
  })
  
  observe({
    updateSelectInput(session, inputId = "bigram_txt", choices = names(dataset_select()), selected = NULL)
  })
  
  load_bigrams <- reactive({
    
    dtset_abstract <- load_data()
    
    dataset_abstract <- data.frame(year = dtset_abstract$year, abstract = dtset_abstract$abstract, stringsAsFactors = FALSE)
    dataset_abstract <- tbl_df(dataset_abstract)
    
    abstract_bigrams <- dataset_abstract %>%
      unnest_tokens(bigram, abstract, token = "ngrams", n = 2)
    
    bigrams_separated <- abstract_bigrams %>%
      separate(bigram, c("word_after", "word_before"), sep = " ")
    
    bigrams_filtered <- bigrams_separated %>%
      filter(!word_after %in% stop_words$word) %>%
      filter(!word_before %in% stop_words$word)
    bigrams_filtered
    
  })
  
  output$control_bigram_tf_idf <- renderUI({
    
    bigrams_filtered <- load_bigrams()
    
    bigrams_united <- bigrams_filtered %>%
      unite(bigram, word_after, word_before, sep = " ")
    
    bigram_tf_idf <- bigrams_united %>%
      count(year, bigram) %>%
      bind_tf_idf(bigram, year, n) %>%
      arrange(desc(tf_idf)) %>%
      mutate(bigram = factor(bigram, levels = rev(unique(bigram))))
    
    tot <- sum(table(bigram_tf_idf$year))
    
    if(tot >= 500){
      sliderInput(inputId = "bins_bigram",
                  label = "Number Top-bigrams:",
                  min = 1,
                  max = 500,
                  value = 30)
    }else if(tot <= 500){
      sliderInput(inputId = "bins_bigram",
                  label = "Number Top-bigrams:",
                  min = 1,
                  max = 100,
                  value = 30)
    }else if(tot==0){
      return()
    }
    
  })
  
  output$bi_gram_tf_idf <- renderPlot({
    
    bigrams_filtered <- load_bigrams()
    
    bigrams_united <- bigrams_filtered %>%
      unite(bigram, word_after, word_before, sep = " ")
    
    bigram_tf_idf <- bigrams_united %>%
      count(year, bigram) %>%
      bind_tf_idf(bigram, year, n) %>%
      arrange(desc(tf_idf)) %>%
      mutate(bigram = factor(bigram, levels = rev(unique(bigram))))
    
    bigram_tf_idf %>% 
      top_n(input$bins_bigram) %>%
      group_by(year) %>% 
      ungroup() %>%
      ggplot(aes(bigram, tf_idf, fill = year)) +
      geom_col(show.legend = FALSE) +
      theme(text = element_text(size=14)) + 
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(~year, ncol = 5, scales = "free") +
      coord_flip()
    
  })
  
  output$control_network<-renderUI({
    
    bigrams_filtered <- load_bigrams()
    
    bigrams_united <- bigrams_filtered %>%
      unite(bigram, word_after, word_before, sep = " ")
    
    bigram_counts <- bigrams_filtered %>%
      count(word_after, word_before, sort = TRUE)
    
    if(bigram_counts >=800){
      sliderInput(inputId = "bins_network",
                  label = "Number Obs:",
                  min = 1,
                  max = (max(bigram_counts$n)-1),
                  value = (max(bigram_counts$n)/7),
                  step=1
      )  
    }else{
      sliderInput(inputId = "bins_network",
                  label = "Number Obs:",
                  min = 1,
                  max = (max(bigram_counts$n)-1),
                  value = (max(bigram_counts$n)/2),
                  step=1
      )
    }
    
    
    
  })
  
  output$network_bigram <- renderForceNetwork({
    
    bigrams_filtered <- load_bigrams()
    
    bigram_counts <- bigrams_filtered %>%
      count(word_after, word_before, sort = TRUE)
    
    bigram_graph <- bigram_counts %>% filter(n>input$bins_network) %>% graph_from_data_frame()
    
    set.seed(1234)
    
    df_bigram<-as.data.frame(as.matrix(bigram_graph[]))
    
    data <- as.matrix(df_bigram)
    
    network<-graph_from_adjacency_matrix(data)
    
    network<-igraph_to_networkD3(network)
    
    size_nodes <- nrow(network$nodes) - 1
    
    network$nodes<-data.frame(id=0:size_nodes, name = network$nodes$name)
    
    network$links$source<-network$nodes[match(network$links$source, network$nodes$id),2]
    network$links$target<-network$nodes[match(network$links$target, network$nodes$id),2]
    
    g<- simpleNetwork(network$links,
                      height = 1200,                     
                      width = 1200,
                      linkDistance = 120,              
                      charge = -80,                    
                      fontSize = 22,                    
                      linkColour = rgb(0.1,0.9,0.1,0.3),
                      nodeColour = "#2A93B9",       
                      opacity = 0.9                    
    )
    
    g
    
  })
  
  output$bigramtxt_after <- DT::renderDataTable(DT::datatable({
    
    bigrams_filtered <- load_bigrams()
    
    bi_filter<-bigrams_filtered %>%
      filter(word_after == input$bigram_txt) %>%
      count(year, word_before, sort = TRUE)
    
    tf_idf <- bi_filter %>%
      bind_tf_idf(word_before, year, n) %>%
      arrange(year, desc(tf_idf))
    tf_idf
    
    if (is.null(tf_idf) | (nrow(tf_idf)==0))
    {
      return()
      
    }else{
      split_year <- split(tf_idf, tf_idf$year)
      
      df_split_year <- as.data.frame(rbindlist(split_year))
      colnames(df_split_year) <- c("Year","Next Word", "Frequency", "Tf", "Idf", "Tf_Idf")
      
    }
    df_split_year
    
  },  style = "bootstrap", rownames = FALSE,  caption = "Words usually appear AFTER...", options = list(searching = FALSE, pageLength = 5)))
  
  output$bigramtxt_before <- DT::renderDataTable(DT::datatable({
    
    bigrams_filtered <- load_bigrams()
    
    bi_filter<-bigrams_filtered %>%
      filter(word_before == input$bigram_txt) %>%
      count(year, word_after, sort = TRUE)
    
    tf_idf <- bi_filter %>%
      bind_tf_idf(word_after, year, n) %>%
      arrange(year, desc(tf_idf))
    tf_idf
    
    if (is.null(tf_idf) | (nrow(tf_idf)==0))
    {
      return()
      
    }else{
      
      split_year <- split(tf_idf, tf_idf$year)
      
      df_split_year <- as.data.frame(rbindlist(split_year))
      
      colnames(df_split_year) <- c("Year","Previous Word", "Frequency", "Tf", "Idf", "Tf_Idf")
      
    }
    df_split_year
    
  }, style = "bootstrap", rownames = FALSE,  caption = "Words usually appear BEFORE...",  options = list(searching = FALSE, pageLength = 5)))
  
  load_corr <- reactive({
    
    dtset_abstract <- load_data()
    
    df_abs<-data.frame(abstract=dtset_abstract[,c("abstract")])
    
    corp <- load_corpus(df_abs)
    
    tdm <<- TermDocumentMatrix(corp)
    
  })
  
  tdm_words <-reactive({
    
    tdm <- load_corr()
    tdm$dimnames$Terms
    
  })
  
  observe({
    updateSelectInput(session, inputId = "corr_txt", choices = names(corr_terms()))
  })
  
  output$sliderYearCorr<-renderUI({
    
    df<-load_data()
    
    sliderInput("year_corr", label= "Year" ,  min = min(as.numeric(df$year)), max = max(as.numeric(df$year)), 
                value=min(as.numeric(df$year)), step = 1, sep = "")  
    
  })
  
  corr_terms <- reactive({
    
    open_index<-Dataset()
    
    df_articles <- load_data()
    
    df_articles <- df_articles[,c("year","abstract")]
    
    getTerms(df_articles, input$year_corr,"","")
    
  })
  
  output$corr_plot <- renderPlot({
    
    tdm <- load_corr()
    
    assocs <-findAssocs(tdm, term=input$corr_txt, corlimit =  0.12)
    assocs
    
    convert_assocs <- as.data.frame(assocs)
    
    df_assocs <-data.frame(word = rownames(convert_assocs), percent = convert_assocs)
    rownames(df_assocs) <- NULL
    colnames(df_assocs) <- c("word","percent")
    
    df_assocs %>%
      filter(word == head(word,input$bins_corr)) %>%
      ggplot(aes(reorder(word, percent), percent, fill=percent)) +
      xlab(NULL) +
      ylab("\npercent")+
      geom_col() +
      geom_text(aes(label = percent),  hjust = -0.5, size = 5, position = position_dodge(width = 1)) +
      theme(text = element_text(size=20, family = 'Open Sans')) + 
      theme_classic(base_size = 16) + theme(legend.position="none") +
      coord_flip()
    
    
  })
  
  output$sentiment_plot <- renderPlot({
    
    abstract_words <- abstract_terms()
    
    abstracts_sentiment <- abstract_words %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment) %>%
      spread(sentiment, nn, fill = 0) %>%
      mutate(sentiment = positive - negative)
    
    abstracts_sentiment <- abstracts_sentiment %>%  filter(word == head(word, input$bins_sentiment))
    
    abstracts_sentiment$fill <- ifelse(abstracts_sentiment$sentiment < 0, "blue", "red")
    
    
    ggplot(abstracts_sentiment, aes(word, sentiment, fill = fill)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = sentiment),  hjust = -0.5, size=4, position = position_dodge(width = 2)) +
      coord_flip() +
      theme(axis.text.x = element_blank()) + 
      theme_classic(base_size = 16) 
    
    
  })
  
  sentiment_terms <- reactive({
    
    abstract_words <- abstract_terms()
    
    abstracts_sentiment <- abstract_words %>%
      inner_join(get_sentiments("bing"))
    
    abstracts_sentiment <- unique(abstracts_sentiment$word)
    abstracts_sentiment
    
  })
  
  observe({
    updateSelectInput(session, inputId = "sentim_txt", choices = sentiment_terms())
  })
  
  output$sent_word_plot <- renderPlot({
    
    abstract_words <- abstract_terms()
    
    abstracts_sentiment <- abstract_words %>%
      inner_join(get_sentiments("bing")) 
    
    abstracts_sentiment[abstracts_sentiment$sentiment == "negative",]$n <- abs(abstracts_sentiment[abstracts_sentiment$sentiment == "negative",]$n )
    
    plot_words<-abstracts_sentiment[abstracts_sentiment$word == input$sentim_txt,]
    
    pal <- c("positive" = "#000083",
             "negative" = "#860001")
    
    ggplot(plot_words, aes(as.factor(year), n, fill = sentiment)) +
      geom_col(show.legend = FALSE) + 
      geom_text(aes(label = n),  vjust = -0.5, size = 5, position = position_dodge(width = 2)) +
      ylab("frequency") + 
      scale_fill_manual( values = pal, limits = names(pal)) + 
      theme_classic(base_size = 16)
    
    
  })
  
  output$sliderRangeSentiment<-renderUI({
    
    df<-load_data()
    
    abstract_words <- abstract_terms()
    
    abstracts_sentiment <- abstract_words %>%
      inner_join(get_sentiments("bing")) 
    
    bing_word_counts <- abstracts_sentiment %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      ungroup()
    
    sliderInput("control_words_sent", label= "Frequency (interval):" ,  min = min(as.numeric(bing_word_counts$nn)), max = max(as.numeric(bing_word_counts$nn)), 
                value=c(floor(quantile(as.numeric(bing_word_counts$nn), probs = 0.9)), max(as.numeric(bing_word_counts$nn))), step = 1, sep = "")  
    
  })
  
  output$impact_words <- renderPlot({
    
    abstract_words <- abstract_terms()
    
    abstracts_sentiment <- abstract_words %>%
      inner_join(get_sentiments("bing")) 
    
    bing_word_counts <- abstracts_sentiment %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      ungroup()
    
    interval <- input$control_words_sent
    bing_word_counts<-bing_word_counts[bing_word_counts$nn %in% interval[1]:interval[2],]
    
    bing_word_counts %>%
      group_by(sentiment) %>%
      ungroup() %>%
      mutate(word = reorder(word, nn)) %>%
      ggplot(aes(word, nn, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = nn),  hjust = -0.5, size = 4, position = position_dodge(width = 1)) +
      facet_wrap(~sentiment, scales = "free_y") +
      coord_flip() +
      ylab(NULL) + 
      theme_minimal(base_size = 16)
    
  })
  
  output$wordcloud_sentiment_pos <- renderWordcloud2({
    
    abstract_words <- abstract_terms()
    
    abstracts_sentiment <- abstract_words %>%
      inner_join(get_sentiments("bing")) 
    
    extract_terms <- abstracts_sentiment %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "nn", fill = 0) 
    
    extract_terms_pos <- as.data.frame(extract_terms)
    extract_terms_pos[,1] <- rownames(extract_terms_pos)
    
    wordcloud2(extract_terms_pos, size = 0.5, color="blue", minRotation = -pi/4, maxRotation = -pi/4)
    
  })
  
  
  output$wordcloud_sentiment_neg <- renderWordcloud2({
    
    abstract_words <- abstract_terms()
    
    abstracts_sentiment <- abstract_words %>%
      inner_join(get_sentiments("bing")) 
    
    extract_terms <- abstracts_sentiment %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "nn", fill = 0) 
    
    extract_terms_neg <- as.data.frame(extract_terms)
    extract_terms_neg[,2] <- rownames(extract_terms_neg)

    extract_terms_neg <- data.frame(positive = extract_terms_neg[,2], negative = extract_terms_neg[,1])
    rownames(extract_terms_neg) <- extract_terms_neg[,1]
    
    wordcloud2(extract_terms_neg, size = 0.5, color="red", minRotation = -pi/4, maxRotation = -pi/4)
  })
  
  
  
}