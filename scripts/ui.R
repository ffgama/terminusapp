library(ggplot2)
library(shinythemes)
# wordcloud v.0.2.0
library(wordcloud2)
library(shinyjs)
library(shinycssloaders)
library(networkD3)
library(shinyWidgets)


fluidPage(theme = shinytheme("sandstone"),
          
          useShinyjs(),
          
          list(tags$head(HTML('<link rel="icon", href="logo_terminus.png", 
                                   type="image/png" />'))),
          
          titlePanel(
            title="", windowTitle="Terminus Application"
          ),
          
          navbarPage(id="MainNav",
                     
                     title=div(img(src="logo_terminus.png", width="125", height="183"), style="margin-left:15px; position:relative; display:block; padding-left:10px;"),
                     
                     tabPanel("Worcloud", value="HomePage",
                              
                              div(id ="Sidebar", style = "margin-top:130px;", sidebarPanel(
                                
                                tabsetPanel(tabPanel("Home",
                                            
                                            tags$head(tags$style(".span12 {background-color: black;}"),
                                                      includeScript("www/d3.min.js")
                                            ),
                                            
                                            conditionalPanel(
                                              
                                              condition = "input.dtset_test == false",
                                              tags$hr(),
                                              
                                              textInput("text", label = h4("Enter Keyword(s):"), value = "cancer gastritis helicobacter gene"),
                                              numericInput("num_records", label = h4("Number (max) papers:"), value = 1500),
                                              helpText("Recommended max. 3000 papers (taking into account perfomance) "),
                                              actionButton(style="background-color:#0085D9;","wordBtn","GENERATE CLOUD"),
                                              tags$hr(),
                                              tags$h5(style = "font-weight:bold;", "Query searching"),
                                              uiOutput("show_query")
                                              
                                            ),
                                            
                                            tags$hr(),
                                            
                                            uiOutput("slider"),
                                            
                                            tags$hr(style="border:1px dashed #ccc;"),
                                            
                                            selectInput('words', 'Remove words',choices = "", selected = NULL, multiple = TRUE, selectize = TRUE),
                                            selectInput('combining_words', 'Joining words', choices = "", selected = NULL, multiple = TRUE, selectize = TRUE),
                                            
                                            tags$hr(),
                                            
                                            tags$h4("", checkboxInput("dtset_test", "Running Test Data", value = FALSE))
                                            
                       ),
                       
                       tabPanel("Summary",
                                fluidRow(column(6, align="center",  verbatimTextOutput("summary")))
                       ), 
                       
                       tabPanel("About Test Data",
                                fluidRow(column(12, align="left", 
                                                tags$hr(),
                                                h5("This data set contains summaries that considering applicability about resistance training on elderly over time."),
                                                
                                                tags$hr(),
                                                
                                                tags$strong(tags$h4("Query based: ")),tags$h5(style="color:#0085D9;", "(physiotherapy[AB]) 
                                                                                            AND (resistance training[AB] OR weathered 
                                                                                            training[AB]) AND (old-aged[AB] OR old man[AB] 
                                                                                            OR aged[AB] OR elderly[AB])")
                                )))
                       
                        ))
                       
                      ),     
                     
                      mainPanel(
                        withSpinner(wordcloud2Output("word_cloud"), color = "#0dc5c1"), tags$hr(), tabPanel("Dataset", 
                                                                                                            mainPanel(width="100%",
                                                                                                                      fluidRow(column(12, align="center", uiOutput("downloadPlotCloud"))),
                                                                                                                      fluidRow(column(12, DT::dataTableOutput("dataframe"))),
                                                                                                                      fluidRow(column(12, align="center", uiOutput("downloadDf")))),
                                                                                                            tags$hr())
                      )
                     
                     ),
                     
                     tabPanel("Descriptive Analysis",
                              div(style="margin-top:130px;",
                                  div(class="GeneralContent",
                                      tags$div(class="span7",
                                               mainPanel(
                                                 fluidRow(column(6, align="left",
                                                                 uiOutput("flare", height="1000px")), column(6, align="left", 
                                                                                                             plotOutput("paper_year", height = "1000px")))
                                               )
                                      )
                                  )
                              )
                     ),
                     
                     
                     tabPanel("Word Frequency Analysis",
                              div(style="margin-top:130px;",
                                  div(class="GeneralContent",
                                      
                                      tags$hr(),
                                      
                                      h3("WORDS FREQUENCY OVER TIME"),
                                      selectInput('select_words', 'Please select words:', choices = "", selected = NULL, multiple = TRUE, selectize = TRUE),
                                      
                                      tags$div(class="span7",
                                               mainPanel(
                                                 
                                                 fluidRow(column(12, align="center",
                                                                 withSpinner(plotOutput("word_freq", height = "780px")))),
                                                 
                                                 h3("INTERESTS DEGREE BY FREQUENCY CUMULATIVE"),
                                                 
                                                 tags$hr(),
                                                 sidebarPanel(
                                                   fluidRow(
                                                     column(6, align="left",
                                                            conditionalPanel(
                                                              condition = "input.opt_dif_occ == 'difference'",
                                                              withSpinner(uiOutput("control_min_max_freq")),
                                                              tags$hr(style="border:1px dashed #ccc;")
                                                            ),
                                                            
                                                            radioButtons("disp", "Order By",
                                                                         choices = c(Decreasing = "decreasing",
                                                                                     Ascending = "ascending"),
                                                                         selected = "decreasing"),
                                                            
                                                            radioButtons("opt_dif_occ", "Show By",
                                                                         choices = c(Difference = "difference",
                                                                                     Occurence = "occurence"),
                                                                         selected = "difference")
                                                     ),
                                                     
                                                     conditionalPanel(
                                                       condition = "input.opt_dif_occ == 'occurence'",
                                                       column(6, align="left",
                                                              uiOutput("control_stacks"),     
                                                              tags$hr(style="border:1px dashed #ccc;")
                                                       )
                                                       
                                                     )
                                                     
                                                   ),
                                                   uiOutput("sliderControlYear")
                                                 ),
                                                 fluidRow(column(12, align="center",
                                                                 withSpinner(plotOutput("interest_words", height = "1000px"))))
                                               ) 
                                      ) 
                                  )
                              ) 
                     ),
                     
                     tabPanel("Words Importance",
                              
                              div(style="margin-top:130px;",
                                  
                                  h3("WORDS IMPORTANCE"),
                                  
                                  div(class="GeneralContent",
                                      
                                      uiOutput("controlTermsTfIdf"),
                                      uiOutput("sliderYear"),
                                      
                                      tags$div(class="span7",
                                               mainPanel(
                                                 fluidRow(column(12, align="center",
                                                                 withSpinner(plotOutput("tf_idf", height = "1000px")))),
                                                 
                                                 tags$hr(),
                                                 
                                                 h3("WEIGHTING BIGRAMS WITH TF-IDF"),
                                                 
                                                 fluidRow(column(4, align="left", withSpinner(uiOutput("control_bigram_tf_idf")))),
                                                 
                                                 fluidRow(column(12, align="center",
                                                                 withSpinner(plotOutput("bi_gram_tf_idf", height = "1500px")))),
                                                 
                                                 
                                                 tags$hr(),
                                                 
                                                 h3("BIGRAM ANALYSIS (FREQUENCY X TF-IDF)"),
                                                 
                                                 tags$hr(),
                                                 
                                                 fluidRow(column(4,align="left",
                                                                 selectInput('bigram_txt', 'Select Terms:', c(Choose='', ''), selectize=FALSE)
                                                 )),
                                                 
                                                 uiOutput("sliderBigram"),
                                                 
                                                 
                                                 fluidRow(column(6, mainPanel(width="100%", align="left",
                                                                              withSpinner(DT::dataTableOutput("bigramtxt_after")))),
                                                          column(6, mainPanel(width="100%", align="left",
                                                                              withSpinner(DT::dataTableOutput("bigramtxt_before"))))
                                                  )
                                               ) 
                                          )
                                  ) 
                              )),
                     
                     tabPanel("Network Representation",
                              div(style="margin-top:130px;",
                                  
                                  div(class="GeneralContent",
                                      tags$div(class="span7",
                                               
                                               mainPanel(
                                                 h3("NETWORK REPRESENTATION"),
                                                 fluidRow(column(4, align="left", uiOutput("control_network"))),
                                                 fluidRow(column(12, align="center",
                                                                 withSpinner(forceNetworkOutput("network_bigram", height = "1000px"))))
                                               )
                                      )
                                  )
                              ) 
                     ),
                     
                     tabPanel("Words Correlation",
                              div(style="margin-top:130px;",
                                  
                                  div(class="GeneralContent",
                                      tags$div(class="span7",
                                               mainPanel(
                                                 
                                                 h3("CORRELATION ANALYSIS"),
                                                 
                                                 tags$hr(),
                                                 
                                                 tags$hr(),
                                                 
                                                 sliderInput(inputId = "bins_corr",
                                                             label = "Number of terms:",
                                                             min = 1,
                                                             max = 50,
                                                             value = 30),
                                                 
                                                 uiOutput("sliderYearCorr"),
                                                 
                                                 selectInput('corr_txt', 'Select Terms:', c(Choose='', ''), selectize=FALSE),
                                                 
                                                 fluidRow(column(12, mainPanel(width="100%", align="center",
                                                                               withSpinner(plotOutput("corr_plot", height = "800px"))))
                                                 )
                                                 
                                                 
                                               ) 
                                      ) 
                                  )
                              ) 
                     ),
            
                     tabPanel("Sentiment Analysis",
                              div(style="margin-top:130px;",
                                  div(class="GeneralContent",
                                      tags$div(class="span7",
                                               mainPanel(
                                                 
                                                 h3("WORD SENTIMENT DEGREE (ACCUMULATED)"),
                                                 
                                                 tags$hr(),
                                                 
                                                 sliderInput(inputId = "bins_sentiment",
                                                             label = "Number of terms:",
                                                             min = 1,
                                                             max = 80,
                                                             value = 30),
                                                 
                                                 fluidRow(column(12, mainPanel(width="100%", align="center",
                                                                               withSpinner(plotOutput("sentiment_plot", height = "1000px"))))
                                                 ),
                                                 
                                                 tags$hr(),

                                                 h3("SENTIMENT DEGREE EVOLUTION BY WORD"),

                                                 tags$hr(),

                                                 selectInput('sentim_txt', 'Select Terms:', choices = "", selected = NULL,  selectize = TRUE),

                                                 fluidRow(column(12, mainPanel(width="100%", align="center",
                                                                               withSpinner(plotOutput("sent_word_plot", height = "800px"))))
                                                 ),

                                                 tags$hr(),

                                                 h3("POSITIVE AND NEGATIVE WORDS (MOST COMMON)"),

                                                 uiOutput("sliderRangeSentiment"),

                                                 tags$hr(),

                                                 fluidRow(column(12, mainPanel(width="100%", align="center",
                                                                               withSpinner(plotOutput("impact_words", height = "1000px"))))
                                                 ),

                                                 tags$hr(),

                                                 h3("WORDCLOUD REPRESENTATION: POSITIVE X NEGATIVE WORDS"),

                                                 tags$hr(),

                                                 fluidRow(column(6, mainPanel(width="50%", align="center",
                                                                              withSpinner(wordcloud2Output("wordcloud_sentiment_pos", height = "600px")))
                                                 ),
                                                 column(6, mainPanel(width="50%", align="center",
                                                                     withSpinner(wordcloud2Output("wordcloud_sentiment_neg", height = "600px"))))
                                                 )
                                                 
                                               ) 
                                      )
                                  )
                              )
                     ),
            
                     tabPanel("About Terminus",
                              div(style="margin-top:150px;",
                              div(class="GeneralContent",
                              includeHTML("www/about.html")
                              )
                      )
                  )
          ) 
)