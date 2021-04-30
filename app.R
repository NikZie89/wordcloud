#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Loading and installing required packages --------------------------------

## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

## First specify the packages of interest
packages = c("shiny", "DT", "tidyverse", "tidytext",
             "stopwords", "tools", "wordcloud2")

## Now load or install&load all
package.check <- lapply(
    packages,
    FUN = function(x) {
        if (!require(x, character.only = TRUE)) {
            install.packages(x, dependencies = TRUE)
            library(x, character.only = TRUE)
        }
    }
)





#####App begins here -------------------------------------------------------
#####Define UI for application (Front end) ---------------------------------


ui <- fluidPage(

    # Application title
    titlePanel("Simple wordcloud generator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
    
            
        fileInput("file", "Choose the CSV File with the goals", accept = ".csv"),
        
        checkboxInput("header", "Uncheck if the file has no column headers", TRUE),
        
        sliderInput(inputId = "min_freq", label = "Filter word combinations that exist less than n times", min = 1  ,max=50, value = 2, step = 1),
        
        sliderInput(inputId = "word_size", label = "Word size", min = 0.5  ,max=2, value = 1, step = 0.2),
        
        radioButtons("lang", "Language (for removing stop words)", selected = "nl",
                     choices=c("Dutch"="nl",
                               "English"="en")),
        
        radioButtons("delimiter", "Select the column delimiter for the csv file", selected = ";",
                     choices=c("semicolon"=";",
                               "comma"=","))
        ),

    mainPanel(
        tabsetPanel(
            tabPanel("Table", div(DT::dataTableOutput("goal_table"), style = "font-size:80%")),
            tabPanel("Word cloud", wordcloud2Output("wordcloud", width = "100%", height=800))
                    )
            )
        )
    )
    



####Define the server side of the app (Back end) -------------------------


server <- function(input, output) {
    
    file <- reactive({input$file})                          #file as reactive, so I refer to it inside the reactive environments
    
# Creates the preview table of the uploaded data set ----------------------------------

    output$goal_table <- DT::renderDataTable({
        
        #file <- input$file
         ext <- tools::file_ext(file()$datapath)
        
         req(file())
         validate(need(ext == "csv", "Please upload a csv file"))
        # 
        # #browser()
         goals<-read.csv(file()$datapath, header = input$header, sep=";")
         DT::datatable(goals, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
    })

# Creates a plot for word cloud -----------------------------------
    
        
    output$wordcloud <-renderWordcloud2({
        goals<-read.csv(file()$datapath, header = input$header, sep=input$delimiter)
        ncol(goals)

    goals_long<-tidyr::gather(goals, key="goal", value="text", 1:ncol(goals))%>%          #transform the columns from long to wide
        dplyr::mutate(doc_id=row_number())


    stopwords<-stopwords::stopwords(language = input$lang)


    #Create the bigrams
    goal_tokens<-goals_long%>%
        tidytext::unnest_tokens(output = "bigrams", input=text, token="ngrams", n=2)%>%         #create the bigrams
        dplyr::filter(!is.na(bigrams))%>%                                                       #filter out the empty rows (no goal entered)
        tidyr::separate(bigrams, into = c("word_1", "word_2"))%>%                               #split uo the bigrams into two column, to remove the stopwords
        dplyr::filter(!(word_1 %in% stopwords))%>%                                           #use the vector with stopwords to filter out the bigrams that inlclude any of the stopwords
        dplyr::filter(!(word_2 %in% stopwords))%>%
        tidyr::unite(bigrams, word_1, word_2, sep=" ")

    goal_tokens<-goal_tokens%>%dplyr::left_join(goals_long, by=c("doc_id", "goal"))                                #joining the original goals back to the bigram data frame. Makes it easy see where certain word combinations occured
    goal_count<-goal_tokens%>%dplyr::count(bigrams, sort= TRUE)%>%dplyr::ungroup()                                        #counting the occurence of each bigram. Needed for the visualization.


    # creating the tidy text word cloud ----------------------------------------------------
    # see also https://richpauloo.github.io/2017-12-29-Using-tidytext-to-make-word-clouds/

    pal <- RColorBrewer::brewer.pal(8,"Dark2")  #colors used

    goal_count %>% dplyr::rename(freq=n, words=bigrams)%>%
        dplyr::filter(freq>input$min_freq)%>%
        wordcloud2(color=pal, size = input$word_size, widgetsize =c("1000","1000"))                       #the word cloud
#random.order = FALSE, max.words = 50,
                                })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
