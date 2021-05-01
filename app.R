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


#color_pallet<-row.names(brewer.pal.info%>%filter(colorblind==TRUE))


#####App begins here -------------------------------------------------------
#####Define UI for application (Front end) ---------------------------------


ui <- fluidPage(

    # Application title
    titlePanel("Simple wordcloud generator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
    
            
        fileInput("file", "Choose the CSV File with the goals", accept = ".csv"),                      #to upload the file
        
        checkboxInput("header", "Uncheck if the file has no column headers", TRUE),                    #to select if file has a header     
        
        sliderInput(inputId = "min_freq",                                                              #for filtering bigrams with low occurrence
                    label = "Filter word combinations that exist less than n times", 
                    min = 1  ,max=50, value = 10, step = 1),
        
        sliderInput(inputId = "word_size",                                                             #word size
                    label = "Word size", 
                    min = 0.5  ,max=2, value = 0.9, step = 0.1),
        
        radioButtons("lang", "Language (for removing stop words)", selected = "nl",                    #language of the file for removing stopwords
                     choices=c("Dutch"="nl",
                               "English"="en")),
        
        radioButtons("delimiter", "Select the column delimiter for the csv file", selected = ";",      #delimiter
                     choices=c("semicolon"=";",
                               "comma"=",")),
        
        selectInput("color", "Select a color palette",                                                 #for selecting the color
                    choices = c("Set2", "Paired", "Dark2", "Greens", "Blues", "Reds", "GnBu"), 
                    selected="Dark2")
        ),

    mainPanel(
        tabsetPanel(
            tabPanel("Table", div(DT::dataTableOutput("goal_table"), style = "font-size:80%")),
            tabPanel("Word cloud", wordcloud2Output("wordcloud", width = "100%", height=800)),
            tabPanel("Bigram count", div(DT::dataTableOutput("bigram_count"), style = "font-size:120%")),
            tabPanel("Bigram occurrence", div(DT::dataTableOutput("bigram_occ"), style = "font-size:100%"))
                    )
            )
        )
    )
    



####Define the server side of the app (Back end) -------------------------


server <- function(input, output) {
    
    file <- reactive({input$file})                          #file as reactive, so I refer to it inside the reactive environments
    
    colorlist<-list(Dark2=brewer.pal(8, "Dark2"),
                     Paired=brewer.pal(12, "Paired"),
                     Set2=brewer.pal(8, "Set2"),
                     Greens=colorRampPalette(brewer.pal(9, "Greens")[3:9])(20),                  #some custom color options
                     Blues=colorRampPalette(brewer.pal(9, "Blues")[3:9])(20),
                     Reds=colorRampPalette(brewer.pal(9, "Reds")[3:9])(20),
                     GnBu=colorRampPalette(brewer.pal(9, "YlGnBu")[3:9])(20))

    

    
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
    goal_count<-goal_tokens%>%dplyr::count(bigrams, sort= TRUE)%>%dplyr::ungroup()                                 #counting the occurrence of each bigram. Needed for the visualization.

    output$bigram_count <- DT::renderDataTable({goal_count%>%arrange(desc(n))})
    output$bigram_occ <- DT::renderDataTable({datatable(goal_tokens, filter="top", selection="multiple", escape=FALSE, options = list(dom = 'ltipr'))})
    
    # creating the tidy text word cloud ----------------------------------------------------


    set.seed(0411) #doesn't seem to be supported with wordcloud2
    goal_count %>% dplyr::rename(freq=n, words=bigrams)%>%
        dplyr::filter(freq>input$min_freq)%>%
        wordcloud2(color=colorlist[[input$color]], size = input$word_size, widgetsize =c("1000","1000"))
                                })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
