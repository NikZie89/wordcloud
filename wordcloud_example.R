## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

## First specify the packages of interest
packages = c("tidyverse", "tidytext",
             "stopwords", "wordcloud")

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

###load the file

goals<-read.csv2("example.csv")

ncol(goals)

goals_long<-tidyr::gather(goals, key="goal", value="text", 1:ncol(goals))%>%          #transform the columns from long to wide
                    dplyr::mutate(doc_id=row_number())


stopwords_nl<-stopwords::stopwords(language = "nl")
stopwords_en<-stopwords::stopwords(language = "en")

#Create the bigrams
goal_tokens<-goals_long%>%
                tidytext::unnest_tokens(output = "bigrams", input=text, token="ngrams", n=2)%>%         #create the bigrams
                dplyr::filter(!is.na(bigrams))%>%                                                       #filter out the empty rows (no goal entered)
                tidyr::separate(bigrams, into = c("word_1", "word_2"))%>%                               #split uo the bigrams into two column, to remove the stopwords
                dplyr::filter(!(word_1 %in% stopwords_nl))%>%                                           #use the vector with stopwords to filter out the bigrams that inlclude any of the stopwords
                dplyr::filter(!(word_2 %in% stopwords_nl))%>%
                tidyr::unite(bigrams, word_1, word_2, sep=" ")

goal_tokens<-goal_tokens%>%dplyr::left_join(goals_long, by=c("doc_id", "goal"))                                #joining the original goals back to the bigram data frame. Makes it easy see where certain word combinations occured
goal_count<-goal_tokens%>%dplyr::count(bigrams, sort= TRUE)%>%dplyr::ungroup()                                        #counting the occurence of each bigram. Needed for the visualization.


# creating the tidy text word cloud ----------------------------------------------------
# see also https://richpauloo.github.io/2017-12-29-Using-tidytext-to-make-word-clouds/

pal <- RColorBrewer::brewer.pal(8,"Dark2")  #colors used

goal_count %>% 
  with(wordcloud(bigrams, n, random.order = FALSE, max.words = 50, colors=pal))                       #the word cloud


