# wordcloud
A ShinyR app that lets the user upload a csv file with multiple text columns to create a word cloud of bigrams.

Initially built for my work at the Erasmus Centre for Study and Career Success (Rotterdam School of Management), to facilitate the creation of work clouds based on student text data of their personal goals. For most institutions between 600-1200 students defined 6-9 personal or academic goals.
The app returns a word cloud with the most frequent two word combinations (bigrams).

However, the app can theoretically create this wordcloud for any text data.


At the moment the app takes a csv file (comma or semicolon delimited) with or without headers and any amount text data/character columns.

The minimal frequency for bigrams to be included, and the font size  of the cloud can be adjusted.
Stopwords can be filtered for Dutch or English (using the package stopwords).

