# Import the relevant library
library(twitteR)

# Set working directory
setwd("~/R/win-library/Business Information Management/Bonus Assignment Twitter")

# Assign Twitter API - keys
ck <- "xxxx"
cs <- "xxx"
at <- "xx"
as <- "x"

setup_twitter_oauth(ck, cs, access_token = at, access_secret = as)

t_stream <- searchTwitter('education', resultType="recent", n=500)

df <- do.call("rbind", lapply(t_stream, as.data.frame))

# lapply applies a function to every element of a list and returns a list as result - it transforms the
# structure into a list of data.frames (one for each tweet)
# do.call executs a function on an argument (the new tweet-list). The function is here rbind(), i.e.the elements
# are connected row-wise, creating a single data frame in the end

# we only need these columns for our analysis

my_columns <- subset(df, select=c("text","created","screenName","retweetCount","isRetweet","id")) 

# before we can use the data frame in Access we need to delete all quotation marks from the text field

my_columns[,1] <- gsub('"',"",my_columns[,1])

# This saves the tweets as a csv-file that can be imported into Access.

write.table(my_columns, "tweets.csv", row.names = FALSE, col.names = TRUE, sep =";")

# R Task 1 Begin (Create list that has the number of characters in each tweet as elements (research function "nchar"), 
# then add the list as a column to the data frame and save the data frame to a new csv file)

my_columns <- read.csv("tweets.csv", sep=";")

my_columns["Characters"] <- "N/A"     # Create a new column with text "N/A" in all cells

is.character(my_columns[,1])          # If true, can run the nchar to count the characters

my_columns$Characters <- nchar(my_columns[,1], type = "chars", allowNA = FALSE, keepNA = NA)

write.table(my_columns, "R Task 1_Character_Count.csv", row.names = FALSE, col.names = TRUE, sep =";")

# R Task 1 End

# R Task 2 Begin (Create a data frame that only contains those tweets with an even ID using a FOR-loop and IF-ELSE
# constructs. Save the data frame as a new csv file)

Tweets_with_EvenID <- subset(df, select=c("text","created","screenName","retweetCount","isRetweet","id"))
Tweets_with_EvenID["EvenID"] <- "N/A"       # Create a new column with text "N/A" in all cells

i <- 1
for (i in 1:500) {
CheckEven <- as.integer(substr(my_columns[i,6],18,18)) %% 2 == 0
  if(CheckEven == TRUE) {
    Tweets_with_EvenID[i,7] = 1 #create Dummy variable
    i <- i + 1
  } else {
    Tweets_with_EvenID[i,7] = 0
    i <- i + 1
  }
} 

OnlyEven <- subset(Tweets_with_EvenID,Tweets_with_EvenID$EvenID==1) # Create a new subset filtering the Even dummies

write.table(OnlyEven, "R Task 2_EvenID.csv", row.names = FALSE, col.names = TRUE, sep =";") # Create a .csv

# as.integer(substr(my_columns[1,6],18,18))
# Hint (see video 2)

# R Task 2 End

# R Task 3 Begin (Create a word cloud of the contents of the tweets, save the word cloud as a png file)

install.packages(("SnowballC"),repos="http://cran.r-project.org")

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

WCloud <- read.csv("tweets.csv", sep=";")

WCloudCorpus <- Corpus(VectorSource(WCloud$text)) # create a corpus from the tweets

WCloudCorpus <- tm_map(WCloudCorpus, stemDocument) # stem words to basic form

# create document term matrix applying some transformations
tdm = TermDocumentMatrix(WCloudCorpus,control = list(removePunctuation = TRUE, stopwords = c("https","educat","educ", stopwords("english")), removeNumbers = TRUE, tolower = TRUE)) 

m = as.matrix(tdm) # define tdm as matrix
word_freqs = sort(rowSums(m), decreasing=TRUE) # get word counts in decreasing order
dm = data.frame(word=names(word_freqs), freq=word_freqs) # create a data frame with words and their frequencies

wordcloud(dm$word, dm$freq, random.order=FALSE, max.words = 100, colors=brewer.pal(8, "Dark2"))  # plot wordcloud

# Save the image in png format

png("R Task 3_EducationCloud.png", width=12, height=8, units="in", res=300) 
wordcloud(dm$word, dm$freq, random.order=FALSE, max.words = 100, colors=brewer.pal(8, "Dark2")) 
dev.off() 

# R Task 3 End
