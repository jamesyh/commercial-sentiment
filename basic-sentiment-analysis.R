# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("httr")
# install.packages("ROAuth")
# install.packages("stringr")
# install.packages("twitteR")


library(dplyr)
library(ggplot2)
library(httr)
library(ROAuth)
library(stringr)
library(twitteR)



setup_twitter_oauth(consumer_key = "sSp1bxIyZYxmMzFFnDmJ0Q8nw",
                    consumer_secret = "AIb5tekjRB3DTgbSnYXOSWDpBslI9AKwhDj2IFYw1ops2bnxWj",
                    access_token = "46759939-Hz9jFukVH7zWYs8DLcKLZoXD8KBTsNXcZo5OW8ZQh",
                    access_secret = "rAEAfj5FhQ1sef45L3T9J0p9kVUIrQfvjhzncrMxCWmkB")




searchString <- "#trump"
numberOfTweets <- 10000
tweets <- searchTwitter(searchString, n = numberOfTweets, lang="en")


tweetsDF <- twListToDF(tweets)

# Convert to dataframe, and encode to native
x <- tweetsDF
x$text <- enc2native(x$text)

# Extract URLs
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA
-F][0-9a-fA-F]))+"
x$contentURL <- str_extract(x$text, url_pattern)

# Clean content of text
x$text <- gsub("^[[:space:]]*","",x$text) # Remove leading whitespaces
x$text <- gsub("[[:space:]]*$","",x$text) # Remove trailing whitespaces
x$text <- gsub(" +"," ",x$text) # Remove extra whitespaces
x$text <- gsub("'", "%%", x$text) # Replacing apostrophes with %%
x$text <- iconv(x$text, "latin1", "ASCII", sub="") # Remove emojis/dodgy unicode
x$text <- gsub("<(.*)>", "", x$text) # Remove pesky Unicodes like <U+A>
x$text <- gsub("\\ \\. ", " ", x$text) # Replace orphaned fullstops with space
x$text <- gsub("  ", " ", x$text) # Replace double space with single space
x$text <- gsub("%%", "\'", x$text) # Changing %% back to apostrophes
x$text <- gsub("https(.*)*$", "", x$text) # remove tweet URL
x$text <- gsub("\\n", "-", x$text) # replace line breaks with -
x$text <- gsub("--", "-", x$text) # remove double - from double line breaks
x$text <- gsub("&amp;", "&", x$text) # fix ampersand &
x$text[x$text == " "] <- "<no text>"

for (i in 1:nrow(x)) {
  if (x$truncated[i] == TRUE) {
    x$text[i] <- gsub("[[:space:]]*$","...",x$text[i])
  }
}

# Remove unused columns
cleanTweets <- x %>% select("text", "contentURL", "favorited", "favoriteCount",
                            "created", "truncated", "id", "screenName",
                            "retweetCount", "isRetweet", "retweeted")
head(cleanTweets)

positiveWords = read.table("positive-words.txt",header = F , as.is = T)[,1]
negativeWords = read.table("negative-words.txt",header = F , as.is = T)[,1]

catch = NA
b = 1

for (j in 1:nrow(cleanTweets)) {
  theTweet <- tolower(cleanTweets$text[j])
  tweetWords <- str_split(theTweet, "\\s+")
  words <- unlist(tweetWords)
  posMatches <- words %in% positiveWords
  negMatches <- words %in% negativeWords
  score <- sum(posMatches) - sum(negMatches)
  cleanTweets$sentimentScore[j] <- score
  
  st = 1
  
  for(k in b:(b+length(words))){
    
    catch[k] = words[st]
    
    st = st + 1
    b = b + 1
    
  }
  
  
  
}

catch = unique(catch)

sum(catch %in% positiveWords)
sum(catch %in% negativeWords)


catch[catch %in% positiveWords]


plotData <- cleanTweets[c("text", "sentimentScore", "favoriteCount")]
xLabel <- paste("Sentiment Score.  Mean sentiment: ",
                round(mean(cleanTweets$sentimentScore), 2), sep = "")
yLabel <- paste("Number of Tweets (", nrow(cleanTweets),")", sep = "")
graphTitle <- paste("Twitter Sentiment Analysis of ", searchString, sep = "")

qplot(factor(sentimentScore), data=plotData,
      geom="bar",
      fill=factor(sentimentScore),
      xlab = xLabel,
      ylab = yLabel,
      main = graphTitle) +
  theme(legend.position="none")

