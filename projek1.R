library(tm)
library(twitteR)
library(rtweet)

api_key <- "5y4xsHiQsnKBmTKvMcHQfwOrB"
api_secret <- "twVY7mBarigAXL6Z8uPi9wiFISnc0CcWCljgZ46ER2MYXPXvLh"
access_token<- "1263740609489022977-F5Wbn6FjMulZIYfhv4K6mW7lCROoVg"
access_token_secret<- "pe1qgqxiEg03eXboby6V9YzRyufZtIPUlhdeUFs91L8RY"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

covid = searchTwitter('vaksin covid-19',
                   lang="id",
                   n = 1000)
saveRDS(covid,file = '/home/andra/Documents/Dev/R/tweet-mentah.rds')


covid <- readRDS('/home/andra/Documents/Dev/R/tweet-mentah.rds')
d = twListToDF(covid)
tweetcovid <- d$text
tweetcovidc <- Corpus(VectorSource(tweetcovid))
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
twitclean <- tm_map(tweetcovidc, removeURL)
removeNL <- function(y) gsub("\n", " ", y)
twitclean <- tm_map(twitclean, removeNL)
replacecomma <- function(y) gsub(",", "", y)
twitclean <- tm_map(twitclean, replacecomma)
removeRT <- function(y) gsub("RT ", "", y)
twitclean <- tm_map(twitclean, removeRT)
removetitik2 <- function(y) gsub(":", "", y)
twitclean <- tm_map(twitclean, removetitik2)
removetitikkoma <- function(y) gsub(";", " ", y)
twitclean <- tm_map(twitclean, removetitikkoma)
removetitik3 <- function(y) gsub("p.", "", y)
twitclean <- tm_map(twitclean, removetitik3)
removeamp <- function(y) gsub("&amp;", "", y)
twitclean <- tm_map(twitclean, removeamp)
removeUN <- function(z) gsub("@\\w+", "", z)
twitclean <- tm_map(twitclean, removeUN)
remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
twitclean <- tm_map(twitclean,remove.all)
twitclean <- tm_map(twitclean, removePunctuation)
twitclean <- tm_map(twitclean, tolower)
myStopwords = readLines("stopwords.txt")
twitclean <- tm_map(twitclean,removeWords,myStopwords)

dataframe<-data.frame(text=unlist(sapply(twitclean, `[`)), stringsAsFactors=F)
View(dataframe)

write.csv(dataframe,file = 'tweetclean-tidy-fix.csv')
