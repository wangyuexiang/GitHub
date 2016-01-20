
Sys.setenv(http_proxy="proxy.asf.fr:8080")
Sys.getenv("http_proxy")


# Install and Activate Packages
# install.packages("twitteR", "RCurl", "RJSONIO", "stringr")
# install.packages("twitteR", "RCurl")
# library(RJSONIO)
# library(stringr)

library(twitteR)
library(RCurl) 
library(ROAuth)


consumer_key <- "L2R8B9hfR6rAUff06E8qeR4nD"
consumer_secret <- "66uRkthuxA75wrvmUDoSfaxVI1DGlDzIxWfiXOe88eDXRkTMVD"
access_token <- "149130360-DkwpFgyN06jsKx1yVj7zl5L72xhmsBz3e8zMyS5M"
access_token_secret <- "ZODTUdzXWJhylM5HY7L9UNsYDdM0fAnKW8esee0JVJ5Jv"
#rm(cred,access_token,access_token_secret,api_key,api_secret,consumer_key,consumer_secret)



# SSL Certificate
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

# API URLs
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "http://api.twitter.com/oauth/authorize"

# API Keys from https://apps.twitter.com/app/new 
apiKey <- "L2R8B9hfR6rAUff06E8qeR4nD"
apiSecret <- "66uRkthuxA75wrvmUDoSfaxVI1DGlDzIxWfiXOe88eDXRkTMVD"

# Connect to Twitter to get credentials

twitCred <- OAuthFactory$new(
  consumerKey=apiKey,
  consumerSecret=apiSecret,
  requestURL=reqURL,
  accessURL=accessURL,
  authURL=authURL)

# Twitter Handshake - you will need to get the PIN after this
twitCred$handshake()

#registerTwitterOAuth(twitCred)
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_token_secret)
save(list="twitCred", file="credentials")


query <- "kuwaiti,kuwait,#kuwait,#q8"
query <- unlist(strsplit(query,","))
tweets = list()

for(i in 1:length(query)){
  result<-searchTwitter(query[i],n=1500,geocode='29.3454657,47.9969453,80mi')
  tweets <- c(tweets,result)
  tweets <- unique(tweets)
}