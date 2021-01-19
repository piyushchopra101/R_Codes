#####################
# these steps have to be done prior to running the entire script.
# this establishes a connection witht he twitter api.
#
# requestURL <- "https://api.twitter.com/oauth/request_token"
# accessURL <- "https://api.twitter.com/oauth/access_token"
# authURL <- "https://api.twitter.com/oauth/authorize"
# # The string within the quotation marks has to be replaced with the actual
# # consumerKey and consumerSecret.
# 
# consumerKey <- "s511LKRehoTSrHMsnj1iDOWj9"
# consumerSecret <- "Zfjx3xdaSYmrTW1CLWlWKRkrCfjdfERPDWokdZSxjjn2c6qbgu"
# # The next two lines establish a connection to the Twitter API.
# # The system will print a URL which should be copied in a browser to receive a PIN number.
# # This PIN has to be entered in the R-console.
# 
# download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")
# 
# my_oauth <- OAuthFactory$new(consumerKey = consumerKey, 
#                              consumerSecret = consumerSecret, 
#                              requestURL = requestURL, 
#                              accessURL = accessURL, 
#                              authURL = authURL)
# 
# my_oauth$handshake(cainfo="cacert.pem")
# 9527743
# my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
# 
#  
# setup_twitter_oauth(consumerKey,consumerSecret)
# # Once the connection is established we can save it so that we do not have
# # repeat this process.
# save(my_oauth, file = "my_oauth.Rdata")

##############################################################
library(streamR)
library(sp)
library(maps)
library(mapproj)
library(ggplot2)
library(scales)
library(grid)
library(ROAuth)
library(dplyr)
library(maps)
library(maptools)

dat_all=NULL

mytweet<-function(){
  states = map('state', fill=TRUE, col='transparent', plot = F)
  ids = sapply(strsplit(states$names, ':'), function(x) x[1])
  states_sp = map2SpatialPolygons(states, IDs = ids,
                                  proj4string = CRS('+proj=longlat +datum=WGS84'))
  load(file = 'my_oauth.RData')
  load(file = 'pos_words.RData')
  load(file = 'neg_words.RData')
  
  states_map = map_data('state')
  names(states_map)[5] = 'state'
  
  #function to get states from longitude and latitude
  get_states = function(lon, lat) {
    coords = data.frame(cbind(lon, lat))
    points_sp = SpatialPoints(coords)
    proj4string(points_sp) = proj4string(states_sp)
    i = over(points_sp, states_sp)
    names = sapply(states_sp@polygons, function(x) x@ID)
    return (names[i])
  }
  
  #function to calculate sentiment score
  get_sentiment = function(txt) {
    words = strsplit(txt, ' +')
    words = unlist(words)
    pos_matches = match(words, pos)
    neg_matches = match(words, neg)
    
    #count all the mapped positive and negative words and subtract the latter from the former
    score = sum(!is.na(pos_matches)) - sum(!is.na(neg_matches))
    return(score)
  }
  
  #function to get live tweets
  get_tweets = function() {
    #gather all geo-coded u.s. tweets for 5 seconds
    tStart <- Sys.time()
    while((as.integer(as.POSIXct(Sys.time()))-as.integer(as.POSIXct(tStart)))<6){
      tweets = filterStream(file.name = '',track ="rio", language = 'en',timeout = 5, locations=c(-122.75,26.8,-60.75,48.995),oauth = my_oauth)
    }
    #check if at least 30 tweets were received (sometimes a lower number errors out)
    if (length(tweets) >= 30) {
      tweets = parseTweets(tweets)
      tweets = subset(tweets, country == 'United States' & !is.na(place_lon) & !is.na(place_lat))
      
      #get states
      #first try place_lon and place_lat
      s1 = get_states(tweets$place_lon, tweets$place_lat)
      
      #then try lon and lat
      #replace na lon and lat to 0 so that the program won't break
      #0 (lon, lat) pair will return na in the end
      lon = ifelse(is.na(tweets$lon), 0, tweets$lon)
      lat = ifelse(is.na(tweets$lat), 0, tweets$lat)
      s2 = get_states(lon, lat)
      
      #combine
      tweets$state = ifelse(is.na(s1), s2, s1)
      tweets = subset(tweets, !is.na(state))
      
      #clean tweets
      text = gsub('[^[:graph:]]', ' ', as.character(tweets$text))
      text = gsub('^ +', '', text)
      text = gsub(' +$', '', text)
      text = gsub(' +', ' ', text)
      tweets$text = text
      
      #estimate sentiment of the tweets
      text = gsub('[^[:alpha:]]', ' ', text)
      text = tolower(text)
      
      tweets$sentiment = sapply(text, get_sentiment)

      return(tweets) 
    }
    else {
      return(data.frame(text = character(), state = character(), sentiment = numeric()))
    }
  }
  
  #function to aggregate sentiment scores at the state level
  agg_sentiment = function(x) {
    #taking into account the magnitude of the positivity and negativity
    pos_sum = sum(x[which(x > 0)])
    abs_sum = sum(abs(x))
    return(pos_sum / abs_sum)
  }
  
  #function to plot the sentiment on a heat map
  plot_map = function(dat, title) {
    #calculate % of positive sentiment per state
    sentiment_prop = dat %>%
      group_by(state) %>%
      summarise(sentiment_prc = agg_sentiment(sentiment))
    
    #merge with map data
    sent_map = left_join(states_map, sentiment_prop, by = 'state')
    sent_map = arrange(sent_map, order)
    
    #replace na with .5 (i.e., neutral states)
    sent_map$sentiment_prc[is.na(sent_map$sentiment_prc)] = .5
    
    #plot sentiment on map
      map_plot = 
        ggplot(sent_map, aes(x = long, y = lat, group = group, fill = sentiment_prc)) +
        geom_polygon() + coord_map('polyconic') +
        scale_fill_gradientn(colours = c("cyan", "black", "red"),
                             values = rescale(c(1, 0.25,0.5, 0.75,1)))+labs(fill = '') + 
        ggtitle(title) +
        theme(legend.position = 'bottom', plot.margin = unit(c(0, 0, 0, 0), 'mm'),
              panel.grid = element_blank(), panel.border = element_blank(),
              axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
              panel.background = element_blank(), plot.background = element_blank(),
              legend.background = element_blank())
    #ggsave(map_plot,filename=paste("myplot",length(dat),".png",sep=""))
    return(map_plot)
      
  } 
  get_input = get_tweets()
  dat=get_input
  dat_all = rbind(dat_all, dat)
  save(dat_all,file = "mydata.csv")
  out<-plot_map(dat, 'Sentiment in the last 5 seconds')
  #plot sentiment on map
  x11()
  print(out)
  out2<-plot_map(dat_all, 'Cumulative Sentiment in the last 5 seconds')
  x11()
  print(out2)
  
  #let the system sleep for 10 seconds to cause a pause
  #for running the funtion
  Sys.sleep(10)
}  


a=proc.time()
while ((proc.time()-a)< 61){
  mytweet()
}











