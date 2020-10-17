# DanForPM analysis 

require(tweetbotornot2)
require(glue)
require(dplyr)
require(rtweet)

api_key <- "GOES HERE"
api_secret_key <- "GOES HERE"
access_token <- "GOES HERE"
access_token_secret <- "GOES HERE"

token <- create_token(
  app = "GOES HERE",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

######################################################
#### COLLECTION - tweets containing #danforpm
tweet_search_danforpm <- search_tweets('#danforpm', n = 18000, include_rts = TRUE, retryonratelimit = TRUE)
saveRDS(tweet_search_danforpm, paste0(Sys.time()," tweet_search_danforpm.rds"))
# length(unique(tweet_search_danforpm$screen_name))

# SAVE TO DISK
library(dplyr)
df_combined_danforpm <- tweet_search_danforpm %>% distinct(status_id, .keep_all = TRUE)
dim(df_combined_danforpm)
# subset only the columns we want to save to disk 
df_combined_danforpm_TO_DISK <- df_combined_danforpm[,c(1:6,14:16,48:62,63:66,82:83)]
write.csv(df_combined_danforpm_TO_DISK,paste0(Sys.time()," tweet_search_danforpm.csv"),row.names = F)
# write tweet IDs to disk
write.table(df_combined_danforpm$status_id,paste0(Sys.time(),"_danforpm_tweet_ids.csv"), row.names = F, col.names = F, sep=",")

# USER AND BOT ANALYSIS

userids_danforpm2 <- unique(df_combined_danforpm$user_id)
# collect timeline data (latest 500 tweets), to feed into the bot prediction model
# we have to use a custom function to avoid a curl error with rate limits
# from here: https://github.com/ropensci/rtweet/issues/266#issuecomment-471092678 
get_timeline_unlimited <- function(users, n){
  
  if (length(users) ==0){
    return(NULL)
  }
  
  rl <- rate_limit(query = "get_timeline")
  
  if (length(users) <= rl$remaining){
    print(glue("Getting data for {length(users)} users"))
    tweets <- get_timeline(users, n, check = FALSE)  
  }else{
    
    if (rl$remaining > 0){
      users_first <- users[1:rl$remaining]
      users_rest <- users[-(1:rl$remaining)]
      print(glue("Getting data for {length(users_first)} users"))
      tweets_first <- get_timeline(users_first, n, check = FALSE)
      rl <- rate_limit(query = "get_timeline")
    }else{
      tweets_first <- NULL
      users_rest <- users
    }
    wait <- rl$reset + 0.1
    print(glue("Waiting for {round(wait,2)} minutes"))
    Sys.sleep(wait * 60)
    
    tweets_rest <- get_timeline_unlimited(users_rest, n)  
    tweets <- bind_rows(tweets_first, tweets_rest)
  }
  return(tweets)
}

# just analyse the first 1000 most active users (by tweet frequency), due to rate limits 
userids_danforpm_TOP1000_most_active <- sort(table(df_combined_danforpm$user_id),decreasing = T)
userids_danforpm_TOP1000_most_active <- names(userids_danforpm_TOP1000_most_active[1:1000])

danforpm_user_timelines2 <- get_timeline_unlimited(userids_danforpm_TOP1000_most_active,n=200)
saveRDS(danforpm_user_timelines2,paste0(Sys.time(),"_danforpm_user_timelines.rds")) # save data to disk 

# run tweetbotornot2 predictions 
bot_results_danforpm2 <- predict_bot(danforpm_user_timelines2)
bot_results_danforpm2$screen_name[which(bot_results_danforpm2$prob_bot > 0.5)]
write.csv(bot_results_danforpm2,"bot_results_danforpm2.csv",row.names = F)

