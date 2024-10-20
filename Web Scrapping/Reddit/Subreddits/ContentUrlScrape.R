library(readr)
subreddits <- read_csv('workable_reddit_df.csv')
subreddit <- subreddits$subreddit 

library(RedditExtractoR)

# Types of EV are:
# BEV: Battery Electric Vehicle,
# PHEV: Plug in Hybrid Electric Vehicle
# HEV: Hybrid Electric Vehicle

keywords <- c('BEV',
              'PHEV',
              'HEV',
              'Battery Electric Vehicle',
              'Plug in Hybrid Electric Vehicle',
              'Hybrid Electric Vehicle',
              'environment',
              'battery',
              'fuel',
              'gas',
              'sustainability',
              'sustainable',
              'pollution')

reddit_url <- function(x, keyword = keywords){
  ls <- list()
  n <- length(keyword)
  for (i in 1:n) {
    df <- data.frame(find_thread_urls(keyword[i], 
                                      sort_by = 'top',
                                      subreddit = x,
                                      period = 'all'))
    ls[[i]] <- df
  }

  Sys.sleep(5)
  return(ls)
}

teslamotors <- reddit_url(subreddit[1])
library(jsonlite)
write_json(teslamotors, "Teslamotors_URL.json", pretty = TRUE)
saveRDS(teslamotors, "Teslamotors_url.rds")

fuckcars <- reddit_url(subreddit[2])
write_json(fuckcars, "fuckcars_URL.json", pretty = TRUE)
saveRDS(fuckcars, "fuckcars_url.rds")

electricvehicles <- reddit_url(subreddit[3])
write_json(electricvehicles, "electricvehicles.json", pretty = TRUE)
saveRDS(electricvehicles, "electricvehicles.rds")

teslamodel3 <- reddit_url(subreddit[4])
write_json(teslamodel3, "teslamodel3.json", pretty = TRUE)
saveRDS(teslamodel3, "teslamodel3.rds")

rivian <- reddit_url(subreddit[5])
write_json(electricvehicles, "electricvehicles.json", pretty = TRUE)
saveRDS(electricvehicles, "electricvehicles.rds")
