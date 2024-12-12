library(readr)
library(RedditExtractoR)
library(tidytext)
library(dplyr)
library(stringr)
library(progress)
electric_vehicles_sentiment <- read_csv("electric_vehicles_sentiment.csv")
fuckcars_sentiment <- read_csv("fuckcars_sentiment.csv")
rivian_sentiment <- read_csv("rivian_sentiment.csv")
teslamodel3_sentiment <- read_csv("teslamodel3_sentiment.csv")
teslamotors_sentiment <- read_csv("teslamotors_sentiment.csv")

clean_text_df <- function(x) {
  tk <- tibble(line = 1:length(x), text = x) 
  tk <- tk %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words, by = "word")
  cleaned_text <- tolower(str_c(tk$word, collapse = ' '))
  return(cleaned_text)
}

comment_scrape_df <- function(df) {   
  output_df <- data.frame()
  pb <- progress_bar$new(
    format = "  Scraping [:bar] :percent | Iteration :current/:total | Elapsed: :elapsedfull | ETA: :eta",
    total = nrow(df),
    clear = FALSE,
    width = 90
  )
  for (i in 1:nrow(df)) {     
    c <- get_thread_content(df$url[i])          
    if (!is.null(c$threads) && length(c$threads$url) > 0) {              
      if (is.null(c$comments) || length(c$comments$comment) == 0) {         
        scrapped_df <- data.frame(           
          post_url = c$threads$url,           
          post_upvote_score = c$threads$score,           
          post_comment = NA,           
          comment_upvote_score = NA,           
          comment_date = NA         
        )       
      } else {         
        scrapped_df <- data.frame(           
          post_url = c$threads$url,           
          post_upvote_score = c$threads$score,           
          post_comment = c$comments$comment,           
          comment_upvote_score = c$comments$score,           
          comment_date = c$comments$date         
        )       
      }
      scrapped_df <- scrapped_df %>%         
        rowwise() %>%         
        mutate(cleaned_comment = ifelse(is.na(post_comment), NA, clean_text_df(post_comment))) %>%         
        select(-post_comment) %>%         
        ungroup()
      output_df <- rbind(output_df, scrapped_df)            
    } else {
      cat("Warning: Missing thread data for URL:", df$url[i], "\n")     
    }
    Sys.sleep(5)
    pb$tick(tokens = list(current = i))  
  }
  return(output_df)
}

teslamotors_comment_scrapped <- comment_scrape_df(teslamotors_sentiment)
write_csv(teslamotors_comment_scrapped, 'teslamotors_comment.csv')

fuckcars_comment_scrapped <- comment_scrape_df(fuckcars_sentiment)
write_csv(fuckcars_comment_scrapped, 'fuckcars_comments.csv')

electric_vehicles_comment_scrapped <- comment_scrape_df(electric_vehicles_sentiment)
write_csv(electric_vehicles_comment_scrapped, 'electric_vehicles_comment.csv')

rivian_comment_scrapped <- comment_scrape_df(rivian_sentiment)
write_csv(rivian_comment_scrapped, 'rivian_comment.csv')

teslamodel3_comment_scrapped <- comment_scrape_df(teslamodel3_sentiment)
write_csv(teslamodel3_comment_scrapped, 'teslamodel3_comment.csv')

library(beepr)
beep(8)
