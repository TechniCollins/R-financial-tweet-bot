## Settings ----

# IMPORTANT NOTICE
# This packages are all needed in the final version.
# Even if this mockup doesn't use them all, I want to be sure that they can all be loaded in AWS
# If some package cannot be loaded in AWS please flag it with a comment

library(dplyr)
library(tidyverse)
library(ggplot2)
library(tsibble)
library(httr)
library(jsonlite)
library(plotly)
library(MMWRweek)
library(DescTools)
library(zoo)
library(forecast)
library(fmpapi)
library(patchwork)
library(gridExtra)
library(rtweet)

options(dplyr.summarise.inform = FALSE)
options(scipen=999)

# The following should be filled with the twitter app parameters that will be used for testing
appname <- Sys.getenv("uppercaseBee")
key <- Sys.getenv("CONSUMER_TOKEN")
secret <- Sys.getenv("CONSUMER_SECRET")
access_token = Sys.getenv("ACCESS_TOKEN")
access_secret = Sys.getenv("ACCESS_SECRET")

# Create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)

## Functions ----

mockup_get_ticker_data <- function(ticker, min_date){
  
  working_date <-  Sys.Date()
  print(working_date)
  
  ## 1. ticker series
  
  call_1 <- paste0("https://financialmodelingprep.com/api/v3/technical_indicator/daily/",ticker,"?period=20&type=ema&apikey=5ca27a5e621ee543209bcf00820b333a")
  
  ticker_history <- GET(call_1)
  
  ticker_history_text <- content(ticker_history, "text")
  
  ticker_history_json <- fromJSON(ticker_history_text, flatten = TRUE)
  
  ticker_history_df <- as.data.frame(ticker_history_json) %>% 
    filter(date >= min_date) %>% 
    mutate(symbol = ticker) %>% 
    select(symbol, date, close, volume)
  
  ticker_history_df <- ticker_history_df[complete.cases(ticker_history_df),]
  
  ## 2. Additional info
  
  call_2 <- paste0("https://financialmodelingprep.com/api/v3/profile/",ticker,"?apikey=5ca27a5e621ee543209bcf00820b333a")
  
  ticker_profile <- GET(call_2)
  
  ticker_text <- content(ticker_profile, "text")
  
  ticker_json <- fromJSON(ticker_text, flatten = TRUE)
  
  profile_data <- as.data.frame(ticker_json) %>%
    mutate(capmln = round(mktCap / 1000000, 2),
           shareout = round(mktCap / price, 2)) %>% 
    select(symbol, companyName, sector, industry, capmln, shareout)
  
  return(list("series_data" = ticker_history_df, "profile_data" = profile_data))
  
}

mockup_prepare_output <- function(tickers, starting_date){
  
  reporting_table <- data.frame("symbol" = NA, "date" = NA, "close" = NA, "volume" = NA)
  
  for(s in tickers){
    
    tryCatch(
      {
        
        print(s)
        
        tempo_ticker_data <- mockup_get_ticker_data(ticker = s, min_date = starting_date)
        
        reporting_table <- rbind(reporting_table, tempo_ticker_data$series_data)
        
      },
      error=function(cond){
        
        message(paste("error", s))
        message(cond)
        
      }
    )
    
  }
  
  reporting_table <- reporting_table %>% 
    filter(!is.na(symbol)) %>% 
    group_by(symbol) %>% 
    arrange(desc(date), .by_group = TRUE)
  
  return(reporting_table)
  
}

mockup_plot_ticker_data <- function(input_data, ploto = FALSE){
  
    maxdate <- max(input_data$date)
    mindate <- min(input_data$date)
    
    plot_a <-
      ggplot(data = input_data) +
      geom_line(aes(x = as.Date(date), y = close, group = 1), colour = "violet", size = 2) +
      theme(panel.background = element_rect(fill = 'black', colour = 'white'), panel.grid.minor =  element_line(colour = "lightgray"),  panel.grid.major =  element_line(colour = "gray")) +
      ggtitle(paste(unique(input_data$symbol)," - stock price and volume from ",mindate," to ",maxdate, sep = "")) +
      ylab("USD price") +
      xlab("Date") + 
      theme(plot.title = element_text(size=22),
            axis.text = element_text(size=18),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=18))
    
    plot_b <-
      ggplot(data = input_data)+
      geom_col(aes(x = as.Date(date), y = volume), colour = "green", fill = "green") +
      theme(panel.background = element_rect(fill = 'black', colour = 'white'), panel.grid.minor =  element_blank(), panel.grid.major =  element_line(colour = "gray")) +
      ylab("# securities") +
      xlab("Date") + 
      theme(axis.text = element_text(size=18),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=18))
  
    plot_c <-
      ggplot(data = input_data)+
      geom_col(aes(x = as.Date(date), y = volume), colour = "red", fill = "red") +
      theme(panel.background = element_rect(fill = 'black', colour = 'white'), panel.grid.minor =  element_blank(), panel.grid.major =  element_line(colour = "gray")) +
      ylab("# securities") +
      xlab("Date") + 
      theme(axis.text = element_text(size=18),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=18))
  
  
    plot_d <- 
      plot_a / plot_b / plot_c + plot_layout(heights = unit(c(10, 4, 4), c('cm', 'cm', 'cm')))
    
    if(ploto == TRUE){
      plot(plot_d)
  }
  
  return(plot_d)
}

mockup_save_ticker_plot <- function(input_plot, path, file_name, sirv_token){
  
  address = paste0(path,file_name,".png")
  
  png(filename = address,  width = 1458, height = 820)

  plot(input_plot)
  dev.off()

  # save image to Sirv
  # upload_to_sirv <- POST(paste0("https://api.sirv.com/v2/files/upload?filename=%2Ftwitter_bot/", file_name, ".png"), body = upload_file(paste0(path,file_name,".png")), add_headers(authorization = paste("Bearer", sirv_token)))
}

mockup_post_charts <- function(input_table, folder_path){
  # Get Sirv bearer token
  # bearer_token <- content(POST("https://api.sirv.com/v2/token", header = ("content-type: application/json"), body = list(clientId = Sys.getenv("SIRV_CLIENT_ID"), clientSecret = Sys.getenv("SIRV_CLIENT_SECRET")), encode="json"))$token
  
  for (ticker in unique(input_table$symbol)){
    
    tryCatch(
      {
        
        print(ticker)
        
        working_data <- input_table %>% 
          filter(symbol == ticker)
        
        working_plot <- mockup_plot_ticker_data(input_data = working_data, ploto = FALSE)
        
        mockup_save_ticker_plot(input_plot = working_plot, path = folder_path, file_name = ticker, bearer_token)
        
        ticker_purchase_text = "Testing..."
        
        post_tweet(ticker_purchase_text, media = paste0(folder_path,ticker,".png"))
        
      },
      error=function(cond){
        
        message("error posting")
        message(cond)
      
      }
    )
  }
}

clean_all_folders <- function(main_path){
  # get all files in the directories, recursively
  f <- list.files(main_path, include.dirs = F, full.names = T, recursive = T)
  # remove the files
  file.remove(f)
}

## Workflow ----

mockup_reporting_table <- mockup_prepare_output(tickers = c("AAPL", "VLVAA", "NEO"), starting_date = "2021-08-01")

# Folder path to be changed as S3 address?

mockup_post_charts(input_table = mockup_reporting_table, folder_path = "/application/images/" )

clean_all_folders(main_path = "/application/images/")

# How to build the image
# docker-compose up --build

# CRON job
# 0 20 * * 2,3,4,5,6 docker run mockup_twitter_bot_r_twitter_bot
