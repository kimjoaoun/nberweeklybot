library(telegram.bot)
library(rvest)
library(stringr)
library(jsonlite)
library(dplyr)

token_nber <- "YOUR BOT KEY"

bot <- Bot(token = token_nber)

updater <- Updater(token = token_nber)

### The Welcome Message!

start <- function(bot, update) {
  bot$sendMessage(
    chat_id = update$message$chat_id,
    text = sprintf("Hello %s! I'm the nberweekly_bot, and my job is to deliver you the last 
                  edition of the NBER newsletter and it's awesome papers.", update$message$from$first_name)
  )
}

start_handler <- CommandHandler("start", start)
updater <- updater + start_handler

### What to answer if the user enter a unknown command?

unknown <- function(bot, update) {
  bot$sendMessage(
    chat_id = update$message$chat_id,
    text = "Sorry, I didn't understand that command."
  )
}
updater <- updater + MessageHandler(unknown, MessageFilters$command)

### Function to Collect the Newsletter

papers <- function(bot, update) {
  nber_page <- read_html("https://data.nber.org/new.html")

  nber_df <- nber_page %>%
    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "multiline-li", " " ))]') %>%
    html_text() %>%
    data.frame() %>%
    tibble::rowid_to_column()

  nber_links <- nber_page %>%
    html_nodes(xpath = "//a") %>%
    html_attr("href") %>%
    tibble()

  names(nber_links) <- "lnks"
  nber_links %<>%
    filter(grepl("/papers/w", lnks))

  nber_df <- bind_cols(nber_df, nber_links)
  names(nber_df) <- c("id", "paper_title", "links")

  nber_df$paper_title <- str_replace_all(nber_df$paper_title, "[\r\n]", "|") %>% substring(2)
  nber_df$author <- str_extract(nber_df$paper_title, "\\|.*") %>% str_remove("\\|")
  nber_df$paper_title <- str_remove(nber_df$paper_title, "\\|.*")
  nber_df$tags <- str_extract(nber_df$author, "\\(.*?\\)")
  nber_df$author <- str_remove(nber_df$author, "\\(.*?\\)")


  bot$send_message(
    chat_id = update$message$chat_id,
    text = paste("no.", nber_df$id,
      ". The Working Paper:", nber_df$paper_title,
      ". That was written by:", nber_df$author,
      # "is tagged as:", nber_df$tags,    # I think showing the tags is useless but you don't, just uncomment this line.
      "and can be seen at:", nber_df$links,
      sep = " "
    )
  )
}

updater <- updater + CommandHandler("papers", papers)

### Function to know where is the NBER (it returns you the location of the NBER HQ in the US.)

whereisnber <- function(bot, update) {
  bot$send_message(
    chat_id = update$message$chat_id,
    text = "You want to know here is the NBER HQ? Ok, it is here:"
  )
  bot$send_location(
    chat_id = update$message$chat_id,
    latitude = 42.3699185,
    longitude = -71.1131004
  )
}
updater <- updater + CommandHandler("whereis", whereisnber)

updater$start_polling()


getwp <- function(bot, update, args) {

}
