library(rvest)

tgl <- Sys.Date() - 1

param <- list(d = format(tgl, "%d"),
              m = format(tgl, "%m"),
              y = format(tgl, "%Y"))
# Kompas.com terpopular

kompas_url <- sprintf("https://news.kompas.com/search/%s-%s-%s/", param$y, param$m, param$d)

kompas_npage <- read_html(kompas_url) %>% 
  html_nodes("div.paging__item") %>% 
  html_nodes("a.paging__link--prev") %>% 
  html_attr("data-ci-pagination-page") %>% 
  as.numeric() 

kompas_headlines <- purrr::map(1:kompas_npage, function(.x){
  read_html(paste0(kompas_url, .x)) %>% 
    html_nodes("div.article__list__title") %>% 
    # html_nodes("div.most__list") %>% 
    # html_nodes("a.most__link") %>% 
    html_nodes("h3.article__title") %>%
    html_nodes("a") %>% 
    html_text()
}) %>% unlist()

k <- data.frame(sources = "Kompas",
                headlines = kompas_headlines)

# Detik.com terpopular
detik_url <- sprintf("https://news.detik.com/indeks?date=%s/%s/%s", param$m, param$d, param$y)

detik_npage <- read_html(detik_url) %>% 
  html_nodes("a.pagination__item") %>% 
  html_text() %>% 
  as.numeric() %>% 
  # suppressWarnings() %>% 
  max(na.rm = TRUE)

detik_headlines <- purrr::map(.x = 1:detik_npage, .f = function(.x){
  read_html(sprintf("https://news.detik.com/indeks/%s?date=%s/%s/%s", .x, param$m, param$d, param$y)) %>% 
    html_nodes("div.media__text") %>% 
    html_nodes("a.media__link") %>% 
    html_text()
}) %>% unlist()

d <- data.frame(sources = "Detik",
                headlines = detik_headlines)

popular_headlines <- rbind.data.frame(k, d)

library(tidytext)
library(dplyr)
library(stringr)
library(katadasaR)

sw <- read.table("stopword_list_id_2.txt", header = FALSE, col.names = "word")
# sw <- data.frame(word = c("di", "ada", "ke", "dari", "buat", "ini", "dalam"))

headlines <- popular_headlines %>% 
  mutate(headlines = str_remove_all(headlines, "-"),
         headlines = str_remove_all(headlines, "[:digit:]"),
         headlines = str_remove_all(headlines, "[:punct:]")) %>% 
  unnest_tokens(output = "word", input = "headlines", 
                to_lower = TRUE, token = "words")

headlines <- headlines %>% 
  mutate(word = word %>% 
  purrr::map_chr(katadasar)) %>% 
  anti_join(sw, by = "word")

headlines %>% 
  count(word, name = "freq", sort = TRUE) %>% 
  # head(10)
  filter(freq > 1) %>% 
  wordcloud2::wordcloud2(shape = "circle")

headlines %>% 
  filter(sources == "Kompas") %>% 
  count(word, name = "freq", sort = TRUE) %>% 
  # head(10)
  filter(freq > 1) %>% 
  wordcloud2::wordcloud2(shape = "circle")

headlines %>% 
  filter(sources == "Detik") %>% 
  count(word, name = "freq", sort = TRUE) %>% 
  # head(10)
  filter(freq > 1) %>% 
  wordcloud2::wordcloud2(shape = "circle")
