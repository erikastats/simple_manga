
# Libraries ---------------------------------------------------------------


ulr_test <-  'https://w65.1piecemanga.com/manga/one-piece-chapter-27/'
# functions ---------------------------------------------------------------

get_imageurl <- function(url_pages){
  library(tidyverse)
  library(rvest)
  
  general_page <- url_pages %>% 
    read_html()
  
  chapter <-  url_pages %>% 
    str_replace_all('https://w65.1piecemanga.com/manga/one-piece-chapter', "") %>% 
    str_extract_all("[:digit:]+") %>%  .[[1]] %>%  as.numeric()
  
  umages_list <-  general_page %>%  html_elements('img') %>% 
    html_attr('scr')
  
  df_imaged <-  tibble(Chapter = chapter, 
                       Images_url = images_list)
  df_imaged <-  df_imaged %>% 
    bind_cols(manga_page = 1:nrow(df_imaged))
  df_imaged
}

