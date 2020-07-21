library(tidyverse)
library(rio)
library(janitor)
library(quantmod)


import_series <- function(series){
  import(series) %>% 
    gather(key = 'key',value = 'value', -1) %>% 
    spread(key =1, value = 3) %>% 
    clean_names() %>% 
    select(categoria, industria_manufacturera) %>% 
    mutate(categoria = categoria %>% str_extract("\\d{4}") %>% as.numeric()) %>% 
    arrange(categoria) %>% 
    mutate_all(as.numeric) 
}


series1 <- import_series("PBG_K_93_09.xls") %>% 
  filter(categoria <= 2004) %>% 
  mutate(var = industria_manufacturera/ last(industria_manufacturera)) %>% 
  transmute( year = categoria, value = var * 14795.03)



series2 <- import_series("PGB_K_04_09.xlsx") %>% 
  rename(year = 1, value = 2)

series <- rbind(series1 %>% head(nrow(series1)-1), series2)

export(series, "series.xlsx")
