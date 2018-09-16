library(tidyverse)
library(rvest)

sold_boats <- read_html("http://www.usa505.org/classifieds/sold-boats-archive")

sold_boats <- sold_boats %>%
  html_nodes("h4") %>%
  html_text() %>% 
  as_tibble() %>% 
  filter( str_detect( value, "^\\d{4}"))

sold_listing <- sold_boats %>% 
  separate( value, into = c("year", "builder", "sail_num", "sell_notice", "photo_link"), sep = ".-.") %>% 
  filter( !is.na(photo_link) )

sold_listing <- sold_listing %>% 
  mutate( price = str_extract( sell_notice, "\\$.*"))

# clean and convert price to CAD
sold_listing <- sold_listing %>% 
  mutate( price = as.numeric(gsub("[\\$,]", "", price)) ) %>% 
  filter( !is.na(price)) %>% 
  mutate( price = price * 1.32730 )

# convert year to date
sold_listing <- sold_listing %>% 
  mutate( year2 = as.Date( paste0(year,"/1","/1"), format = '%Y/%m/%d' ))

sold_listing %>% 
  ggplot( aes(year2, price, label = sail_num) ) +
  geom_point() +
  geom_smooth() +
  ylab( "Price ($CAD @ 1.327*USD)" ) +
  ggtitle( "Price of 505s over year boat was built " )
