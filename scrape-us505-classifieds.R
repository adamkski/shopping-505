library(tidyverse)
library(rvest)
boats <- read_html("http://www.usa505.org/classifieds")

headers <- boats %>%
  html_nodes("h4") %>%
  html_text() %>% 
  as_tibble() %>% 
  filter( str_detect( value, "^\\d{4}"))
  
listing <- headers %>% 
  separate( value, into = c("year", "builder", "sail_num", "location", "price", "photo_link"), sep = ".-.") %>% 
  filter( !is.na(price))

# add category
listing <- listing %>% 
  mutate( cat = as.factor(case_when(
    sail_num %in% c("5818","3061") ~ "1",
    sail_num %in% c("8240","8244","8194") ~ "2",
    sail_num %in% c("7877","6821","7347","7776","8438","7197","8013","8190","7318","7879") ~ "3",
    sail_num %in% c("8559","8627","8083","8821","7875","8265") ~ "4",
    sail_num %in% c("8824","8813","8851","8951","8853","9165","9187") ~ "5"
  )))

# clean and convert price to CAD
listing <- listing %>% 
  mutate( price = as.numeric(gsub("[\\$,]", "", price)) ) %>% 
  filter( !is.na(price)) %>% 
  mutate( price = price * 1.32730 )

# convert year to date
listing <- listing %>% 
  mutate( year2 = as.Date( paste0(year,"/1","/1"), format = '%Y/%m/%d' ))

rgb_cols <- c(
  rgb(153,204,252, maxColorValue = 255),
  rgb(203,251,205, maxColorValue = 255),
  rgb(254,253,204, maxColorValue = 255),
  rgb(248,203,153, maxColorValue = 255),
  rgb(236,126,127, maxColorValue = 255)
  )

listing %>% 
  ggplot( aes(year2, price, label = sail_num, color = cat) ) +
  geom_point() +
  geom_smooth() +
  geom_label( aes( fill = cat ), color = "black", fontface = "bold", size = 3, angle = 45, hjust = 0, nudge_x = 125 ) +
  scale_color_manual( values = rgb_cols ) +
  scale_fill_manual( values = rgb_cols ) +
  ylab( "Price ($CAD @ 1.327*USD)" ) +
  ggtitle( "Price of 505s over year boat was built " )
 
ggsave("price-plot.jpeg")
