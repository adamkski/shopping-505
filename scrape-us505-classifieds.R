library(tidyverse)
library(modelr)
library(rvest)
boats <- read_html("http://www.usa505.org/classifieds")

# select headers by bk color
extract_category <- function(cat, colour, data) {
  data %>% 
    html_nodes( paste0("[style~='background-color:", colour,"']") ) %>% 
    html_text() %>% 
    as_tibble() %>% 
    mutate( category = cat )
}

headers <- bind_rows(
  extract_category("1", "rgb(159,197,232)", boats), 
  extract_category("2", "rgb(182,215,168)", boats),
  extract_category("3", "rgb(255,229,153)", boats),
  extract_category("4", "rgb(249,203,156)", boats),
  extract_category("5", "rgb(234,153,153)", boats)
)


listing <- headers %>% 
  separate( value, into = c("year", "builder", "sail_num", "location", "price", "photo_link"), sep = ".-.") 

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
  ggplot( aes(year2, price, label = sail_num, color = category) ) +
  geom_point() +
  geom_label( aes( fill = category ), color = "black", fontface = "bold", size = 3, angle = 45, hjust = 0, nudge_x = 125 ) +
  scale_color_manual( values = rgb_cols ) +
  scale_fill_manual( values = rgb_cols ) +
  ylab( "Price ($CAD @ 1.327*USD)" ) +
  ggtitle( "Price of 505s over year boat was built " )
 
ggsave("price-plot.jpeg")

listing %>% 
  ggplot( aes(year2, price, label = sail_num) ) +
  geom_point() +
  geom_smooth() +
  ylab( "Price ($CAD @ 1.327*USD)" ) +
  ggtitle( "Price of 505s over year boat was built " )

ggsave("price-plot_smooth.jpeg")


listing
# fit models
mod1 <- lm( price ~ year, listing)
mod2 <- loess( price ~ year, listing )

grid <- listing %>% 
  data_grid( year ) %>% 
  add_predictions(mod1)

listing %>% 
  ggplot( aes( year )) +
  geom_point( aes(y=price) ) +
  geom_line( aes( y = pred ), data = grid, colour = "red", size = 1)

listing %>%
  gather_residuals( mod1, mod2 ) %>% 
  ggplot( aes( resid )) +
  facet_wrap( ~model) +
  geom_freqpoly()

listing %>%
  gather_residuals( mod1, mod2 ) %>% 
  ggplot( aes( year, resid )) +
  facet_wrap( ~model) +
  geom_point()


# fit loess model
