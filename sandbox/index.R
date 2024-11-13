library(tidyverse)
library(nycflights13)
library(dplyr)
library(ggplot2)

library(palmerpenguins)
ggplot(penguins)


# library(ggpmisc)
# 
#   ggplot(penguins, 
#          aes(x=flipper_length_mm, y=body_mass_g, color = species)) + 
#     geom_point() + 
#     geom_smooth(method="lm", se=FALSE, color="black") + 
#     stat_poly_eq() + 
#     xlab("Flipper Length (mm)") + 
#     ylab("Body Mass (g)") +
#     theme_bw() +
#     scale_color_brewer(type="qual", palette=6, name="Species")  + 
#     theme(legend.position="bottom") +
#     facet_wrap(~species) +
#     ggtitle("Correlation of Flipper Length and Body Mass across Penguin Species") +
#     labs(caption="Data provided by Dr. K. Gorman and the Palmer Station, Antarctica LTER") +
#     guides(color="none")

# plot(diamonds$carat, diamonds$price)
# 
# ggplot(diamonds, aes(x = carat, y = price, col=clarity)) + 
#   geom_point() + 
#   facet_wrap(~cut) +
#   geom_smooth(color="red") +
#   theme_bw()
# 
# 
# ggplot(diamonds, aes(x = diamonds$price, color = color)) +
#   geom_freqpoly(binwidth = 10)

# install.packages("CVXR")
# library(CVXR)
# library(tidyverse)
# data(cdiac)
# glimpse(cdiac)
# 
# cdiac$year_date <- as.Date(paste0(cdiac$year, "-01-01"))
# 
# ggplot(data = cdiac, aes(x = year_date, y = annual )) + 
#   scale_x_date(name = "Year", date_breaks = "10 year", date_labels = "%Y") +
#   geom_point()
# 
# 
# install.packages("gapminder")

# library(sf)
# library(dplyr)
# library(ggplot2)
# nc <- read_sf(system.file("shape/nc.shp", package="sf"))
# nc |> select(NAME, AREA, PERIMETER, geometry)
# ggplot(nc, aes(geometry = geometry, fill = BIR74)) + geom_sf()



library(sf)
library(ggplot2)
nyc_demos <- read_csv("data/in_class/nyc_demos.csv")
nycc <- sf::read_sf("data/in_class/nycc_24c/nycc.shp")

glimpse(nycc) # CounDist  
glimpse(nyc_demos) #district

shape_data <- nycc |> 
  inner_join(nyc_demos, join_by("CounDist" == "district"))

unique(nyc_demos$field)

shape_data_white <- shape_data |> 
  filter(field == "White Nonhispanic") |> 
  mutate(change = Y2010 - Y2000)

# ggplot(shape_data_white, aes(geometry = geometry, fill = change)) + geom_sf()
# 
# View(nyc_demos)

shape_data_adult <- shape_data |> 
  filter(field == "18 years and over") 

AVG_ADULTS <- mean(shape_data_adult$Y2010)

shape_data_adult <- shape_data_adult |> 
  mutate(dif = (Y2010 - AVG_ADULTS) / AVG_ADULTS )

glimpse(shape_data_adult)

ggplot(shape_data_adult, aes(geometry = geometry, fill = dif)) + geom_sf()
# shp_data <- st_read(nycc)
# 
# ggplot(nycc, aes(geometry = geometry)) +
#   geom_sf()
# 
# str(shp_data)
# children_under_5 <- nyc_demos |> 
#   filter(field == "Under 5 years")
# glimpse(children_under_5)

# nyc_demos |> 
#   filter(field == "18 years and over") |>
#   mutate(voting_power_2010 = mean(Y2010) / Y2010 - 1) |>
#   left_join(nycc, join_by(district == CountDist))

# data <- read.csv("data/in_class/candy.csv")
# 
# ggplot(data, aes(x = pricepercent, y= winpercent)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE) +
#   labs(title = "Regression of Win Percent vs Sugar Percent",
#        x = "Price Percent",
#        y = "Win Percent") +
#   theme_minimal()
# 
# data %>% 
#   group_by(sugarpercent) %>% 
#   summarise(num_wins = n()) %>% 
#   ggplot(aes(x = sugarpercent,y = num_wins)) +
#   geom_histogram(stat = 'identity',width = 0.1)

install.packages("httr2")
library(httr2)
req <- request("https://r-project.org")
req |> req_headers("Accept" = "application/json")
req |> req_body_json(list(x = 1, y = 2))
req |> req_url_path(path = "path/to/my/file")
req |> req_retry(max_tries = 5)
req |> req_method("PATCH")

req |> req_dry_run()

resp <- req_perform(req)
resp |> resp_content_type()
resp |> resp_status_desc()
resp |> resp_body_html()

req <- request("https://api.artic.edu/api/v1/artworks/27992?fields=id,title,image_id") |> 
  req_headers("Accept" = "application/json") |>
  req_method("GET")

resp <- req_perform(req)
# resp |> resp_content_type()
# resp |> resp_status_desc()
json <- resp |> resp_body_json()


json
