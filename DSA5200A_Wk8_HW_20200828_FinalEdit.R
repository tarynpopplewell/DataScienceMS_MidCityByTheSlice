#Data Visualization DSA 5200A 
#Merrimack College - Summer 2020
#Homework 4

#Invoke the required library packages
# install.packages("data.table")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("scales")
# install.packages("viridis")
# install.packages("sf")
# install.packages("maps")
#install.packages("gmodels")
#install.packages("tidyr")
install.packages("gt")
library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(viridis)
library(sf)
library(maps)
library(ggpattern)
library(stringr)
library(gt)
library(forcats)

ds1 <- fread("D:/Merrimack/DSA5200A - Data Visualization/Week 8/Pizza_Data_AllDates.csv", stringsAsFactors=FALSE)
ds1$ZipCode <- as.character(ds1$ZipCode)
ds1$CollectionPeriod <- as.Date(ds1$CollectionPeriod, format = "%m/%d/%Y")

zip_boundaries <- st_read("D:/Merrimack/DSA5200A - Data Visualization/Week 8/San Diego Zip Codes")
zip_boundaries$zip <- as.character(zip_boundaries$zip)
zip_boundaries <- zip_boundaries %>% filter(community == 'San Diego')

PRLatLong <- fread("D:/Merrimack/DSA5200A - Data Visualization/Week 8/Pizza_Data_LatLong.csv", stringsAsFactors=FALSE)
names(PRLatLong)[names(PRLatLong) == "V1"] <- "RestaurantCode"
names(PRLatLong)[names(PRLatLong) == "V2"] <- "RLat"
names(PRLatLong)[names(PRLatLong) == "V3"] <- "RLong"

ds2 <- ds1
ds2$NLat <- ifelse(ds2$ZipCode == "92103",32.7498,
                  ifelse(ds2$ZipCode == "92104",32.7399,
                         ifelse(ds2$ZipCode == "92116",32.7679,0)))
ds2$NLong <- ifelse(ds2$ZipCode == "92103",-117.1677,
                   ifelse(ds2$ZipCode == "92104",-117.1206,
                          ifelse(ds2$ZipCode == "92116",-117.1235,0)))

ds2 <- ds2 %>% left_join(PRLatLong, by = c("RestaurantCode" = "RestaurantCode"))

######################################
#DATA PREP
######################################

#DATA - Avg Pizza Price by Neighborhood + Lat/Long
pmN <- ds2 %>%
  group_by(ZipCode, Neighborhood, NLong, NLat) %>%
  summarize(mean_PizzaPrice = mean(PizzaPrice, na.rm = TRUE))
pmN <- pmN %>% left_join(zip_boundaries, by = c("ZipCode" = "zip" ))

#DATA - Avg Pizza Price by Restaurant + Lat/Long
pmR <- ds2 %>%
  group_by(ZipCode, RestaurantName, RLong, RLat) %>%
  summarize(mean_PizzaPrice = mean(PizzaPrice, na.rm = TRUE))
pmR <- pmR %>% left_join(zip_boundaries, by = c("ZipCode" = "zip" ))

#DATA - Avg Pizza Price by Restaurant
pmR_Rsort <- ds2 %>%
  group_by(RestaurantName) %>%
  summarize(mean_PizzaPrice = mean(PizzaPrice, na.rm = TRUE)) %>%
  mutate(RestaurantName = fct_reorder(RestaurantName, desc(mean_PizzaPrice)))

#DATA - Avg Pizza Price by Neighborhood and Collection Period + Lat/Long
pNCP <- ds2 %>%
  group_by(ZipCode, Neighborhood, CollectionPeriod, NLong, NLat) %>%
  summarize(mean_PizzaPrice = mean(PizzaPrice), 
            mean_Pricein2 = mean(Pricein2),
            mean_PriceTopp = mean(PriceTopping),
            mean_ToppCount = mean(NumberofToppings),
            mean_PricebyToppCnt = mean_PizzaPrice/mean_ToppCount)

#DATA - Avg Pizza Price by Restaurant and Collection Period + Lat/Long
pRCP <- ds2 %>%
  group_by(ZipCode, RestaurantName, CollectionPeriod, NLong, NLat) %>%
  summarize(mean_PizzaPrice = mean(PizzaPrice), 
            mean_Pricein2 = mean(Pricein2),
            mean_PriceTopp = mean(PriceTopping),
            mean_ToppCount = mean(NumberofToppings),
            mean_PricebyToppCnt = mean_PizzaPrice/mean_ToppCount)

#DATA Create the Percentages Crosstable
XYh <- ds2 %>%
  group_by(Neighborhood) %>%
  summarize(mean_PizzaPrice = mean(PizzaPrice, na.rm = TRUE)) %>%
  mutate(Neighborhood=paste0(Neighborhood)) %>%
  spread(Neighborhood, mean_PizzaPrice)

Ratio_H <- XYh %>% 
  mutate(A1 = (Hillcrest/Hillcrest)-1)  %>% 
  mutate(B1 = (`North Park`/Hillcrest)-1)  %>% 
  mutate(C1 = (`Uni Heights`/Hillcrest)-1) %>%
  mutate(Neighborhood = 'Hillcrest') %>%
  select(Neighborhood,A1:C1)

Ratio_N <- XYh %>% 
  mutate(A1 = (Hillcrest/`North Park`)-1)  %>% 
  mutate(B1 = (`North Park`/`North Park`)-1)  %>% 
  mutate(C1 = (`Uni Heights`/`North Park`)-1)  %>%
  mutate(Neighborhood = 'North Park') %>%
  select(Neighborhood,A1:C1)

Ratio_U <- XYh %>% 
  mutate(A1 = (Hillcrest/`Uni Heights`)-1)  %>% 
  mutate(B1 = (`North Park`/`Uni Heights`)-1)  %>% 
  mutate(C1 = (`Uni Heights`/`Uni Heights`)-1)  %>%
  mutate(Neighborhood = 'Uni Heights') %>%
  select(Neighborhood,A1:C1)

#rm(Ratio_Table, Ratio_H, Ratio_N, Ratio_U, XYh)

Ratio_Table <- bind_rows(Ratio_H, Ratio_N, Ratio_U) 

Ratio_Table <- Ratio_Table %>%
    mutate_if(is.numeric, scales::percent) %>%
  rename(
    'Hillcrest' = A1,
    'North Park' = B1,
    'Uni Heights' = C1
  )

###############################################################################
#CHARTS
###############################################################################

#CHART - COLUMN - Avg Pizza Pie Price by Neighborhood
ggplot(pmN, aes(Neighborhood, mean_PizzaPrice )) + 
  geom_col_pattern(
    aes(Neighborhood, mean_PizzaPrice, pattern_fill = Neighborhood), 
    pattern = 'image',
    pattern_type = 'squish',
    pattern_gravity = 'West',
    pattern_filename = 'D:/Merrimack/DSA5200A - Data Visualization/Week 8/R work/pizza-png2.png'
  ) +
  theme(legend.position = "none") +
  labs(title="Average Pizza Pie Price by Neighborhood") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels=scales::dollar_format()) +
  #scale_x_discrete("ZipCode") + 
  #expand_limits(y = 0) +
  ylab("Average Price") +
  xlab(NULL)

#CHART - MAP - VERSION 2 - Average Pizza Price  by Zip Code and Restaurant
ggplot(pmR, aes(fill = mean_PizzaPrice)) +
  geom_sf(aes(geometry = geometry)) +
  labs(title = "Average Pizza Prices by Zip Code",
       caption = "Mid-City Neighborhoods in San Diego, CA",
       fill = "Avg Pizza Price",
       size = "Avg Pizza Price",
       labels = dollar) +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5)) +
  ylab(NULL) + 
  xlab(NULL) +
  scale_fill_viridis_c(labels = dollar) +
  coord_sf(xlim = c(-117.192, -117.1),ylim = c(32.725, 32.778)) +
  geom_point(data = pmR, aes(x=RLong, y=RLat, size = mean_PizzaPrice), shape=23, color="darkred", fill = "red", labels=scales::dollar()) 


#CHART - TABLE - Cost Ramifications of Travel
Ratio_Table %>% gt()

#CHART - COLUMN - Avg Pizza Price by Restaurant
ggplot(pmR_Rsort) + 
  geom_col_pattern(
    aes(RestaurantName, mean_PizzaPrice, pattern_fill = RestaurantName), 
    pattern = 'image',
    pattern_type = 'expand',
    pattern_gravity = 'West',
    pattern_filename = 'D:/Merrimack/DSA5200A - Data Visualization/Week 8/R work/pizza_h.png'
  ) +
  coord_flip() +
  theme(legend.position = "none") +
  labs(title="Average Pizza Price by Restaurant") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1.05, vjust = 1.05)) +
  scale_y_continuous(labels=scales::dollar_format()) +
  scale_x_discrete("Restaurant") + 
  expand_limits(y = 0) +
  ylab("Average Price")


#CHART - LINE - Avg Pizza Price by Restaurant and Collection Period
ggplot(pRCP, aes(x = CollectionPeriod, y = mean_PizzaPrice, group = RestaurantName )) + 
  geom_line(size = 2) +
  geom_point()+
  labs(title="Average Pizza Price \nby Restaurant and Collection Period") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1.05, vjust = 1.05)) +
  scale_y_continuous(labels=scales::dollar_format()) +
  ylab("Average Price")+
  xlab("Collection Period")  +
  facet_wrap(~RestaurantName, labeller = labeller(RestaurantName = label_wrap_gen(15)))


TableTest <- Ratio_Table %>% gt()
print(TableTest)

################################################################################################################

#CHART - MAP - VERSION 2 - Average Pizza Price  by Zip Code and Restaurant
ggplot(pmR, aes(fill = mean_PizzaPrice)) +
  geom_sf(aes(geometry = geometry)) +
  labs(title = "Average Pizza Prices by Zip Code",
       subtitle = "Each diamond represents an individual restaurant.",
       caption = "Mid-City Neighborhoods in San Diego, CA",
       fill = "Avg Pizza Price \nby Zip Code ($)",
       size = "Avg Pizza Price \nby Restaurant ($)") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5)) +
  ylab(NULL) + 
  xlab(NULL) +
  scale_fill_viridis_c() +
  coord_sf(xlim = c(-117.192, -117.1),ylim = c(32.725, 32.778)) +
  geom_point(data = pmR, aes(x=RLong, y=RLat, size = mean_PizzaPrice), shape=23, color="darkred", fill = "red") +
  guides(size = guide_legend(reverse = TRUE)) 

  
