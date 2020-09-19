library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(gganimate)
library(tidytext)
windowsFonts(`Roboto Condensed` = windowsFont("Roboto Condensed"))

source("Data_Prep.R")


# Efficiency by Crop Over Time --------------------------------------------
efficiency <- full_data %>% group_by(Crop,Year) %>% summarise(Yield=mean(Yield,na.rm = T)) %>% 
  ungroup() %>% group_by(Crop) %>% mutate(base_yield=head(Yield,1)) %>% ungroup() %>% 
  mutate(relative_yield=Yield/base_yield)

#Heatmap
efficiency %>% 
  #filter(Crop %in% c("Bananas", "Barley", "Beans", "Cocoabeans", "Maize", "Potatoes", "Rice", "Wheat")) %>% 
  ggplot(aes(x=Year,y=fct_reorder(Crop,.x = relative_yield, .fun = max))) +
  geom_tile(aes(fill=relative_yield, height=1)) +
  scale_fill_viridis_c(option = "inferno",
                       guide = guide_legend(title.position = "top")) +
  scale_x_continuous(expand = c(0,0), 
                     breaks = seq(1960,2020,by=5)) +
  labs(fill="Relative Yield",
       title="The world has gotten much better at making Maize, but not Cassava",
       subtitle="Crop yield per hectare relative to 1961",
       caption = "Plot: @jakepscott2020 | Data: Our World in Data") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = rel(1.2)),
        plot.subtitle = element_text(face = "plain", size = rel(.9), color = "grey70"),
        plot.caption = element_text(size = rel(0.9), face="italic", 
                                    color = "grey70"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title.position = "plot",
        legend.position = "bottom",
        legend.text = element_text(size=rel(.6)),
        legend.title = element_text(size=rel(.75)))

#ggsave("pngs/HeatMap.png",dpi=600)


# Efficiency by Crop by Continent Over Time --------------------------------------------
efficiency_cont <- full_data %>% group_by(Continent,Crop,Year) %>% summarise(Yield=mean(Yield,na.rm = T)) %>% 
  ungroup() %>% group_by(Continent,Crop) %>% mutate(base_yield=head(Yield,1)) %>% ungroup() %>% 
  mutate(relative_yield=Yield/base_yield) %>% filter(!is.na(Continent))

efficiency_cont  %>% 
  ggplot(aes(x=Year,y=relative_yield)) +
  geom_line(aes(color=Continent),lwd=1.25) +
  facet_wrap(~Crop) +
  scale_color_brewer(palette = "Set1", 
                    guide=guide_legend(title.position = "top",
                                       nrow = 1)) +
  scale_x_continuous(breaks = seq(1970,2010,by=20)) +
  labs(title="Asia has powered the increase in efficiency of Maize production",
       subtitle = "Relative crop yields per hectare by continent",
       caption = "Plot: @jakepscott2020 | Data: Our World in Data") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = rel(1.2)),
        plot.subtitle = element_text(face = "plain", size = rel(.9), color = "grey70"),
        plot.caption = element_text(size = rel(0.9), face="italic", 
                                    color = "grey70"),
        plot.title.position = "plot",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=rel(.75)),
        legend.title = element_text(size=rel(1)))

efficiency_cont %>% 
  filter(Crop=="Maize") %>% 
  ggplot(aes(x=Year,y=relative_yield)) +
  geom_line(aes(color=Continent),lwd=1.25) +
  scale_color_brewer(palette = "Set1", 
                     guide=guide_legend(title.position = "top",
                                        nrow = 1)) +
  scale_x_continuous(breaks = seq(1960,2020,by=10)) +
  labs(title="Asia has powered the increase in efficiency of Maize production",
       subtitle = "Maize yield per hectare relative to 1961",
       caption = "Plot: @jakepscott2020 | Data: Our World in Data") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = rel(1.2)),
        plot.subtitle = element_text(face = "plain", size = rel(.9), color = "grey70"),
        plot.caption = element_text(size = rel(0.9), face="italic", 
                                    color = "grey70"),
        plot.title.position = "plot",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=rel(.75)),
        legend.title = element_text(size=rel(1)))
#ggsave("pngs/LineChart.png",dpi=600)



# Efficiency by Crop by Continent Over Time --------------------------------------------
efficiency_count <- full_data %>% filter(Continent=="Asia") %>% 
  group_by(Country,Crop,Year) %>% summarise(Yield=mean(Yield,na.rm = T)) %>% 
  ungroup() %>% group_by(Country,Crop) %>% mutate(base_yield=head(Yield,1)) %>% ungroup() %>% 
  mutate(relative_yield=Yield/base_yield) %>% filter(!is.na(Country))

efficiency_count %>% 
  filter(Crop=="Maize") %>% 
  ggplot(aes(x=Year,y=relative_yield)) +
  geom_line() +
  facet_wrap(~Country) +
  scale_x_continuous(breaks = c(1979,2010)) +
  scale_y_continuous(breaks = c(0,30)) +
  labs(title="There is significant heterogeneity under the hood",
       subtitle = "Maize yield per hectare relative to 1961 by Country",
       caption = "Plot: @jakepscott2020 | Data: Our World in Data") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = rel(1.2)),
        plot.subtitle = element_text(face = "plain", size = rel(.9), color = "grey70"),
        plot.caption = element_text(size = rel(0.9), face="italic", 
                                    color = "grey70"),
        plot.title.position = "plot",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=rel(.75)),
        legend.title = element_text(size=rel(1)))
#ggsave("pngs/ByCountry.png",dpi=600)

# Getting the most popular crop by country in 2018 ------------------------
#Getting a world sf object
world <- ne_countries(scale = "small", returnclass = "sf") %>% rename("Country"=sovereignt,
                                                                      "Code"=adm0_a3)
#Getting the most common crop for 2018 for each country
max_crop_2018 <- full_data %>% 
  filter(Year==2018) %>% 
  select(Code,Country,Crop,Yield) %>% 
  group_by(Country) %>% 
  slice_max(order_by = Yield,
            n = 1) 
max_crop_2018 %>% ungroup() %>% count(Crop)

#Joining
world_2018 <- world %>% left_join(max_crop_2018,by="Code")

#Plotting
ggplot(data = world_2018) +
  geom_sf(aes(fill=Crop)) +
  scale_fill_brewer(palette = "Set1", 
                    guide=guide_legend(title.position = "top",
                                       nrow = 1)) +
  theme_void() +
  labs(title="The potato is king, the banana is a distant second",
       subtitle="Most produced crop by hectare produced in 2018",
       fill="Most produced crop by yield",
       caption = "Plot: @jakepscott2020 | Data: Our World in Data") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = rel(1.2)),
        plot.subtitle = element_text(face = "plain", size = rel(.9), color = "grey70"),
        plot.caption = element_text(size = rel(0.9), face="italic", 
                                    color = "grey70"),
        plot.title.position = "plot",
        legend.position = "bottom",
        legend.text = element_text(size=rel(.75)),
        legend.title = element_text(size=rel(1)))
#ggsave("pngs/World_Map.png", dpi=600)
