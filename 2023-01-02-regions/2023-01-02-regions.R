library(tidyverse)
library(innovar)
library(sf)
library(rgee)
library(ggtext)
library(extrafont)
library(scico)
library(glue)
library(cowplot)
ee_Initialize()

# 1. Reading spatial data -------------------------------------------------

peru_region <- st_read(
  "https://github.com/ambarja/gpkg-pe/raw/main/departamentos.gpkg"
  ) %>% 
  select(NOMBDEP)

# New functions for get deforestation
area_deforest_f <- function(x){
  def <- peru_region[x,] %>% 
    sf_as_ee() %>%  
    get_def(
      from = "2001-01-01",
      to = "2021-12-31"
    )
  return(def)
}


# 2. Get Information of deforestation -------------------------------------

lista_area_deforest <- lapply(1:nrow(peru_region),area_deforest_f)

area_deforest <- lista_area_deforest %>% 
  map_df(.f = as.data.frame)

region_def <- peru_region %>% 
  left_join(area_deforest,by = "NOMBDEP") %>% 
  pivot_longer(
    cols = Adef_2001:Adef_2021,
    names_to = "year",
    values_to = "area_km2"
    
  ) %>% 
  mutate(year = as.numeric(str_extract(year,pattern = "[0-9]+")))
  

# 3. Cooking Plot ---------------------------------------------------------

p1 <- region_def %>% 
  filter(year != 2021) %>% 
  ggplot() + 
  geom_sf(
    aes(
      fill = area_km2),
    color = NA
    ) + 
  scale_fill_scico(palette = 'nuuk') +
  facet_wrap(~ year) + 
  guides(
    fill = guide_colourbar(
      title = "area deforest [km<sup/>2]",
      title.position = "top",
      barwidth = 16,
      barheight = .8,
      title.hjust = .5
    )
  ) + 
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent",colour = "transparent"),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "bottom",
    plot.margin=grid::unit(c(0,0,0,0), "mm"),
    strip.background =element_rect(fill=NA),
    strip.text = element_text(family = "Bebas Neue",size = 15),
    legend.title = element_markdown(family = "Oswald Medium",size = 14,padding = unit(c(0, 0, 0, 0), "mm")),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = NA)) + 
  xlab("") +
  ylab("") 

p2 <- region_def %>% 
  filter(year == 2021) %>% 
  ggplot() + 
  geom_sf(
    aes(fill = area_km2),
    color = NA
  ) + 
  scale_fill_scico(palette = 'nuuk') +
  labs(title = "2021") +
  xlab("") +
  ylab("")  + 
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent",colour = "transparent"),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "none",
    plot.margin=grid::unit(c(0,0,0,0), "mm"),
    plot.title = element_markdown(family = "Bebas Neue",hjust = .5,size = 40,face = "bold",
                                  margin=margin(0,0,-10,0)),
    panel.border = element_blank(),
    panel.grid = element_blank()
  )

p3 <- region_def %>% 
  st_drop_geometry() %>%
  group_by(year) %>% 
  summarise(area_km2 = sum(area_km2)) %>% 
  mutate(
    mean_area = mean(area_km2),
  ) %>% 
  ggplot(aes(x = year,y = area_km2, fill = area_km2)) + 
  geom_area(alpha=0.9) + 
  xlab(label = "") + 
  ylab(label = "") + 
  scale_x_continuous(limits = c(2001,2020), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,3500), expand = c(0, 0)) +
  geom_hline(aes(yintercept = 1759.125,colour = "Mean area [km2]"),linetype="dashed", show.legend = TRUE, size = 1.5) +
  scale_fill_gradientn(colours = c("#190D33")) + 
  guides(
    fill = guide_colourbar(
      title = "•  area deforest [km<sup/>2] <br>Peru",
      title.position = "left",
      barwidth = 1.0,
      barheight = 0.25,
      title.vjust = 0.7
    ),
    colour = guide_legend(title.vjust = 0.7,keywidth = 1.0,keyheight = 0.5,title.position = "left", fill = NA)
  ) +
  labs(color = "• mean area [km2]") + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.85,0.85),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent",colour = "transparent"),
    axis.text = element_markdown(family = "Oswald Light",size = 15),
    legend.title = element_markdown(family = "Oswald Medium",size = 5,padding = unit(c(0, 0, 0, 0), "mm")),
    legend.text = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(fill = NA),
    legend.key = element_rect(fill = NA)
  )

title <- ggplot(
  tibble(x = 1.5, y = 1),
  aes(x, y)) +
  labs(
    title = "<b>DEFORESTATION IN PERU</b>",
    subtitle = 
      "<br>Spatial and temporal variation of deforestation in the 24 departments <br> of Peru.
    Each department has the sum of the total area of forest lost <br> from 2001 to 2021.
    The departments with the largest accumulated <br> area of forest lost in the last 20
    years are led by <b>Loreto</b> and <b>San Martin.</b>") +
  theme(
    plot.title = element_textbox_simple(
      family = "Bebas Neue",
      face = "bold",
      color = "black",
      size = 60,
      lineheight = 1.35),
    plot.subtitle = element_textbox_simple(
      family = "Oswald Light",
      color = "black",
      size = 18,
      lineheight = 1.35)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent",colour = "transparent"),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.margin=grid::unit(c(2,-1,0,0), "mm"),
    plot.subtitle = element_textbox_simple(
      padding = margin(0, 0, 0, 0),
      margin = margin(0, 0, 0, 0),
      width = NULL
    ),
    panel.border = element_blank()
  ) + 
  xlab("") +
  ylab("")

date <- Sys.Date()
source <- ggplot(
  tibble(x = 1.5, y = 1),
  aes(x, y)) +
  labs(
    caption = 
      glue("#MapPromptMonday<br> Date: {date}<br>Developed by antony barja (ambarja)<br>Data: Global Forest Change - University of Maryland 
            Department of Geographical Sciences ")) +
  theme(
    plot.title = element_textbox_simple(
      family = "Bebas Neue",
      face = "bold",
      color = "black",
      size = 60,
      lineheight = 1.35),
    plot.subtitle = element_textbox_simple(
      family = "Oswald Light",
      color = "black",
      size = 18,
      lineheight = 1.35)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent",colour = "transparent"),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.margin=grid::unit(c(2,-1,0,0), "mm"),
    plot.caption = element_textbox_simple(
      margin = margin(0, 0, 0, 0),
      padding = margin(0, 0, 0, 0),
      size = 12,
      family = "Oswald Light",
      face = "bold"),
    panel.border = element_blank()
  ) + 
  xlab("") +
  ylab("") 


end <- ggdraw() +
  # title  - - - - - - - - - - - - - - - - - - 
  draw_plot(
    title,
    0.18, 0.64, 0.95, 0.30,
    scale = 1.4
  ) +
  # area plot  - - - - - - - - - - - - - - - - 
  draw_plot(
    p3,
    0.32, 0.5, 0.95, 0.75,scale = 0.35
  ) +
  draw_plot(
    source,
    -0.01, 0.001, 4.8, 4.80
  )+
  # map 1  - - - - - - - - - - - - - - - - - - 
  draw_plot(
    p1,
    0.5, 0.22, 0.5, 0.30,scale = 2.4
  ) + 
  # map 2 - - - - - - - - - - - - - - - - - - 
  draw_plot(
    p2,
    -0.04, 0.20, 0.5, 0.30,scale = 2.5
  ) 

ggsave(
  "final_plot.pdf",
  plot = end,
  width = 10,
  height = 10,
  bg = "#e4ddc3"
    )