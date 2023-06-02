## IMPACT MFS maps
## Author: Sirimon Thomas - sirimon.thomas@impact-initiatives.org
## Date: 01/05/2023

#options(timeout = 100000) # use if internet is slow to install larger packages

library(tidyverse)
library(sf)
library(ggrepel)
library(extrafont)
library(here)
library(ggsn)

rm(list = ls())

#font_import() # only run once
#loadfonts(device = 'all')


## import spatial layers
#adm0
ssd_base <- st_read("maps/input/jmmi_gis.gpkg", layer = 'ssd_base')
#amd1
ssd_states <- st_read("maps/input/jmmi_gis.gpkg", layer = 'ssd_states')
#amd2
ssd_counties <- st_read("maps/input/jmmi_gis.gpkg", layer = 'ssd_counties')

## import market points and MFS data
points <- read.csv('maps/input/jmmi_market_locations.csv')
mfs <- read.csv('output/mfs_ssd.csv')

## make market points a spatial layer and tidy names for labels
points.sf <- points %>% st_as_sf(coords = c('X','Y'), crs = 4326, remove = F) %>%
  mutate(
    label = str_replace(location, " Town",""),
    label = str_replace(label, "_"," "),
    label = str_replace(label, "NewFangak","New Fangak"),
    label = str_replace(label, "OldFangak","Old Fangak"),
    label = str_replace(label, "YusufBatil RC","Yusuf-Batil RC"),
    label = str_replace(label, "AjuongThok RC","Ajuong-Thok RC"),
  )

# add gps points to market functionality data
mfs_sf <- left_join(mfs,as.data.frame(points.sf),by=c('state','county','location')) %>% st_sf(sf_column_name = 'geometry') %>%
  mutate(
    lab_mfs = paste(label, as.character(round(mfs_score,1)), sep = "\n"),
    lab_access = paste(label, as.character(round(mfs_accessibility_pillar_score,1)), sep = "\n"),
    lab_avail = paste(label, as.character(round(mfs_availability_pillar_score,1)), sep = "\n"),
    lab_afford = paste(label, as.character(round(mfs_affordability_pillar_score,1)), sep = "\n"),
    lab_resil = paste(label, as.character(round(mfs_resilience_pillar_score,1)), sep = "\n"),
    lab_infra = paste(label, as.character(round(mfs_infrastructure_pillar_score,1)), sep = "\n")
  )

#clip base layer and tidy labels
ssd_base_clip <- st_crop(ssd_base,c(st_bbox(ssd_states)[1:2] - 0.5, st_bbox(ssd_states)[3:4] + 0.5)) %>%
  mutate(
    map_lab = ifelse(is.na(map_lab),'Ilemi',map_lab),
    lab_wrap = str_wrap(map_lab, 20)
  )

#create dataset of country centroids for mapping
country_cent <- st_centroid(ssd_base_clip) %>%
  select(adm0_name,map_lab,lab_wrap,iso3,disp_area,geom)%>%
  filter(map_lab != 'South Sudan') %>%
  mutate(
    X = st_coordinates(.)[,1],
    Y = st_coordinates(.)[,2],
    Location = recode(map_lab, 'Central African Republic' = 'CAR', 'Democratic Republic of the Congo' = 'DRC')
  )


#create rectangle polygon to cover border in final plots
bbox_border <- st_as_sf(st_as_sfc(st_bbox(ssd_base_clip), crs = st_crs(4326)))

####---- MAPS

#create base plot for maps
jmmi_base<-ggplot() + theme_void() +
  geom_sf(data = ssd_base_clip, fill = "#E3E5E6", linewidth = 0.6) +
  geom_sf(data = filter(ssd_base_clip, disp_area == 'YES'), fill = "#F5F5F5", linewidth = 0.6) +
  geom_sf_text(data = country_cent, aes(label = lab_wrap, fontface = "bold"), size = 3) +
  geom_sf(data = ssd_counties, fill = '#FFFFFF', linewidth = 0.2, colour = "#E3E5E6")+
  geom_sf(data = ssd_states, fill = NA, linewidth = 0.5, colour = "#B3B3B3") +
  geom_sf(data = ssd_base_clip, fill = NA, linewidth = 0.6) +
  ggsn::scalebar(data = ssd_states, location = 'bottomleft', dist = 100, dist_unit = 'km', transform = T, model = 'WGS84', st.size = 2, height = 0.01, border.size = 0.4,
                                                                         anchor = data.frame(x = st_bbox(ssd_counties)[1],
                                                                                             y = st_bbox(ssd_counties)[2] - 0.1)) +
  geom_sf(data = bbox_border, fill = NA, color = "#E3E5E6", linewidth = 1.8)


######### MFS map only ---------------------------------

mfs_map <- jmmi_base +
  theme(legend.position = c(0.148,0.86),
        legend.title = element_text(size=8, family = 'Segoe UI'), legend.text=element_text(size=6, family = 'Segoe UI'), legend.key.height = unit(0.025, 'npc')) +
  geom_point(data = mfs_sf, aes(x = X, y = Y, fill = mfs_score), size = 3.5, shape = 21, colour = ifelse(is.na(mfs_sf$mfs_score), '#505050','#FFFFFF')) +
  geom_text_repel(data = mfs_sf, aes(x = X, y = Y, label = lab_mfs), size = 2.3, max.overlaps = 20, lineheight = 1, force_pull = 5, point.padding = 0.5,
                  show.legend = FALSE, bg.color = "white", bg.r = 0.2) +
  scale_fill_gradient2(low = '#991C21',
                       mid = '#FFFFBF',
                       high = '#005F60',
                       midpoint = 50,
                       limits = c(0,100),
                       na.value = NA) +
  labs(fill = 'Market Functionality Score')

ggsave("maps/output/MFS_map.jpg", plot = mfs_map, width = 10.6, height = 6.8)


######## MFS pillar maps -------------------------

# for loop to create maps for each pillar of the MFS

for (pillar in 4:8) {
  #create variable with max weighted pillar score
  pillar_max <- ifelse(pillar == 4,25,
                       ifelse(pillar == 5, 30,
                              ifelse(pillar == 6, 15,
                                     ifelse(pillar == 7, 20,
                                            10))))

  # create map
  map <- jmmi_base +
    theme(legend.position = c(0.23,0.82),
          legend.title = element_text(size=10, family = 'Segoe UI'), legend.text=element_text(size=8, family = 'Segoe UI'), legend.key.height = unit(0.03, 'npc')) +
    geom_point(data = mfs_sf, aes(x = X, y = Y, fill = as.numeric(unlist(st_drop_geometry(mfs_sf[,pillar])))), size = 3.5, shape = 21, colour = ifelse(is.na(as.numeric(unlist(st_drop_geometry(mfs_sf[,pillar])))), '#505050','#FFFFFF')) +
    geom_text_repel(data = mfs_sf, aes(x = X, y = Y, label = unlist(st_drop_geometry(mfs_sf[,pillar+11]))), size = 2.3, max.overlaps = 20, lineheight = 1, force_pull = 5, point.padding = 0.5,
                    show.legend = FALSE, bg.color = "white", bg.r = 0.2) +
    scale_fill_gradient2(low = '#991C21',
                         mid = '#FFFFBF',
                         high = '#005F60',
                         midpoint = pillar_max/2,
                         limits = c(0,pillar_max),
                         na.value = NA) +
    labs(fill = paste0(str_to_title(unlist(strsplit(colnames(st_drop_geometry(mfs_sf[,pillar])),'_'))[2]),' Pillar of Market Functionality Score'))

  # save map
  ggsave(paste0('maps/output/MFS_map_',unlist(strsplit(colnames(st_drop_geometry(mfs_sf[,pillar])),'_'))[2],'.jpg'), plot = map, width = 10.6, height = 6.8)


}

