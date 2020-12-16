library(tidyverse)
library(landscapemetrics)
library(landscapetools)
library(raster)
library(stars)
library(sf)
library(tmap)
library(RColorBrewer)
library(rgdal)
library(here)

here()
# load in shapefile
gi <- readOGR(here("data","Laso-2020","Galapagos_Agroecosystems_LandCover2018.shp"))
gi %>% summary()
# check projection
gi %>% crs()
# change to sf object
gi_sf <- gi %>% 
  st_as_sf()
# filter to only Santa Cruz  Island
sci <- gi_sf %>% 
  filter(Island =="Santa Cruz")
# check landcover classes of original polygon 
as_vector(sci$Level_5) -> landcover
landcover
# (Level 5 has the 20 land cover categories)
# Rasterize the polygon file according to Level 5 
sci_rast <- st_rasterize(sci["Level_5"])
# plot to check
plot(sci_rast)
# create new land cover column according to ID
sci_rast <- sci_rast  %>% 
  mutate(landcover = case_when(ID == 1 ~ "Bare Ground",
                               ID == 2 ~ "Built Environment",
                               ID == 3 ~ "Cedrela-Cedar",
                               ID == 4 ~ "Cinchona-Quinine",
                               ID == 5 ~ "Coffea-Coffee",
                               ID == 6 ~ "Cultivated Grass",
                               ID == 7 ~ "Deciduous Forest",
                               ID == 8 ~ "Evergreen Forest and Shrubland", 
                               ID == 9 ~ "Evergreen Seasonal Forest and Shrubland",
                               ID == 10 ~ "Freshwater",
                               ID == 11 ~ "Humid Tallgrass",
                               ID == 12 ~ "Mixed Forest",
                               ID == 13 ~ "Pennisetum-Elephant Grass",
                               ID == 14 ~ "Other Permanent Crops",
                               ID == 15 ~ "Pioneer",
                               ID == 16 ~ "Psidium-Guava",
                               ID == 17 ~ "Rubus-Blackberry",
                               ID == 18 ~ "Silvopasture",
                               ID == 19 ~ "Transitory Crops",
                               ID == NA ~ "Not Available"))
# make dataframe for other analyses
sci_rast_df <- sci_rast %>% 
  as_tibble() %>% 
  mutate(landcover = case_when(ID == 1 ~ "Bare Ground",
                               ID == 2 ~ "Built Environment",
                               ID == 3 ~ "Cedrela-Cedar",
                               ID == 4 ~ "Cinchona-Quinine",
                               ID == 5 ~ "Coffea-Coffee",
                               ID == 6 ~ "Cultivated Grass",
                               ID == 7 ~ "Deciduous Forest",
                               ID == 8 ~ "Evergreen Forest and Shrubland", 
                               ID == 9 ~ "Evergreen Seasonal Forest and Shrubland",
                               ID == 10 ~ "Freshwater",
                               ID == 11 ~ "Humid Tallgrass",
                               ID == 12 ~ "Mixed Forest",
                               ID == 13 ~ "Pennisetum-Elephant Grass",
                               ID == 14 ~ "Other Permanent Crops",
                               ID == 15 ~ "Pioneer",
                               ID == 16 ~ "Psidium-Guava",
                               ID == 17 ~ "Rubus-Blackberry",
                               ID == 18 ~ "Silvopasture",
                               ID == 19 ~ "Transitory Crops",
                               ID == NA ~ "Not Available"))
# check projection
sci_rast %>% st_crs()

# set projections, bounding box, 
# and custom randomized color palette
myCRS <- st_crs(sci_rast)
myBox <- st_bbox(sci_rast)
myPal <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99',
           '#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a',
           '#ffff99','#b15928','#8dd3c7','#ffffb3','#bebada',
           '#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5')

# geom_stars plot
ggplot() +
  geom_stars(data = sci_rast, aes(x, y, fill = landcover)) +
  coord_sf(datum = st_crs(myCRS)) +
  scale_fill_discrete(name = "Landcover") +
  guides(fill = guide_legend(ncol = 4)) +
  theme(legend.position = "bottom") +
  labs(title = "Santa Cruz Island: Raster",
       source = "Laso et al 2020",
       x = "Easting",
       y = "Northing")
# tmap raster
# tm_shape(sci_rast) +
#   tm_raster("landcover")
# # tmap polgyon
# tm_shape(sci) +
#   tm_borders(col = "black")


# polygon map: caution - very slow
# ggplot() +
#   geom_sf(data = sci, aes(fill = Level_5)) +
#   coord_sf(datum = st_crs(myCRS)) +
#   scale_fill_discrete(name = "Landcover") +
#   guides(fill = guide_legend(ncol = 4)) +
#   theme(legend.position = "bottom") +
#   labs(title = "Santa Cruz Island: Polygon",
#        subtitle = "Laso et al. 2020",
#        x = "Easting",
#        y = "Northing")

# ggplot map: caution - way too slow!
# ggplot() +
#   geom_raster(data = sci_rast_df, aes(x, y, fill = as_factor(landcover))) +
#   scale_fill_manual(name = "Landcover") +
#   guides(fill = guide_legend(ncol = 4)) +
#   theme(legend.position = "bottom") +
#   labs(title = "Santa Cruz Island: Raster",
#        subtitle = "Laso et al. 2020",
#        x = "Easting",
#        y = "Northing")


# Tortoises ---------------------------------------------------------------
# load in movebank tortoise file
gt <- read.csv(here("data","gt_2018.csv"))
# clean column names
gt %>% 
  janitor::clean_names() -> gt
# data.frame to tibble
gt %>% 
  tibble() -> gt
# assigning tortoise name/id from ref table (not included)
# these tortoises were preset on Santa Cruz Island
torts_sci <- c("Alison", "Andrea", "Carolina", "Chrissie", "Connor",
               "Cuthbert", "Danny", "Delmira", "Dennis", "Erbert", 
               "Freddy V", "Fredy", "George", "Graciella", "Harry",
               "Helber", "Herbert", "Inoho", "Jumbo", "Jumbo_2", 
               "Karla", "Karlitos", "Kiana", "Kitty","Lola", "Lolo", "Lore", 
               "Lucy", "Mandy", "Maria", "Marilyn", "Mariposa", "Mary", 
               "Melina", "Narcissa","Nigel", "Nigrita", "Patty", "Plug", 
               "Samy", "Sandra", "Sebastian", "Sepp", "Sharon", "Sidney", 
               "Sir David", "Smiffy", "Sonia", "Spotty",
               "Susan", "Tanya", "Toots", "Veronica", "Wacho", "Wilman", "Yvonne")
# rename column
gt <- gt %>% 
  rename(id = individual_local_identifier)
# filtr for specific tortoises from vector
gt_sci <- gt %>% 
  filter(id %in% torts_sci)
# drop data without geoference points
gt_sci <- gt_sci %>% 
  drop_na(utm_easting, utm_northing)
# create new columns: life stage, study site, sex
gt_sci <- gt_sci %>% 
  mutate(life_stage = "adult",
         study_site = "Isla Santa Cruz east (Cerro Fatal)",
         sex = if_else(id == "Dennis" | id == "Harry" | id == "Herbert", "male", "female"))
# count tortoises in dataset so far
gt_sci %>% 
  count(id, sex, study_site, life_stage)

# gt points into sf spatial object
gt_sci_loc <- gt_sci %>% 
  st_as_sf(coords = c("utm_easting", "utm_northing"),  remove = FALSE)
# assign projection and bounding box
gt_sci_loc <-  gt_sci_loc %>% 
  st_set_crs(myCRS) %>% 
  st_crop(myBox)

# plot gt locations + raster file
ggplot() +
  geom_stars(data = sci_rast, aes(x, y, fill = landcover)) +
  geom_sf(data = gt_sci_loc, aes(shape = id), 
          color = "red", alpha = 0.2) +
  coord_sf(datum = st_crs(myCRS)) +
  scale_fill_discrete(name = "Landcover") +
  guides(fill = guide_legend(ncol = 4)) +
  theme(legend.position = "bottom") +
  labs(title = "2018 Galápagos Tortoise locations on Santa Cruz Island: Raster",
       subtitle = "Laso et al 2020 & Bastille-Rousseau et al 2016",
       x = "Easting",
       y = "Northing")

# landscapemetrics --------------------------------------------------------
# create table of all metrics in Rstudio viewer
list_lsm() %>% gt::gt()

# checks our stars object
# seems like it works
sci_rast %>% check_landscape()

# we can describe a composition of the landscape 
# to explore patch structure of the landscape,
l_area_mn <- sci_rast %>% lsm_l_area_mn() # mean of all patches in the landscape
l_area_mn
l_area_sd <- sci_rast %>% lsm_l_area_sd()
l_area_sd
l_lsm_ta <- sci_rast %>% lsm_l_ta() # total (class) area sums the area of all patches in the landscape
l_lsm_ta 

sci_rast %>% 
  lsm_p_area %>% 
  ggplot(aes(x = class)) +
  geom_histogram() 

# plot landscape + landscape with labeled patches
sci_rast %>% show_patches()
sci_rast %>% show_patches(class = "all")
# plot core areas
sci_rast %>% show_cores(class = "all")
# fill patch according to area
sci_rast %>% show_lsm(what = "lsm_p_area", 
         class = "global", label_lsm = TRUE)

# metrics - correlation
l_metrics <-sci_rast %>% calculate_lsm(what = "landscape")
p_metrics <- sci_rast %>% calculate_lsm(what = "patch")
c_metrics <- sci_rast %>% calculate_lsm(what = "class")

l_metrics %>% show_correlation(method = "pearson")
p_metrics %>% show_correlation(method = "pearson")
c_metrics %>% show_correlation(method = "pearson")

# summarizing raster
sci_rast %>% 
  as_tibble() %>% 
  count(landcover, sort = TRUE) %>% 
  mutate(landcover = fct_reorder(landcover, n)) %>% 
  ggplot(aes(x = landcover, y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Distribution of Landcover Classes on Santa Cruz Island",
       subtitle = "source: Laso et al. 2020",
       x = "Landcover",
       y = "")

# lanscape level metrics: core area
sci_rast %>% lsm_l_tca()
sci_rast %>% lsm_l_core_mn()
sci_rast %>% lsm_l_core_sd()
sci_rast %>% lsm_l_cai_mn()
sci_rast %>% lsm_l_ai()


# get patches
sci_rast %>% 
  get_patches(class = "all")

# patch level metrics
sci_rast %>% 
  lsm_p_ncore() %>% 
  ggplot(aes(x = class, y = value)) +
  geom_point()
  

# number of core area patches in raster
sci_rast %>% 
  lsm_p_ncore %>% 
  mutate(class = as_factor(class)) %>% 
  ggplot(aes(x = class, y = value)) +
  geom_col()

# extracting metrics in combination with gt locations
# look at how many tortoises picked each class
sci_rast %>% 
  extract_lsm(y = gt_sci_loc, what = "lsm_p_core",
              extract_id = as_vector(gt_sci_loc$id)) %>% 
  mutate(extract_id = as_factor(extract_id),
         class = as_factor(class)) %>% 
  mutate(landcover = case_when(class == 1 ~ "Bare Ground",
                               class == 2 ~ "Built Environment",
                               class == 3 ~ "Cedrela-Cedar",
                               class == 4 ~ "Cinchona-Quinine",
                               class == 5 ~ "Coffea-Coffee",
                               class == 6 ~ "Cultivated Grass",
                               class == 7 ~ "Deciduous Forest",
                               class == 8 ~ "Evergreen Forest and Shrubland", 
                               class == 9 ~ "Evergreen Seasonal Forest and Shrubland",
                               class == 10 ~ "Freshwater",
                               class == 11 ~ "Humid Tallgrass",
                               class == 12 ~ "Mixed Forest",
                               class == 13 ~ "Pennisetum-Elephant Grass",
                               class == 14 ~ "Other Permanent Crops",
                               class == 15 ~ "Pioneer",
                               class == 16 ~ "Psidium-Guava",
                               class == 17 ~ "Rubus-Blackberry",
                               class == 18 ~ "Silvopasture",
                               class == 19 ~ "Transitory Crops",
                               class == NA ~ "Not Available")) %>% 
  ggplot(aes(x = landcover, y = value, fill = extract_id)) +
  geeom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Galápagos Tortoises",
       subtitle = "Core Area")

# core areas used by each tortoise
sci_rast %>% 
  extract_lsm(y = gt_sci_loc, what = "lsm_p_core",
              extract_id = as_vector(gt_sci_loc$id)) %>% 
  mutate(landcover = case_when(class == 1 ~ "Bare Ground",
                               class == 2 ~ "Built Environment",
                               class == 3 ~ "Cedrela-Cedar",
                               class == 4 ~ "Cinchona-Quinine",
                               class == 5 ~ "Coffea-Coffee",
                               class == 6 ~ "Cultivated Grass",
                               class == 7 ~ "Deciduous Forest",
                               class == 8 ~ "Evergreen Forest and Shrubland", 
                               class == 9 ~ "Evergreen Seasonal Forest and Shrubland",
                               class == 10 ~ "Freshwater",
                               class == 11 ~ "Humid Tallgrass",
                               class == 12 ~ "Mixed Forest",
                               class == 13 ~ "Pennisetum-Elephant Grass",
                               class == 14 ~ "Other Permanent Crops",
                               class == 15 ~ "Pioneer",
                               class == 16 ~ "Psidium-Guava",
                               class == 17 ~ "Rubus-Blackberry",
                               class == 18 ~ "Silvopasture",
                               class == 19 ~ "Transitory Crops",
                               class == NA ~ "Not Available")) %>% 
  mutate(class = as_factor(class)) %>% 
  ggplot(aes(x = landcover, y = value, fill = extract_id)) +
  geom_col() +
  scale_fill_discrete(name = "Tortoise ID") +
  labs(title = "Core Area used by Tortoise",
       x = "Landcover type",
       y = "Count selected") +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1))



