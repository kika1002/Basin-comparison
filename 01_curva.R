
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, tidyverse, sf, tmaptools, magrittr, Hmisc, gsubfn)

# Initial setup -----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)
utm <- '+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

# Functions to use --------------------------------------------------------
mark_class <- function(x){
  # x <- '(100,150]'
  x <- as.character(x)
  interv <- strapply(x, "[[:digit:].]+", as.numeric, simplify = TRUE)
  valor <- mean(c(interv[1, ], interv[2, ]))
  return(valor)
}
seqil <- function(from, to, by, include.last = TRUE) {
  x <- do.call(seq.default, list(from, to, by))
  if(include.last) c(x, to) else x
}
makeHipsometric <- function(altitude){
  dem_df <- rasterToPoints(altitude) %>% 
    as.data.frame() %>% 
    setNames(c('x', 'y', 'value')) %>% 
    as_tibble()
  maximo <- max(dem_df$value)
  minimo <- min(dem_df$value)
  dem_rcl <- dem_df %>%
    mutate(reclass = raster::cut(value, breaks = seqil(from = minimo - 1, to = maximo, by = 50), dig.lab = 10)) 
  hipsometric <- dem_rcl %>% 
    mutate(mark_class = map(.x = reclass, .f = mark_class)) 
  mrk_cls <- hipsometric %>%
    dplyr::select(mark_class) %>% 
    unnest() %>% 
    pull() 
  hipsometric <- hipsometric %>% 
    mutate(mark_class = mrk_cls) %>% 
    dplyr::group_by(mark_class) %>% 
    dplyr::summarize(count = n()) %>% 
    dplyr::ungroup() %>% 
    arrange(desc(mark_class))
  hipsometric <- hipsometric %>% 
    mutate(area_ha = count * size / 10000,
           area_total = (area_ha / sum(area_ha)) * 100,
           acum = cumsum(area_total)) # size = tamano del pixel (lado por lado del pixel)
  print('To make the graph')
  gg <- ggplot(hipsometric) +
    geom_line(aes(x = acum, y = mark_class)) +
    labs(x = 'Area below the indicated elevation (%)', 
         y = 'Elevation (m.a.s.l.)') +
    ggtitle('') + 
    # theme_bw() +
    theme(axis.text = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.title.x = element_text(size = 14)) #+
    # scale_y_continuous(limits = c(100, 2500),
    #                    breaks = seq(100, 2500, 200))
  return(gg)
}

# Load data ---------------------------------------------------------------
dem <- raster('dem/dem_bsns.tif') * 1
bsn <- shapefile('shp/basins.shp')
dem <- projectRaster(dem, crs = crs(bsn))

# Preparing the rasters dem
dem_tua <- raster::crop(dem, bsn[bsn@data$name == 'Tua',]) %>% 
  raster::mask(., bsn[bsn@data$name == 'Tua',])
dem_cby <- raster::crop(dem, bsn[bsn@data$name == 'Cabuyaro',]) %>% 
  raster::mask(., bsn[bsn@data$name == 'Cabuyaro',])

max(dem_tua[], na.rm = T)
max(dem_cby[], na.rm = T)

min(dem_tua[], na.rm = T)
min(dem_cby[], na.rm = T)

# DEM Resolution ----------------------------------------------------------
dem_prj <- projectRaster(dem, crs = crs(utm))
size <- res(dem_prj)[[1]] * res(dem_prj)[[2]] # 92.6 el tamano de un lado del pixel
par(mfrow = c(1, 2))
plot(dem_tua)
plot(dem_cby)

# Apply the functions -----------------------------------------------------
hps_cby <- makeHipsometric(altitude = dem_cby)
hps_tua <- makeHipsometric(altitude = dem_tua)

max(dem_tua[], na.rm = TRUE)

ggsave(plot = hps_cby, 
       filename = 'png/hipsometric_cabuyaro.png', 
       units = 'cm',
       height = 11, 
       width = 14,
       dpi = 300)

ggsave(plot = hps_tua, 
       filename = 'png/hipsometric_tua.png', 
       units = 'cm',
       height = 11, 
       width = 14,
       dpi = 300)

