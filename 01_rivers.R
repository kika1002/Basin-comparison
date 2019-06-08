
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, tidyverse, sf, foreign)

# Initial setup 
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data
slpe <- raster('dem/slope_prc.tif') 
dem <- raster('dem/dem_basins_prj.tif')





rvrs <- shapefile('shp/drenaje_doble.shp')
nmes <- unique(rvrs$NOMBRE_GEO) %>% 
  as.character() %>% 
  na.omit() %>% 
  as.character()

# Indice de Graveolus -----------------------------------------------------
bsn <- st_read('../_shp/_base/basins_prj.shp')

# Cabuyaro
prm.cab <- bsn %>% 
  filter(name == 'Cabuyaro') %>% 
  pull(7)
are.cab <- bsn %>%
  filter(name == 'Cabuyaro') %>% 
  pull(6)
are.cab <- are.cab / 100

grv.cab <- prm.cab / (2 * (sqrt(pi * are.cab)))
  
# Tua
prm.tua <- bsn %>% 
  filter(name == 'Tua') %>% 
  pull(7)
are.tua <- bsn %>%
  filter(name == 'Tua') %>% 
  pull(6)
are.tua <- are.tua / 100

grv.tua <- prm.tua / (2 * (sqrt(pi * are.tua)))

# Pendiente media por cuenca ----------------------------------------------
slpe[which(slpe[] > 100)] <- NA

# Cabuyaro
slpe.cab <- raster::mask(slpe, as(bsn %>% filter(name == 'Cabuyaro'), 'Spatial'))
slpe.cab <- raster::crop(slpe, as(bsn %>% filter(name == 'Cabuyaro'), 'Spatial'))
mean(slpe.cab[], na.rm = TRUE)

# Tua
slpe.tua <- raster::mask(slpe, as(bsn %>% filter(name == 'Tua'), 'Spatial'))
slpe.tua <- raster::crop(slpe, as(bsn %>% filter(name == 'Tua'), 'Spatial'))
mean(slpe.tua[], na.rm = TRUE)

# Pendiente del cauce principal -------------------------------------------
cab <- rvrs %>% filter(NOMBRE_GEO == 'RÍO CABUYARITO')
tua <- rvrs %>% filter(NOMBRE_GEO == 'RÍO TUA')
cab <- as(cab, 'Spatial')
tua <- as(tua, 'Spatial')

dem.cab <- raster::crop(dem, cab) %>% raster::mask(., cab)

cab.slp.val <- ((275-176) / 207900) * 100 
tua.slp.val <- ((1053-242) / 122000) * 100 

# Densidad de drenaje ----------------------------------------------------- longitud cause principal (kms) / area cuenca (kms2)
dns.drenaje.cab <- (155.3 + 193.1) / 932.838 
dns.drenaje.tua <- (96.1 + 118.2) / 291.888

dns.drenaje.cab <- (155.3) / 932.838 
dns.drenaje.tua <- (96.1) / 291.888

# Tiempo de concentración ------------------------------------------------- punto mas alto ahsta lelgar al punto bajo el flujo
cnc.cab <- ((0.870 * (193.1 ^ 3)) / (275 - 176)) ^ 0.385 # Resultado horas
cnc.tua <- ((0.870 * (118.2 ^ 3)) / (1053 - 242)) ^ 0.385 # Resultado horas

# Landform ----------------------------------------------------------------
lnd.cab <- read.dbf('../_tif/_dem/landform_cab_ok.tif.vat.dbf') %>% 
  dplyr::select(category, has, porcen) %>% 
  rename(has_cab = has, porcen_cab = porcen)
lnd.tua <- read.dbf('../_tif/_dem/landform_tua_ok.tif.vat.dbf') %>% 
  dplyr::select(category, has, porcen) %>% 
  rename(has_tua = has, porcen_tua = porcen)

lnd <- inner_join(lnd.cab, lnd.tua, by = 'category') %>% 
  dplyr::select(category, has_cab, has_tua, porcen_cab, porcen_tua) %>% 
  mutate(has_cab = round(has_cab, 1),
         has_tua = round(has_tua, 1),
         porcen_cab = round(porcen_cab, 1),
         porcen_tua = round(porcen_tua, 1))

write.csv(lnd, '../_tbl/landforms_has_basins.csv', row.names = TRUE)
