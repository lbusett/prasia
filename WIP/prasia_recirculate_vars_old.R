library(sprawl)
library(sf)



reorganize_mosaics <- function(mosaics_folder) {

  files_in <- list.files(mosaics_folder, pattern = ".tif")

  #   ____________________________________________________________________________
  #   create full raster stacks and cahnge band names                         ####

  patterns    <- c("n*", "s*", "e*", "p*", "v*", "l*", "c*")
  newpatterns <- c("nseas", "sos_", "eos_", "pos_", "veglgt_", "totlgt_", "cumevi_")

  for (pat in seq_along(patterns)) {
    old_names <- list.files(mosaics_folder, pattern = glob2rx(patterns[pat]), full.names = F)
    new_names <- gsub(strsplit(patterns[pat],"*")[[1]][1], newpatterns[pat], old_names)
    file.rename(file.path(mosaics_folder, old_names), file.path(mosaics_folder, new_names))
  }




  file.rename()

  in_rast_nseas  <- raster::stack(list.files(mosaics_folder, pattern = glob2rx("n_*"), full.names = T))
  in_rast_sos    <- raster::stack(list.files(mosaics_folder, pattern = glob2rx("s*"), full.names = T))
  in_rast_eos    <- raster::stack(list.files(mosaics_folder, pattern = glob2rx("e*"), full.names = T))
  in_rast_pos    <- raster::stack(list.files(mosaics_folder, pattern = glob2rx("p*"), full.names = T))
  in_rast_veglgt <- raster::stack(list.files(mosaics_folder, pattern = glob2rx("v*"), full.names = T))
  in_rast_totlgt <- raster::stack(list.files(mosaics_folder, pattern = glob2rx("l*"), full.names = T))
  in_rast_cumevi <- raster::stack(list.files(mosaics_folder, pattern = glob2rx("c*"), full.names = T))

  names(in_rast_nseas)  <- gsub("n_", "nseas_", names(in_rast_nseas))
  names(in_rast_sos)    <- gsub("s", "sos_", names(in_rast_sos))
  names(in_rast_pos)    <- gsub("p", "pos_", names(in_rast_pos))
  names(in_rast_eos)    <- gsub("e", "eos_", names(in_rast_eos))
  names(in_rast_veglgt) <- gsub("v", "lgtveg_", names(in_rast_veglgt))
  names(in_rast_totlgt) <- gsub("_all", "_a", names(in_rast_totlgt))
  names(in_rast_totlgt) <- gsub("l", "lgttot_", names(in_rast_totlgt))
  names(in_rast_cumevi) <- gsub("c", "cumevi_", names(in_rast_cumevi))

  years = seq(2003,2016)
  for (y_ind in seq_along(years)) {
    yy = years[y_ind]
    names(in_rast_nseas)  <- gsub(paste0("_all.",y_ind, "$"), paste0("_", yy), names(in_rast_nseas ))
    names(in_rast_sos)    <- gsub(paste0("_all.",y_ind, "$"), paste0("_", yy), names(in_rast_sos))
    names(in_rast_eos)    <- gsub(paste0("_all.",y_ind, "$"), paste0("_", yy), names(in_rast_eos))
    names(in_rast_pos)    <- gsub(paste0("_all.",y_ind, "$"), paste0("_", yy), names(in_rast_pos))
    names(in_rast_veglgt) <- gsub(paste0("_all.",y_ind, "$"), paste0("_", yy), names(in_rast_veglgt))
    names(in_rast_totlgt) <- gsub(paste0("_a.",y_ind, "$"), paste0("_", yy), names(in_rast_totlgt))
    names(in_rast_cumevi) <- gsub(paste0("_all.",y_ind, "$"), paste0("_", yy), names(in_rast_cumevi))
  }

}

mosaics_folder <- "/home/lb/my_data/prasia/mosaics/by_year"
reorganize_mosaics(mosaics_folder)

test_out <- file.path("/home/lb/my_data/prasia/mosaics/tests/test1.tif")
dir.create(dirname(test_out))
test <- raster::writeRaster(in_rast_nseas[[1:5]], test_out, options = c("COMPRESS=DEFLATE"))

test_vrt <- tempfile(fileext = ".vrt")
gdalUtils::gdalbuildvrt(in_rast_nseas[[1:5]], test_vrt)


#   ____________________________________________________________________________
#   get correspondence between names, var, seas, and year. Create a data frame ####
#   with an associated column that will tell us in which order to pick the
#   bands form the original stack

names <- tibble::tibble(name = names(in_rast_sos))
split_names <- stringr::str_split_fixed(names$name, "_", 3)
names <- cbind(names, split_names, or_order = seq(1:length(names$name))) %>%
  `colnames<-`(c("name", "var", "season", "year", "order")) %>%
 dplyr::arrange(Year, Season)

r <- raster::stack(in_rast_sos)

for (band in 1:raster::nlayers(in_rast_sos)) {

  pippo[[band]] <- in_rast_sos[[names$order[band]]]

}

#   ____________________________________________________________________________
#   order in which we have to pick the bands from the stack is in "names$order"   ####


#   ____________________________________________________________________________
#   Crop on an area of interest                                             ####
#

country    <- "PHL"
boundaries <- get_boundaries(country, level = 0)
boundaries <- st_as_sf(boundaries)

dir.create("/home/lb/my_data/prasia/PHL")

t1 <- Sys.time()
sos <- mask_rast(in_rast_sos, boundaries, crop = TRUE, to_file = TRUE,
                  out_rast = "/home/lb/my_data/prasia/PHL/SOS_PHL.tif")
names(sos) <-
Sys.time() - t1
#   ____________________________________________________________________________
#   Decircularize dates rasters                                             ####
#

