create_vrt_mosaics <- function(mosaics_folder, pattern) {

  in_files <- list.files(mosaics_folder, pattern = pattern, full.names = T)[1:3]
  names <- tibble::tibble(fullname = in_files,
                          shortname = basename(tools::file_path_sans_ext(in_files)))
  split_names <- stringr::str_split_fixed(names$shortname, "_", 3)
  names <- cbind(names, split_names, or_order = seq(1:length(names$fullname))) %>%
    `colnames<-`(c("fullname", "basename", "var", "season", "year", "order")) %>%
    dplyr::arrange(year, season)

  ordered_files <- names$fullname

  vrt_file <- file.path(mosaics_folder, paste0(pattern, "_ordered.vrt"))
  gdalUtils::gdalbuildvrt(ordered_files, vrt_file, separate = T, overwrite = T)

}

library(tidyverse)
library(gdalUtils)
library(foreach)
library(parallel)
library(doSNOW)
mosaics_folder <- "/home/lbusetto/nas-s4a/nr_working/shared/PhenoRice/Asia/Data/mosaics/by_year/"
patterns <- c("sos", "eos", "pos", "cumevi", "veglgt", "totlgt", "nseas")

for (pattern in patterns) {
  print(pattern)
  create_vrt_mosaics(mosaics_folder, pattern)
}

vrts    <- list.files(mosaics_folder, pattern = ".vrt", full.names = T)
outfold <- "/home/lbusetto/nas-s4a/nr_working/shared/PhenoRice/Asia/Data/mosaics/ordered"
dir.create(outfold)

ncores <- 8
clust <- parallel::makeCluster(ncores, outfile = "")
doSNOW::registerDoSNOW(clust)

out <- foreach(vrt_n = seq_len(vrts),
               .combine      = "c",
               .verbose      = FALSE,
               .packages     = c("gdalUtils"),
               .verbose      = TRUE) %dopar% {

                 message("create_vrt_tiff --> processing: ", basename(vrts[vrt_n]))
                 parameter <- basename(tools::file_path_sans_ext(vrts[vrt_n]))
                 outfile <- file.path(outfold, paste0(basename(tools::file_path_sans_ext(vrts[vrt_n])), ".tif"))
                 if (parameter %in% c("sos_ordered", "eos_ordered", "pos_ordered")) {
                   ot <- "Int16"
                 }
                 if (parameter %in% c("cumevi_ordered")) {
                   ot <- "Int32"
                 }
                 if (parameter %in% c("totlgt_ordered", "veglgt_ordered", "nseas_ordered")) {
                   ot <- "Byte"
                 }

                 gdalUtils::gdal_translate(vrts[vrt_n],
                                           outfile,
                                           ot        = "Int16",
                                           of        = "GTiff",
                                           co        = "COMPRESS=DEFLATE",
                                           separate  = TRUE,
                                           verbose   = TRUE,
                                           overwrite = TRUE)

                 return(paste0(outfile, " completed ! "))
               }
parallel::stopCluster(clust)



