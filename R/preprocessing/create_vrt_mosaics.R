create_vrt_mosaics <- function (mosaics_folder, pattern) {

  in_files <- list.files(mosaics_folder, pattern = pattern, full.names = T)
  names <- tibble::tibble(fullname = in_sos_files,
                          shortname = basename(tools::file_path_sans_ext(in_sos_files)))
  split_names <- stringr::str_split_fixed(names$shortname, "_", 3)

  names <- cbind(names, split_names, or_order = seq(1:length(names$fullname))) %>%
    `colnames<-`(c("fullname", "basename", "var", "season", "year", "order")) %>%
    dplyr::arrange(year, season)

  ordered_files <- names$fullname

  vrt_file <- file.path(mosaics_folder, paste0(pattern, "_ordered.vrt"))
  gdalUtils::gdalbuildvrt(ordered_files, vrt_file, separate = T)

}

library(tidyverse)
library(gdalUtils)
mosaics_folder <- "/home/lbusetto/nas-s4a/nr_working/shared/PhenoRice/Asia/Data/mosaics/by_year/"
patterns <- c("sos", "eos", "pos", "cumevi", "veglgt", "totlgt", "nseas")

for (pattern in patterns) {
  print(pattern)
  create_vrt_mosaics(mosaics_folder, pattern)
}

