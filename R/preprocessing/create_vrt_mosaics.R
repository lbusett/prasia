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
mosaics_folder <- "/home/lbusetto/nas-s4a/nr_working/shared/PhenoRice/Asia/Data/mosaics/by_year/"
patterns <- c("sos", "eos", "pos", "cumevi", "veglgt", "totlgt", "nseas")

for (pattern in patterns) {
  print(pattern)
  create_vrt_mosaics(mosaics_folder, pattern)
}

vrts    <- list.files(mosaics_folder, pattern = ".vrt", full.names = T)
outfold <- "/home/lbusetto/nas-s4a/nr_working/shared/PhenoRice/Asia/Data/mosaics/ordered"
dir.create(outfold)

out <- foreach(vrt_n = seq_len(vrts),
               .combine      = "c",
               .verbose      = FALSE,
               .packages     = c("gdalUtils") %dopar% {

                 outfile <- file.path(outfold, paste0(basename(tools::file_path_sans_ext(vrts[vrt_n])), ".tif"))
                 gdalUtils::gdalbuildvrt(vrts[vrt_n], outfile, separate = TRUE)
                 return (paste0(outfile, " completed ! "))
               }
)




