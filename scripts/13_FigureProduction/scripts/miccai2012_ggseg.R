library(tidyverse)
library(ggseg)
devtools::load_all(".")

mic_atlas <- load("/Users/butellyn/Documents/ggsegExtra/data-raw/MiccaiOasis/mic_atlases.Rda")
miccai <- mic_atlas %>%
  mutate(label = paste(hemi, gsub(" ", "_", aparc), sep = "_"),
         hemi = case_when(hemi == "lh" ~ "left",
                          hemi == "rh" ~ "right"),
         side = ifelse(id %in% c(1:17, 2000:2009), "lateral", "medial"),
         miccai = ifelse(grepl("wall", miccai), NA, miccai),
         atlas = "miccai") %>%
  rename(area = miccai,
         cluster = miccai2) %>%
  select(-group, -meas, -piece) %>%
  mutate(
    long = long - min(long),
    lat = lat - min(lat),
    cluster = factor(as.integer(cluster))
  )


# swap medial and lateral of right hemisphere
minMed <- miccai %>% filter(hemi=="right" & side == "medial") %>% select(long) %>% min
minLat <- miccai %>% filter(hemi=="right" & side == "lateral") %>% select(long) %>% min
diff <- minLat - minMed
diff <- 4.25 # adjustment for nicer distances

miccai <- miccai %>%
  mutate(long = ifelse(hemi=="right" & side == "lateral",
                       long + diff, long),
         long = ifelse(hemi=="right" & side == "medial",
                       long - diff, long))


# miccai$pos[1] <- list(x = 1)
# for(i in 1:nrow(miccai)){
#   miccai$pos[[i]] = list(
#     stacked = list(
#       x = list(breaks = c(1.7, 6.5),
#                labels = c("lateral", "medial")),
#       y = list(breaks = c(1,  4.5),
#                labels = c("left", "right")), labs = list(
#                  y = "side", x = "hemisphere")),
#     dispersed = list(
#       x = list(
#         breaks = c(4, 13.5),
#         labels = c("left", "right")),
#       y = list(breaks = NULL, labels = ""),
#       labs = list(y = NULL, x = "hemisphere")))
# }
miccai <- as_ggseg_atlas(miccai)
usethis::use_data(miccai, internal = FALSE, overwrite = TRUE, compress = "xz")




load("data-raw/geobrain_miccaiArea.Rda")

miccaiAr <- geobrain_miccaiArea %>%
  mutate(label = paste(hemi, gsub(" ", "_", aparc), sep = "_"),
         hemi = case_when(hemi == "lh" ~ "left",
                          hemi == "rh" ~ "right"),
         side = ifelse(id %in% c(1:20, 2000:2020), "lateral", "medial"),
         aparc = ifelse(grepl("wall", aparc), NA, aparc),
         atlas = "miccaiAr") %>%
  rename(area = aparc,
         cluster = aparc2) %>%
  select(-group, -meas, -piece, -`as.numeric(id)`) %>%
  mutate(
    long = long - min(long),
    lat = lat - min(lat),
    cluster = factor(as.integer(cluster))
  )


# swap medial and lateral of right hemisphere
minMed <- miccaiAr %>% filter(hemi=="right" & side == "medial") %>% select(long) %>% min
minLat <- miccaiAr %>% filter(hemi=="right" & side == "lateral") %>% select(long) %>% min
diff <- minLat - minMed
diff <- 4.25 # adjustment for nicer distances

miccaiAr <- miccaiAr %>%
  mutate(long = ifelse(hemi=="right" & side == "lateral",
                       long + diff, long),
         long = ifelse(hemi=="right" & side == "medial",
                       long - diff, long))


# miccaiAr$pos[1] <- list(x = 1)
# for(i in 1:nrow(miccaiAr)){
#   miccaiAr$pos[[i]] = list(
#     stacked = list(
#       x = list(breaks = c(1.7, 6.5),
#                labels = c("lateral", "medial")),
#       y = list(breaks = c(1,  4.5),
#                labels = c("left", "right")), labs = list(
#                  y = "side", x = "hemisphere")),
#     dispersed = list(
#       x = list(
#         breaks = c(4, 13.5),
#         labels = c("left", "right")),
#       y = list(breaks = NULL, labels = ""),
#       labs = list(y = NULL, x = "hemisphere")))
# }
miccaiAr <- as_ggseg_atlas(miccaiAr)
usethis::use_data(miccaiAr, internal = FALSE, overwrite = TRUE, compress = "xz")
