## code to prepare `DATASET` dataset goes here

# Beetles
# beetles <- read.csv("data-raw/beetles2.csv")
# usethis::use_data(beetles, overwrite = TRUE)

# Levanger

Levanger.raw <- read.csv("data-raw/semi_natural_grasslands_Levanger_2008_figshare.csv",
                     sep=";")
IsSpecies <-  grepl(".", names(Levanger.raw), fixed = TRUE)
# Sort sites so most covered is to left
SiteAb <- rowSums(Levanger.raw[,IsSpecies])
Levanger.raw <- Levanger.raw[order(SiteAb, decreasing = TRUE), ]

Levanger <- list(
  species = Levanger.raw[,IsSpecies],
  site = Levanger.raw[,!IsSpecies]
)
# Sort species so most abundant is at top
SpAb <- colSums(Levanger$species)
Levanger$species <- Levanger$species[,order(SpAb, decreasing = TRUE)]

usethis::use_data(Levanger, overwrite = TRUE)

# Forminifera
load("data-raw/CESTES.RData")
forminifera <- LSmatred[[21]]
SiteAb <- order(rowSums(forminifera$comm), decreasing = TRUE)
SpeciesAb <- order(colSums(forminifera$comm), decreasing = TRUE)

forminifera <- list(
  comm = as.data.frame(forminifera$comm[SiteAb, SpeciesAb]),
  traits = as.data.frame(forminifera$traits[SpeciesAb, ]),
  envir = as.data.frame(forminifera$envir[SiteAb, ]),
  coord = as.data.frame(forminifera$coord[SiteAb, ])
)
usethis::use_data(forminifera, overwrite = TRUE)


