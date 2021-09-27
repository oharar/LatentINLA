
#' Levanger
#'
#' Data on plant coverage in semi-natural grasslands from around Levanger, Norway
#'
#' @format A list with two data frames. species contains percent cover of 90
#' species, site contains site 35 covariates (including plotID and date).
#' The data are by row and column abundance.
#'
#' This data set contains data on vascular plant species in semi-natural
#' grasslands in a study area in Levanger, Norway. The extent of the study area
#' was about 4 km × 5 km. The vegetation was recorded in 1 m × 1 m plots using
#' a restricted random sampling design and the dataset includes data from 132
#' such plots.
#'
#' The coordinates for each plot were randomly drawn within the study area but
#' only plots located within a semi-natural meadow was included in the final
#' dataset. The random sampling was restricted by dividing the study area into
#' five equally sized units and requiring an equal number of plots within each
#' of these.  The study area and the management of the grasslands are also described in
#' Johansen et al. (2016).
#'
#' In the sampling plots, the cover of all vascular
#' plant species was recorded as percentage cover and these are the numbers
#' presented in the dataset. The total cover within a plot was allowed to
#' exceed 100%.
#'
#' In addition to the plant cover data, a number of environmental variables
#' were recorded for each plot. The environmental variables can be found to
#' the right in the dataset, and include variables scored by visual assessment
#' in the field and results from lab analysis of soil samples. Separated soil
#' samples were collected from 0-10 cm and 10-20 cm soil depth. Each soil
#' sample included soil from nine different subsamples taken just outside
#' the 1 m × 1 m plots. The soil samples were analysed for pH and organic
#' matter content (OM) according to standard methods. The
#' ammonium-acetate-lactate method (–AL) described by Egner et al. (1960) was
#' used to estimate the content of plant available P, K, Mg and Ca in the soil
#' samples. The units for all plant available nutrients are mg 100 g^{-1}.


#' @source \url{https://doi.org/10.6084/m9.figshare.15143937.v1}
"Levanger"


#' Abundances of Forminifera, along with trait and site data.
#'
#' @format A list with 4 data frames:
#' comm: abundances of 24 species (columns) at 31 sites (rows)
#' traits: data on 3 traits of the 24 species
#' envir: 10 environmental variables measured at the 31 sites
#' coord: Coordinates of the 31 sites.
#'
#' The data are by species and site abundance.
#'
#' @source van der Veen (original somewhere else: need to dig out the reference)
"forminifera"

