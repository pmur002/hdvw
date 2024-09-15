## -----------------------------------------------------------------------------
#| echo: false
#| message: false
library(dplyr)
library(knitr)
library(kableExtra)
library(reshape2)
library(ggplot2)
library(scales)
library(colorspace)
library(ggsci)
library(grid)


## -----------------------------------------------------------------------------
#| echo: false
figbg <- "#F2F2F2"
highlight <- "#7D12BA" ## Match text code colour (more precise than "purple")
## Set default colour palettes
scale_colour_discrete <- function(...) {
    scale_colour_npg(...)
}
scale_fill_discrete <- function(...) {
    scale_fill_npg(...)
}
scale_fill_ordinal <- function(...) {
    scale_fill_brewer(palette="Purples", ...)
}
scale_fill_continuous <- function(...) {
    scale_fill_distiller(palette="Purples", ...)
}
## Set default line width
update_geom_defaults("line", list(linewidth=1))
## Set default theme
theme_set(theme_bw())
## Match the table odd row fill
theme_update(plot.background=element_rect(colour=NA, 
                                          fill=figbg),
             panel.background=element_blank(),
             panel.grid.minor=element_blank(),
             panel.grid.major=element_blank(),
             legend.background=element_blank())


## -----------------------------------------------------------------------------
#| echo: false
source("youth-crime.R")
crimeAgeSimple <- subset(crimeAge, age %in% c("14", "15", "16"))

