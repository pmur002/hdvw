
################################################################################
## Packages
pkgs <- readLines("packages.txt")
invisible(lapply(pkgs, library, character.only=TRUE))

################################################################################
## Default themes
figbg <- "#F2F2F2"
highlight <- "#7D12BA" ## Match text code colour (more precise than "purple")
## Set default colour palettes
scale_colour_discrete <- function(...) {
    scale_colour_npg(...)
}
scale_colour_ordinal <- function(...) {
    scale_colour_brewer(palette="Purples", ...)
}
scale_colour_continuous <- function(...) {
    scale_colour_distiller(palette="Purples", direction=1, ...)
}
scale_fill_discrete <- function(...) {
    scale_fill_npg(...)
}
scale_fill_ordinal <- function(...) {
    scale_fill_brewer(palette="Purples", ...)
}
scale_fill_continuous <- function(...) {
    scale_fill_distiller(palette="Purples", direction=1, ...)
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

################################################################################
## Data
source("rwc.R")
source("youth-crime.R")
crimeAgeSimple <- subset(crimeAge, age %in% c("14", "15", "16"))

################################################################################
## knitr defaults
knitr::opts_chunk$set(dev='svg', 
                      dev.args=list(bg=figbg),
                      fig.width=8, fig.height=4,
                      out.width="100%")

################################################################################
## Common labels
crimeAgeLineTitle <- ggtitle("Youth Crime in New Zealand",
                             "Distinct offenders per 10,000 pop.")
crimeAgeLineXlab <- xlab(NULL)
crimeAgeLineYlab <- ylab(NULL)

