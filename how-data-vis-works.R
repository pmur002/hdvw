## -----------------------------------------------------------------------------
#| echo: false
#| message: false
library(dplyr)
library(knitr)
library(reshape2)
library(ggplot2)
library(scales)
library(colorspace)
library(ggsci)


## -----------------------------------------------------------------------------
#| echo: false
figbg <- "#F2F2F2"
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




## -----------------------------------------------------------------------------
#| echo: false
#| message: false
#| label: tbl-crime
#| tbl-cap: A table of the number of youth offenders aged 14 to 16 from 2011 to 2021.  It is difficult to perceive trends in the crime rates from this purely text-based, tabular presentation of the data.

crimeTable <- dcast(crimeAgeSimple[c("age", "year", "rate")],
                    age ~ year)
kable(crimeTable, digits=0)


## -----------------------------------------------------------------------------
#| echo: false
#| label: crime-age-line
#| output: false
ggplot(crimeAgeSimple) +
    geom_line(aes(x=year, y=rate, colour=ageFactor))


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-crime
#| fig-cap: A line plot of the crime rate amongst youth offenders aged 14 to 16 from 2011 to 2021.  Detecting trends in the crime rates is very fast and easy with this data visualisation.
gg <- 
ggplot(crimeAgeSimple) +
    geom_line(aes(x=year, y=rate, colour=ageFactor))
gg +
    scale_colour_discrete(name="age") +
    theme(panel.grid.major.y=element_line(colour="black", linewidth=.1),
          aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-crime-heatmap
#| fig-cap: A heatmap of the crime rate amongst youth offenders aged 14 to 16 from 2011 to 2021.
ggplot(crimeAgeSimple) +
    geom_tile(aes(year, age, fill=rate), colour=NA) +
    scale_x_continuous(expand=expansion(0)) +
    scale_y_continuous(expand=expansion(0), breaks=14:16) +
    theme(aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
title <- "Crime per Year"
subtitle <- "Children aged 14-16"
scatter <- ggplot(crimeYear) + 
    geom_line(aes(yearDate, total), linewidth=.5) +
    labs(title=title, subtitle=subtitle) +
    scale_x_date(name="year") +
    theme(plot.subtitle=element_text(size=8))
bar <- ggplot(crimeYear) + 
    geom_col(aes(yearDate, total), width=200) +
    labs(title=title, subtitle=subtitle) +
    scale_x_date(name="year") +
    theme(plot.subtitle=element_text(size=8))
title2 <- "Crime by Severity"
n <- nrow(crimeLevelTotal)
crimeLevelTotal$xmin <- c(0, cumsum(crimeLevelTotal$total)[-n])
crimeLevelTotal$xmax <- cumsum(crimeLevelTotal$total)
crimeLevelTotal$ymin <- .5
crimeLevelTotal$ymax <- 1
pie <- ggplot(crimeLevelTotal) + 
    geom_rect(aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax, fill=level), 
              colour="black", linewidth=.1) +
    coord_polar() +
    scale_y_continuous(limits=c(0, 1)) +
    scale_fill_grey() +
    labs(title=title2, subtitle=subtitle) +
    theme(panel.border=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          plot.subtitle=element_text(size=8))


## -----------------------------------------------------------------------------
#| echo: false
#| results: hide
svg("Figures/line-plot.svg", bg=figbg, width=3, height=3)
print(scatter, newpage=FALSE)
dev.off()
svg("Figures/bar-plot.svg", bg=figbg, width=3, height=3)
print(bar, newpage=FALSE)
dev.off()
svg("Figures/donut-plot.svg", bg=figbg, width=3, height=3)
print(pie, newpage=FALSE)
dev.off()


## -----------------------------------------------------------------------------
#| echo: false
#| results: hide
#| message: false
highlight <- "#7D12BA" ## Match text code colour (more precise than "purple")
scatterGeom <- scatter +
    geom_line(aes(yearDate, total), linewidth=1, colour=highlight) +
    theme(panel.border=element_rect(colour="grey"),
          plot.title=element_text(colour="grey"),
          plot.subtitle=element_text(colour="grey"),
          axis.ticks=element_line(colour="grey"),
          axis.text=element_text(colour="grey"),
          axis.title=element_text(colour="grey"))
barGeom <- bar + 
    geom_col(aes(yearDate, total), fill=highlight, width=250) +
    theme(panel.border=element_rect(colour="grey", fill=NA),
          plot.title=element_text(colour="grey"),
          plot.subtitle=element_text(colour="grey"),
          panel.grid.major.y=element_line(colour="grey80"),
          axis.ticks=element_line(colour="grey"),
          axis.text=element_text(colour="grey"),
          axis.title=element_text(colour="grey"))
pieGeom <- pie + 
    geom_rect(aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax), 
              fill=highlight, colour="black", linewidth=.1) +
    scale_fill_manual(values=grey(.6 + .4*1:n/(n + 1))) +
    theme(plot.title=element_text(colour="grey"),
          plot.subtitle=element_text(colour="grey"),
          axis.ticks=element_line(colour="grey"),
          axis.text=element_text(colour="grey"),
          axis.title=element_text(colour="grey"),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          legend.text=element_text(colour="grey"),
          legend.title=element_text(colour="grey"))

## -----------------------------------------------------------------------------
#| echo: false
#| results: hide
svg("Figures/line-plot-geom.svg", bg=figbg, width=3, height=3)
print(scatterGeom, newpage=FALSE)
dev.off()
svg("Figures/bar-plot-geom.svg", bg=figbg, width=3, height=3)
print(barGeom, newpage=FALSE)
dev.off()
svg("Figures/donut-plot-geom.svg", bg=figbg, width=3, height=3)
print(pieGeom, newpage=FALSE)
dev.off()


## -----------------------------------------------------------------------------
#| echo: false
#| results: hide
#| message: false
scatterGuide <- scatter +
    geom_line(aes(yearDate, total), linewidth=1, colour="grey90") +
    theme(panel.border=element_rect(colour="grey"),
          plot.title=element_text(colour="grey"),
          plot.subtitle=element_text(colour="grey"),
          axis.line=element_line(colour=highlight),
          axis.ticks=element_line(colour=highlight),
          axis.text=element_text(colour=highlight),          
          axis.title=element_text(colour="grey"))
barGuide <- bar + 
    geom_col(aes(yearDate, total), fill="grey90", col="grey90", width=200) +
    theme(panel.border=element_rect(colour="grey", fill=NA),
          plot.title=element_text(colour="grey"),
          plot.subtitle=element_text(colour="grey"),
          panel.grid.major.y=element_line(colour=highlight),
          axis.line=element_line(colour=highlight),
          axis.ticks=element_line(colour=highlight),
          axis.text=element_text(colour=highlight),          
          axis.title=element_text(colour="grey"))
pieGuide <- pie + 
    geom_rect(aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax), 
              fill="grey90", colour="grey", linewidth=.1) +
    scale_fill_manual(values=colorRampPalette(c(highlight, lighten(highlight, .5)))(n)) +
    theme(plot.title=element_text(colour="grey"),
          plot.subtitle=element_text(colour="grey"),
          axis.ticks=element_line(colour="grey"),
          axis.text=element_text(colour="grey"),
          axis.title=element_text(colour="grey"),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          legend.text=element_text(colour=highlight),
          legend.title=element_text(colour="grey"))

## -----------------------------------------------------------------------------
#| echo: false
#| results: hide
svg("Figures/line-plot-guide.svg", bg=figbg, width=3, height=3)
print(scatterGuide, newpage=FALSE)
dev.off()
svg("Figures/bar-plot-guide.svg", bg=figbg, width=3, height=3)
print(barGuide, newpage=FALSE)
dev.off()
svg("Figures/donut-plot-guide.svg", bg=figbg, width=3, height=3)
print(pieGuide, newpage=FALSE)
dev.off()


## -----------------------------------------------------------------------------
#| echo: false
#| results: hide
#| message: false
scatterLabel <- scatter +
    geom_line(aes(yearDate, total), linewidth=1, colour="grey90") +
    theme(panel.border=element_rect(colour="grey"),
          plot.title=element_text(colour=highlight, face="bold"),
          plot.subtitle=element_text(colour=highlight, face="bold"),
          axis.ticks=element_line(colour="grey"),
          axis.text=element_text(colour="grey"),          
          axis.title=element_text(colour=highlight, face="bold"))
barLabel <- bar + 
    geom_col(aes(yearDate, total), fill="grey90", col="grey90", width=200) +
    theme(panel.border=element_rect(colour="grey", fill=NA),
          plot.title=element_text(colour=highlight, face="bold"),
          plot.subtitle=element_text(colour=highlight, face="bold"),
          panel.grid.major.y=element_line(colour="grey80"),
          axis.ticks=element_line(colour="grey"),
          axis.text=element_text(colour="grey"),          
          axis.title=element_text(colour=highlight, face="bold"))
pieLabel <- pie + 
    geom_rect(aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax), 
              fill="grey90", colour="grey", linewidth=.1) +
    scale_fill_manual(values=grey(.6 + .4*1:n/(n + 1))) +
    theme(plot.title=element_text(colour=highlight, face="bold"),
          plot.subtitle=element_text(colour=highlight, face="bold"),
          axis.ticks=element_line(colour="grey"),
          axis.text=element_text(colour="grey"),
          axis.title=element_text(colour="grey"),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          legend.text=element_text(colour="grey"),
          legend.title=element_text(colour=highlight, face="bold"))

## -----------------------------------------------------------------------------
#| echo: false
#| results: hide
svg("Figures/line-plot-label.svg", bg=figbg, width=3, height=3)
print(scatterLabel, newpage=FALSE)
dev.off()
svg("Figures/bar-plot-label.svg", bg=figbg, width=3, height=3)
print(barLabel, newpage=FALSE)
dev.off()
svg("Figures/donut-plot-label.svg", bg=figbg, width=3, height=3)
print(pieLabel, newpage=FALSE)
dev.off()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-crime-2
#| fig-cap: A line plot of the crime rate amongst youth offenders aged 14 to 16 from 2011 to 2021.  This is a reproduction of @fig-crime.
gg <- 
ggplot(crimeAgeSimple) +
    geom_line(aes(x=year, y=rate, colour=ageFactor))
gg +
    scale_colour_discrete(name="age") +
    theme(panel.grid.major.y=element_line(colour="black", linewidth=.1),
          aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-crime-heatmap-2
#| fig-cap: A heatmap of the crime rate amongst youth offenders aged 14 to 16 from 2011 to 2021.  This is a reproduction of @fig-crime-heatmap.
ggplot(crimeAgeSimple) +
    geom_tile(aes(year, age, fill=rate), colour=NA) +
    scale_x_continuous(expand=expansion(0)) +
    scale_y_continuous(expand=expansion(0), breaks=14:16) +
    theme(aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-bar
#| fig-cap: A bar plot of number of crimes for different levels of crime. The data symbols in this plot are the bars (highlighted in purple).
ggplot(crimeLevelTotal) + 
    geom_col(aes(x=total, y=level), fill=highlight) +
    scale_x_continuous(expand=expansion(c(0, .05))) +
    theme(panel.border=element_rect(colour="grey", fill=NA),
          plot.title=element_text(colour="grey"),
          plot.subtitle=element_text(colour="grey"),
          panel.grid.major.x=element_line(colour="grey80"),
          panel.grid.minor.x=element_blank(),
          panel.grid.major.y=element_blank(),
          panel.grid.minor.y=element_blank(),
          axis.ticks=element_line(colour="grey"),
          axis.text=element_text(colour="grey"),
          axis.title=element_text(colour="grey"),
          aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-length
#| fig-cap: A bar plot of number of crimes for different levels of crime. The data values, the `total` number of crimes, have been mapped to the **lengths** of the bars (the purple lines).
ggplot(crimeLevelTotal) + 
    geom_col(aes(x=total, y=level), fill="grey") +
    scale_x_continuous(expand=expansion(c(0, .05))) +
    geom_segment(aes(xend=total, y=level, yend=level), x=0, 
                 color=highlight, linewidth=1,
                 arrow=arrow(angle=15, length=unit(3, "mm"), ends="both")) +
    theme(panel.border=element_rect(colour="grey", fill=NA),
          plot.title=element_text(colour="grey"),
          plot.subtitle=element_text(colour="grey"),
          panel.grid.major.x=element_line(colour="grey80"),
          panel.grid.minor.x=element_blank(),
          panel.grid.major.y=element_blank(),
          panel.grid.minor.y=element_blank(),
          axis.ticks=element_line(colour="grey"),
          axis.text=element_text(colour="grey"),
          axis.title=element_text(colour="grey"),
          aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-position
#| fig-cap: A bar plot of number of crimes for different levels of crime. The data values, the `level`s of crime, have been mapped to the **positions** of the bars (the purple dots).
ggplot(crimeLevelTotal) + 
    geom_col(aes(x=total, y=level), fill="grey") +
    scale_x_continuous(expand=expansion(c(0, .05))) +
    geom_point(aes(y=level), x=0, color=highlight, size=3) +
    coord_cartesian(clip="off") +
    theme(panel.border=element_rect(colour="grey", fill=NA),
          plot.title=element_text(colour="grey"),
          plot.subtitle=element_text(colour="grey"),
          panel.grid.major.x=element_line(colour="grey80"),
          panel.grid.minor.x=element_blank(),
          panel.grid.major.y=element_blank(),
          panel.grid.minor.y=element_blank(),
          axis.ticks=element_line(colour="grey"),
          axis.text=element_text(colour="grey"),
          axis.title=element_text(colour="grey"),
          aspect.ratio=1)

