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
#| eval: false
#| label: eye-basic
## ## grid.newpage()
## eyeball <- circleGrob(r=.3, gp=gpar(lwd=3, fill="grey90"))
## eyeballNot <- as.path(grobTree(eyeball, rectGrob(width=1.5)), rule="evenodd")
## pushViewport(viewport(width=unit(1, "snpc"), height=unit(1, "snpc"),
##                       gp=gpar(lineheight=.9, fill=NA)))
## grid.text("light", x=unit(0, "npc") - unit(2, "mm"), just="right")
## grid.segments(0, .5,
##               unit(.17, "npc") - unit(2, "mm"), .5,
##               gp=gpar(lwd=3, fill="black"),
##               arrow=arrow(type="closed", length=unit(3, "mm")))
## grid.draw(eyeball)
## grid.define(circleGrob(r=.1), name="cornea", gp=gpar(lwd=3))
## grid.define(circleGrob(r=.07), name="lens", gp=gpar(lwd=3, fill="white"))
## pushViewport(viewport(clip=eyeballNot))
## pushViewport(viewport(x=.22, width=.5))
## grid.use("cornea")
## popViewport(2)
## pushViewport(viewport(x=.22, width=.5))
## grid.use("lens")
## popViewport()
## grid.text("lens", x=unit(.25, "npc") + unit(2, "mm"), just="left")
## pushViewport(viewport(clip=rectGrob(x=.5, height=.1, just="left")))
## grid.circle(r=.28, gp=gpar(lwd=3))
## popViewport()
## grid.text("retina", x=.75, just="right")
## pushViewport(viewport(clip=polygonGrob(c(.5, 1, 1), c(.5, -.2, 1.2))))
## grid.circle(r=.28, gp=gpar(lwd=3))
## popViewport()
## pushViewport(viewport(clip=eyeballNot))
## grid.segments(.5, .5, 1, .35,
##               gp=gpar(lwd=3, fill="black"),
##               arrow=arrow(type="closed", length=unit(3, "mm")))
## popViewport()
## grid.text("optic\nnerve", y=.35,
##           x=unit(1, "npc") + unit(2, "mm"), just="left")


## -----------------------------------------------------------------------------
#| echo: false
#| eval: false
#| label: eye-basic-2
## ## grid.newpage()
## eyeball <- circleGrob(r=.3, gp=gpar(lwd=3, fill="grey90"))
## eyeballNot <- as.path(grobTree(eyeball, rectGrob(width=1.5)), rule="evenodd")
## pushViewport(viewport(width=unit(1, "snpc"), height=unit(1, "snpc"),
##                       gp=gpar(lineheight=.9, fill=NA)))
## grid.text("light", x=unit(0, "npc") - unit(2, "mm"), just="right")
## grid.segments(0, .5,
##               unit(.17, "npc") - unit(2, "mm"), .5,
##               gp=gpar(lwd=3, fill="black"),
##               arrow=arrow(type="closed", length=unit(3, "mm")))
## grid.draw(eyeball)
## grid.define(circleGrob(r=.1), name="cornea", gp=gpar(lwd=3))
## grid.define(circleGrob(r=.07), name="lens", gp=gpar(lwd=3, fill="white"))
## pushViewport(viewport(clip=eyeballNot))
## pushViewport(viewport(x=.22, width=.5))
## grid.use("cornea")
## popViewport(2)
## pushViewport(viewport(x=.22, width=.5))
## grid.use("lens")
## popViewport()
## pushViewport(viewport(clip=rectGrob(x=.5, height=.1, just="left")))
## grid.circle(r=.28, gp=gpar(lwd=3))
## popViewport()
## grid.text("cones", x=.75, just="right")
## pushViewport(viewport(clip=polygonGrob(c(.5, 1, 1), c(.5, .65, 1.2))))
## grid.circle(r=.28, gp=gpar(lwd=3))
## popViewport()
## grid.text("rods", x=.7, y=.65, just="right")
## pushViewport(viewport(clip=polygonGrob(c(.5, 1, 1), c(.5, .3, -.2))))
## grid.circle(r=.28, gp=gpar(lwd=3))
## popViewport()
## grid.text("rods", x=.7, y=.35, just="right")
## pushViewport(viewport(clip=eyeballNot))
## grid.segments(.5, .5, 1, .35,
##               gp=gpar(lwd=3, fill="black"),
##               arrow=arrow(type="closed", length=unit(3, "mm")))
## popViewport()
## grid.text("brain\nthis\nway", y=.35,
##           x=unit(1, "npc") + unit(2, "mm"), just="left")


## -----------------------------------------------------------------------------
#| echo: false
#| results: hide
svg("Figures/eye-basic.svg", bg=figbg, width=4, height=3)
## grid.newpage()
eyeball <- circleGrob(r=.3, gp=gpar(lwd=3, fill="grey90"))
eyeballNot <- as.path(grobTree(eyeball, rectGrob(width=1.5)), rule="evenodd")
pushViewport(viewport(width=unit(1, "snpc"), height=unit(1, "snpc"),
                      gp=gpar(lineheight=.9, fill=NA)))
grid.text("light", x=unit(0, "npc") - unit(2, "mm"), just="right")
grid.segments(0, .5,
              unit(.17, "npc") - unit(2, "mm"), .5,
              gp=gpar(lwd=3, fill="black"), 
              arrow=arrow(type="closed", length=unit(3, "mm")))
grid.draw(eyeball)
grid.define(circleGrob(r=.1), name="cornea", gp=gpar(lwd=3))
grid.define(circleGrob(r=.07), name="lens", gp=gpar(lwd=3, fill="white"))
pushViewport(viewport(clip=eyeballNot))
pushViewport(viewport(x=.22, width=.5))
grid.use("cornea")
popViewport(2)
pushViewport(viewport(x=.22, width=.5))
grid.use("lens")
popViewport()
grid.text("lens", x=unit(.25, "npc") + unit(2, "mm"), just="left")
pushViewport(viewport(clip=rectGrob(x=.5, height=.1, just="left")))
grid.circle(r=.28, gp=gpar(lwd=3))
popViewport()
grid.text("retina", x=.75, just="right")
pushViewport(viewport(clip=polygonGrob(c(.5, 1, 1), c(.5, -.2, 1.2))))
grid.circle(r=.28, gp=gpar(lwd=3))
popViewport()
pushViewport(viewport(clip=eyeballNot))
grid.segments(.5, .5, 1, .35, 
              gp=gpar(lwd=3, fill="black"),
              arrow=arrow(type="closed", length=unit(3, "mm")))
popViewport()
grid.text("optic\nnerve", y=.35, 
          x=unit(1, "npc") + unit(2, "mm"), just="left")
dev.off()
svg("Figures/eye-basic-2.svg", bg=figbg, width=4, height=3)
## grid.newpage()
eyeball <- circleGrob(r=.3, gp=gpar(lwd=3, fill="grey90"))
eyeballNot <- as.path(grobTree(eyeball, rectGrob(width=1.5)), rule="evenodd")
pushViewport(viewport(width=unit(1, "snpc"), height=unit(1, "snpc"),
                      gp=gpar(lineheight=.9, fill=NA)))
grid.text("light", x=unit(0, "npc") - unit(2, "mm"), just="right")
grid.segments(0, .5,
              unit(.17, "npc") - unit(2, "mm"), .5,
              gp=gpar(lwd=3, fill="black"), 
              arrow=arrow(type="closed", length=unit(3, "mm")))
grid.draw(eyeball)
grid.define(circleGrob(r=.1), name="cornea", gp=gpar(lwd=3))
grid.define(circleGrob(r=.07), name="lens", gp=gpar(lwd=3, fill="white"))
pushViewport(viewport(clip=eyeballNot))
pushViewport(viewport(x=.22, width=.5))
grid.use("cornea")
popViewport(2)
pushViewport(viewport(x=.22, width=.5))
grid.use("lens")
popViewport()
pushViewport(viewport(clip=rectGrob(x=.5, height=.1, just="left")))
grid.circle(r=.28, gp=gpar(lwd=3))
popViewport()
grid.text("cones", x=.75, just="right")
pushViewport(viewport(clip=polygonGrob(c(.5, 1, 1), c(.5, .65, 1.2))))
grid.circle(r=.28, gp=gpar(lwd=3))
popViewport()
grid.text("rods", x=.7, y=.65, just="right")
pushViewport(viewport(clip=polygonGrob(c(.5, 1, 1), c(.5, .3, -.2))))
grid.circle(r=.28, gp=gpar(lwd=3))
popViewport()
grid.text("rods", x=.7, y=.35, just="right")
pushViewport(viewport(clip=eyeballNot))
grid.segments(.5, .5, 1, .35, 
              gp=gpar(lwd=3, fill="black"),
              arrow=arrow(type="closed", length=unit(3, "mm")))
popViewport()
grid.text("brain\nthis\nway", y=.35, 
          x=unit(1, "npc") + unit(2, "mm"), just="left")
dev.off()


## -----------------------------------------------------------------------------
#| echo: false
#| message: false
#| label: tbl-crime-group-total
#| tbl-colwidths: [20,20]
#| tbl-cap: A table of the total number of offenders aged 14 to 16 from 2011 to 2021 for different ethnic groups.
crimeGroupTotalTable <- subset(crimeGroupTotal, select=c("group", "total"))
kable_styling(kable(crimeGroupTotalTable, 
                    table.attr='data-quarto-disable-processing="true"'),
              bootstrap_options=c("basic", "hover"), 
              full_width=FALSE)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-bar
#| fig-cap: A bar plot of number of crimes for different ethnic groups. The data symbols in this plot are the horizontal bars.
ggplot(crimeGroupTotal) + 
    geom_col(aes(x=total, y=group)) +
    scale_x_continuous(expand=expansion(c(0, .05))) +
    theme(aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-length
#| fig-cap: A bar plot of number of crimes for different ethnic groups. The data values, the `total` number of crimes, have been mapped to the **lengths** of the bars (the purple lines).
ggplot(crimeGroupTotal) + 
    geom_col(aes(x=total, y=group), fill="grey") +
    scale_x_continuous(expand=expansion(c(0, .05))) +
    geom_segment(aes(xend=total, y=group, yend=group), x=0, 
                 color=highlight, linewidth=1,
                 arrow=arrow(angle=15, length=unit(3, "mm"), ends="both")) +
    theme(panel.border=element_rect(colour="grey", fill=NA),
          plot.title=element_text(colour="grey"),
          plot.subtitle=element_text(colour="grey"),
          axis.ticks=element_line(colour="grey"),
          axis.text=element_text(colour="grey"),
          axis.title=element_text(colour="grey"),
          aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-position
#| fig-cap: A bar plot of number of crimes for different ethnic groups. The data values, the ethnic `group`s, have been mapped to the **positions** of the bars (the purple dots).
ggplot(crimeGroupTotal) + 
    geom_col(aes(x=total, y=group), fill="grey") +
    scale_x_continuous(expand=expansion(c(0, .05))) +
    geom_point(aes(y=group), x=0, color=highlight, size=3) +
    coord_cartesian(clip="off") +
    theme(panel.border=element_rect(colour="grey", fill=NA),
          plot.title=element_text(colour="grey"),
          plot.subtitle=element_text(colour="grey"),
          axis.ticks=element_line(colour="grey"),
          axis.text=element_text(colour="grey"),
          axis.title=element_text(colour="grey"),
          aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-visual-features
#| fig-cap: Some examples of basic visual features are **position**, **length**, **angle**, **area**, **colour**, and **shape**.
grid.newpage()
pushViewport(viewport(layout=grid.layout(6, 2, widths=1:2), gp=gpar(cex=1.5)))
pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
grid.text("position", x=0, just="right")
grid.segments(.2, .5, .8, .5, gp=gpar(lwd=2))
grid.segments(.2, .4, .2, .6, gp=gpar(lwd=2))
grid.segments(.8, .4, .8, .6, gp=gpar(lwd=2))
grid.circle(c(.3, .6), .7, r=unit(1, "mm"), gp=gpar(fill="black"))
popViewport()
pushViewport(viewport(layout.pos.row=2))
grid.rect(gp=gpar(col=NA, fill="grey90"))
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
grid.text("length", x=0, just="right")
grid.rect(.2, c(.3, .7), c(.1, .4), .2, just="left", gp=gpar(fill="black"))
popViewport()
pushViewport(viewport(layout.pos.row=3, layout.pos.col=2))
grid.text("angle", x=0, just="right")
grid.lines(c(.55, .45, .55) - .2, c(.6, .4, .4), gp=gpar(lwd=2))
grid.lines(c(.55, .45, .55) + .2, c(.8, .4, .4), gp=gpar(lwd=2))
popViewport()
pushViewport(viewport(layout.pos.row=4))
grid.rect(gp=gpar(col=NA, fill="grey90"))
popViewport()
pushViewport(viewport(layout.pos.row=4, layout.pos.col=2))
grid.text("area", x=0, just="right")
grid.circle(c(.3, .7), .5, r=unit(c(1, 3), "mm"), gp=gpar(fill="black"))
popViewport()
pushViewport(viewport(layout.pos.row=5, layout.pos.col=2))
grid.text("colour", x=0, just="right")
grid.circle(c(.3, .7), .5, r=unit(2, "mm"), gp=gpar(fill=2:3))
popViewport()
pushViewport(viewport(layout.pos.row=6))
grid.rect(gp=gpar(col=NA, fill="grey90"))
popViewport()
pushViewport(viewport(layout.pos.row=6, layout.pos.col=2))
grid.text("shape", x=0, just="right")
grid.points(c(.3, .7), c(.5, .5), size=unit(4, "mm"), pch=3:4, gp=gpar(lwd=2))
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-quant-features
#| fig-height: 3
#| fig-cap: The visual features position, length, area, and angle are all appropriate visual features for visualising quantitative data values.
## grid.newpage()
vp2 <- viewport(width=.9, y=unit(3, "lines"), 
                height=unit(1, "npc") - unit(6, "lines"), 
                just="bottom")
pushViewport(viewport(layout=grid.layout(1, 5)))
pushViewport(viewport(layout.pos.col=1),
             vp2)
## grid.segments(1, 0, 1, 1)
grid.segments(unit(1, "npc") - unit(2, "mm"), c(0, .5, 1), 
              unit(1, "npc") + unit(2, "mm"), c(0, .5, 1),
              arrow=arrow(type="closed", angle=15, length=unit(2, "mm")), 
              gp=gpar(fill="black"))
grid.text(c("(zero) 0", "(amount) 10", "(twice) 20"),
          unit(1, "npc") - unit(4, "mm"), c(0, .5, 1),
          just="right")
popViewport(2)
pushViewport(viewport(layout.pos.col=2),
             vp2)
grid.segments(.5, 0, .5, 1, gp=gpar(lty="dotted"))
grid.points(rep(.5, 3), c(0, .5, 1), pch=16, size=unit(4, "mm"))
grid.text("position", y=unit(-1, "lines"))
popViewport(2)
pushViewport(viewport(layout.pos.col=3),
             vp2)
grid.rect(1:3/4, 0, .2, c(0, .5, 1), gp=gpar(fill="black"), just="bottom")
grid.text("length", y=unit(-1, "lines"))
popViewport(2)
pushViewport(viewport(layout.pos.col=4),
             vp2)
grid.circle(.5, c(0, .5, 1), r=unit(sqrt(.5*c(0.001, .5, 1)), "cm"),
            gp=gpar(fill="black"))
grid.text("area", y=unit(-1, "lines"))
popViewport(2)
pushViewport(viewport(layout.pos.col=5),
             vp2)
grid.lines(unit(.5, "npc") + unit(c(-4, -4, 4), "mm"),
           unit(1, "npc") + unit(c(4, -4, -4), "mm"),
           gp=gpar(lwd=3))
grid.lines(unit(.5, "npc") + unit(c(2, -4, 4), "mm"),
           unit(.5, "npc") + unit(c(2, -4, -4), "mm"),
           gp=gpar(lwd=3))
grid.segments(unit(.5, "npc") + unit(-4, "mm"), 0,
              unit(.5, "npc") + unit(4, "mm"), 0,
              gp=gpar(lwd=3))
grid.text("angle", y=unit(-1, "lines"))
popViewport(2)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-not-quant
#| fig-height: 3
#| fig-cap: The visual features **colour** and **shape** are not capable of visualising quantitative data values.
vp2 <- viewport(width=.9, y=unit(3, "lines"), 
                height=unit(1, "npc") - unit(6, "lines"), 
                just="bottom")
pushViewport(viewport(layout=grid.layout(1, 5)))
pushViewport(viewport(layout.pos.col=1),
             vp2)
## grid.segments(1, 0, 1, 1)
grid.segments(unit(1, "npc") - unit(2, "mm"), c(0, .5, 1), 
              unit(1, "npc") + unit(2, "mm"), c(0, .5, 1),
              arrow=arrow(type="closed", angle=15, length=unit(2, "mm")), 
              gp=gpar(fill="black"))
grid.text(c("(zero) 0", "(amount) 10", "(twice) 20"),
          unit(1, "npc") - unit(4, "mm"), c(0, .5, 1),
          just="right")
popViewport(2)
pushViewport(viewport(layout.pos.col=2),
             vp2)
cols <- hcl.colors(3, "harmonic")
grid.points(rep(.5, 3), c(0, .5, 1), pch=21, size=unit(7, "mm"),
            gp=gpar(col="black", fill=cols))
grid.text("colour", y=unit(-1, "lines"))
popViewport(2)
pushViewport(viewport(layout.pos.col=3),
             vp2)
grid.points(rep(.5, 3), c(0, .5, 1), pch=2:4, size=unit(5, "mm"),
            gp=gpar(lwd=2))
grid.text("shape", y=unit(-1, "lines"))
popViewport(2)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-qual-features
#| fig-height: 3
#| fig-cap: The visual features **colour**, **shape**, and **position** are capable of representing different groups.
vp2 <- viewport(width=.9, y=unit(3, "lines"), 
                height=unit(1, "npc") - unit(6, "lines"), 
                just="bottom")
pushViewport(viewport(layout=grid.layout(1, 5)))
pushViewport(viewport(layout.pos.col=1),
             vp2)
## grid.segments(1, 0, 1, 1)
grid.segments(unit(1, "npc") - unit(2, "mm"), c(.2, .5, .8), 
              unit(1, "npc") + unit(2, "mm"), c(.2, .5, .8),
              arrow=arrow(type="closed", angle=15, length=unit(2, "mm")), 
              gp=gpar(fill="black"))
grid.text(c("Group A", "Group B", "Group C"),
          unit(1, "npc") - unit(4, "mm"), c(.2, .5, .8),
          just="right")
popViewport(2)
pushViewport(viewport(layout.pos.col=2),
             vp2)
cols <- hcl.colors(3, "harmonic")
grid.points(rep(.5, 3), c(.2, .5, .8), pch=21, size=unit(5, "mm"),
            gp=gpar(col="black", fill=cols))
grid.text("colour", y=unit(-1, "lines"))
popViewport(2)
pushViewport(viewport(layout.pos.col=3),
             vp2)
grid.points(rep(.5, 3), c(.2, .5, .8), pch=2:4, size=unit(5, "mm"),
            gp=gpar(lwd=2))
grid.text("shape", y=unit(-1, "lines"))
popViewport(2)
pushViewport(viewport(layout.pos.col=4),
             vp2)
grid.rect(rep(.2, 3), c(.2, .5, .8), width=c(.6, .6, .6), height=.1,
          just="left", gp=gpar(fill="black"))
grid.text("position", y=unit(-1, "lines"))
popViewport(2)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-not-qual
#| fig-height: 3
#| fig-cap: The visual features **length**, **area**, and **angle** are not appropriate for representing qualitative data values because they imply an ordering of data values that have no inherent order.
vp2 <- viewport(width=.9, y=unit(3, "lines"), 
                height=unit(1, "npc") - unit(6, "lines"), 
                just="bottom")
pushViewport(viewport(layout=grid.layout(1, 5)))
pushViewport(viewport(layout.pos.col=1),
             vp2)
## grid.segments(1, 0, 1, 1)
grid.segments(unit(1, "npc") - unit(2, "mm"), c(.2, .5, .8), 
              unit(1, "npc") + unit(2, "mm"), c(.2, .5, .8),
              arrow=arrow(type="closed", angle=15, length=unit(2, "mm")), 
              gp=gpar(fill="black"))
grid.text(c("Group A", "Group B", "Group C"),
          unit(1, "npc") - unit(4, "mm"), c(.2, .5, .8),
          just="right")
popViewport(2)
pushViewport(viewport(layout.pos.col=2),
             vp2)
grid.rect(1:3/4, 0, .2, c(.2, .5, .8), just="bottom", gp=gpar(fill="black")) 
grid.text("length", y=unit(-1, "lines"))
popViewport(2)
pushViewport(viewport(layout.pos.col=3),
             vp2)
grid.circle(.5, c(.2, .5, .8), r=unit(sqrt(.5*c(.2, .5, .8)), "cm"),
            gp=gpar(fill="black"))
grid.text("area", y=unit(-1, "lines"))
popViewport(2)
pushViewport(viewport(layout.pos.col=4),
             vp2)
grid.lines(unit(.5, "npc") + unit(c(0, 0), "mm"),
           unit(.8, "npc") + unit(c(4, -4), "mm"),
           gp=gpar(lwd=3))
grid.lines(unit(.5, "npc") + unit(c(-4, 4), "mm"),
           unit(.5, "npc") + unit(c(-4, 4), "mm"),
           gp=gpar(lwd=3))
grid.segments(unit(.5, "npc") + unit(-4, "mm"), .2,
              unit(.5, "npc") + unit(4, "mm"), .2,
              gp=gpar(lwd=3))
grid.text("angle", y=unit(-1, "lines"))
popViewport(2)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-accuracy
#| fig-cap: An ordering of basic visual features in terms of their accuracy, with more accurate at the top.
grid.newpage()
pushViewport(viewport(layout=grid.layout(6, 2, widths=1:2), gp=gpar(cex=1.5)))
pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
grid.text("position", x=0, just="right")
grid.segments(.2, .5, .8, .5, gp=gpar(lwd=2))
grid.segments(.2, .4, .2, .6, gp=gpar(lwd=2))
grid.segments(.8, .4, .8, .6, gp=gpar(lwd=2))
grid.circle(c(.3, .6), .7, r=unit(1, "mm"), gp=gpar(fill="black"))
popViewport()
pushViewport(viewport(layout.pos.row=2))
grid.rect(gp=gpar(col=NA, fill="grey90"))
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
grid.text("length", x=0, just="right")
grid.rect(.2, c(.3, .7), c(.1, .4), .2, just="left", gp=gpar(fill="black"))
popViewport()
pushViewport(viewport(layout.pos.row=3, layout.pos.col=2))
grid.text("angle", x=0, just="right")
grid.lines(c(.55, .45, .55) - .2, c(.6, .4, .4), gp=gpar(lwd=2))
grid.lines(c(.55, .45, .55) + .2, c(.8, .4, .4), gp=gpar(lwd=2))
popViewport()
pushViewport(viewport(layout.pos.row=4))
grid.rect(gp=gpar(col=NA, fill="grey90"))
popViewport()
pushViewport(viewport(layout.pos.row=4, layout.pos.col=2))
grid.text("area", x=0, just="right")
grid.circle(c(.3, .7), .5, r=unit(c(1, 3), "mm"), gp=gpar(fill="black"))
popViewport()
pushViewport(viewport(layout.pos.row=5, layout.pos.col=2))
grid.text("colour", x=0, just="right")
grid.circle(c(.3, .7), .5, r=unit(2, "mm"), 
            gp=gpar(fill=hcl(240, c(40, 70), c(40, 70))))
popViewport()
pushViewport(viewport(layout.pos.row=6))
grid.rect(gp=gpar(col=NA, fill="grey90"))
popViewport()
pushViewport(viewport(layout.pos.row=6, layout.pos.col=2))
grid.text("shape", x=0, just="right")
grid.points(c(.3, .7), c(.5, .5), size=unit(4, "mm"), 
            pch=c(4, 8), gp=gpar(lwd=2))
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-colour-cap
#| fig-height: 3
#| fig-cap: Mapping qualitative data values to colour works well for a small number of colours, but less well for a large number of colours.
pie <- function(n) {
    t <- rev(seq(0, 2*pi, length.out=100*n) + pi/2)
    cols <- scales::hue_pal()(n)
    for (i in 1:n) {
        index <- 1:100 + (i-1)*100
        x <- cos(t[index])
        y <- sin(t[index])
        grid.polygon(c(.5, .5 + .3*x), c(.5, .5 + .3*y), 
                     gp=gpar(col=figbg, lwd=4, fill=cols[i]))
        grid.text(letters[i], .5 + mean(.25*x), .5 + mean(.25*y),
                  gp=gpar(cex=1))
    }
}
pushViewport(viewport(layout=grid.layout(1, 3, respect=TRUE)))
pushViewport(viewport(layout.pos.col=1))
pie(3)
popViewport()
pushViewport(viewport(layout.pos.col=2))
pie(7)
popViewport()
pushViewport(viewport(layout.pos.col=3))
pie(11)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-pos-cap
#| fig-height: 3
#| fig-cap: Mapping qualitative data values to position works well for both a small number of positions and for a large number of positions.
bars <- function(n) {
    h <- 3
    y <- unit(.5, "npc") + unit((h + 3)*(seq(1:n) - n/2), "mm")
    grid.rect(.5, y, .3, unit(h, "mm"), gp=gpar(fill="grey"))
    grid.text(rev(letters[1:n]), unit(.35, "npc") - unit(.5, "lines"), y)
}
pushViewport(viewport(layout=grid.layout(1, 3, respect=TRUE)))
pushViewport(viewport(layout.pos.col=1))
bars(3)
popViewport()
pushViewport(viewport(layout.pos.col=2))
bars(7)
popViewport()
pushViewport(viewport(layout.pos.col=3))
bars(11)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-dot
#| fig-cap: A dot plot of the number of crimes for different ethnic groups. The data symbols in this plot are the data points.
ggplot(crimeGroupTotal) + 
    geom_segment(aes(x=-Inf, xend=Inf, y=group, yend=group), 
                 linetype="dotted") +
    geom_point(aes(x=total, y=group)) +
    theme(aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-pie
#| fig-cap: A pie chart of the number of crimes for different ethnic groups. The data symbols in this plot are the pie wedges.
ggplot(crimeGroupTotal) + 
    geom_col(aes(x=total, y="", fill=group), colour="black") +
    coord_polar() +
    theme(aspect.ratio=1,
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank())

