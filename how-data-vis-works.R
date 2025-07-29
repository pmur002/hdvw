pdf(NULL)
## -----------------------------------------------------------------------------
#| echo: false
#| message: false
pkgs <- readLines("packages.txt")
invisible(lapply(pkgs, library, character.only=TRUE))


## -----------------------------------------------------------------------------
#| echo: false
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


## -----------------------------------------------------------------------------
#| echo: false
source("rwc.R")
source("youth-crime.R")
crimeAgeSimple <- subset(crimeAge, age %in% c("14", "15", "16"))






## -----------------------------------------------------------------------------
#| echo: false
#| message: false
#| label: tbl-crime
#| tbl-cap: A table of the number of distinct youth offenders, per 10,000 of popuplation, aged 14 to 16 from 2011 to 2021.  It is difficult to perceive trends in the crime rates from this purely text-based, tabular presentation of the data.

crimeTable <- dcast(crimeAgeSimple[c("age", "year", "rate")],
                    age ~ year)
kable(crimeTable, digits=0)


## -----------------------------------------------------------------------------
#| echo: false
#| label: crime-age-line
#| output: false
gg <- ggplot(crimeAgeSimple) +
    geom_line(aes(x=year, y=rate, colour=ageFactor))


## -----------------------------------------------------------------------------
#| echo: false
crimeAgeLineTitle <- ggtitle("Youth Crime in New Zealand",
                             "Distinct offenders per 10,000 pop.")
crimeAgeLineXlab <- xlab(NULL)
crimeAgeLineYlab <- ylab(NULL)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-crime
#| fig-cap: A line plot of the crime rate amongst youth offenders aged 14 to 16 from 2011 to 2021.  Detecting trends in the crime rates is very fast and easy with this data visualisation.
gg <- 
gg <- ggplot(crimeAgeSimple) +
    geom_line(aes(x=year, y=rate, colour=ageFactor))
ggCrimeAge <- gg +
    scale_x_continuous(breaks=seq(2012, 2020, 2)) +    
    scale_colour_discrete(name="age") +
    crimeAgeLineTitle +
    crimeAgeLineXlab +
    crimeAgeLineYlab +
    theme(panel.grid.major.y=element_line(colour="black", linewidth=.1),
          aspect.ratio=1)
pushViewport(viewport(width=.9, height=.9))
print(ggCrimeAge, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
crimeAgeHeatTitle <- ggtitle("Youth Crime in New Zealand",
                             "Distinct offenders per 10,000 pop.")
crimeAgeHeatXlab <- xlab(NULL)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-crime-heatmap
#| fig-cap: A heatmap of the crime rate amongst youth offenders aged 14 to 16 from 2011 to 2021.
gg <- ggplot(crimeAgeSimple) +
    geom_tile(aes(year, age, fill=rate), colour=NA) +
    crimeAgeHeatTitle +
    crimeAgeHeatXlab +
    scale_x_continuous(expand=expansion(0), breaks=seq(2012, 2020, 2)) +    
    scale_y_continuous(expand=expansion(0), breaks=14:16) +
    theme(aspect.ratio=1)
pushViewport(viewport(width=.9, height=.9))
print(gg, newpage=FALSE)
popViewport()


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
              fill=highlight, colour="grey", linewidth=.1) +
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
              fill="grey90", colour="grey", linewidth=.5) +
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
# ## grid.newpage()
# eyeball <- circleGrob(r=.3, gp=gpar(lwd=3, fill="grey90"))
# eyeballNot <- as.path(grobTree(eyeball, rectGrob(width=1.5)), rule="evenodd")
# pushViewport(viewport(width=unit(1, "snpc"), height=unit(1, "snpc"),
#                       gp=gpar(lineheight=.9, fill=NA)))
# grid.text("light", x=unit(0, "npc") - unit(2, "mm"), just="right")
# grid.segments(0, .5,
#               unit(.17, "npc") - unit(2, "mm"), .5,
#               gp=gpar(lwd=3, fill="black"),
#               arrow=arrow(type="closed", length=unit(3, "mm")))
# grid.draw(eyeball)
# grid.define(circleGrob(r=.1), name="cornea", gp=gpar(lwd=3))
# grid.define(circleGrob(r=.07), name="lens", gp=gpar(lwd=3, fill="white"))
# pushViewport(viewport(clip=eyeballNot))
# pushViewport(viewport(x=.22, width=.5))
# grid.use("cornea")
# popViewport(2)
# pushViewport(viewport(x=.22, width=.5))
# grid.use("lens")
# popViewport()
# grid.text("lens", x=unit(.25, "npc") + unit(2, "mm"), just="left")
# pushViewport(viewport(clip=rectGrob(x=.5, height=.1, just="left")))
# grid.circle(r=.28, gp=gpar(lwd=3))
# popViewport()
# grid.text("retina", x=.75, just="right")
# pushViewport(viewport(clip=polygonGrob(c(.5, 1, 1), c(.5, -.2, 1.2))))
# grid.circle(r=.28, gp=gpar(lwd=3))
# popViewport()
# pushViewport(viewport(clip=eyeballNot))
# grid.segments(.5, .5, 1, .35,
#               gp=gpar(lwd=3, fill="black"),
#               arrow=arrow(type="closed", length=unit(3, "mm")))
# popViewport()
# grid.text("optic\nnerve", y=.35,
#           x=unit(1, "npc") + unit(2, "mm"), just="left")


## -----------------------------------------------------------------------------
#| echo: false
#| eval: false
#| label: eye-basic-2
# ## grid.newpage()
# eyeball <- circleGrob(r=.3, gp=gpar(lwd=3, fill="grey90"))
# eyeballNot <- as.path(grobTree(eyeball, rectGrob(width=1.5)), rule="evenodd")
# pushViewport(viewport(width=unit(1, "snpc"), height=unit(1, "snpc"),
#                       gp=gpar(lineheight=.9, fill=NA)))
# grid.text("light", x=unit(0, "npc") - unit(2, "mm"), just="right")
# grid.segments(0, .5,
#               unit(.17, "npc") - unit(2, "mm"), .5,
#               gp=gpar(lwd=3, fill="black"),
#               arrow=arrow(type="closed", length=unit(3, "mm")))
# grid.draw(eyeball)
# grid.define(circleGrob(r=.1), name="cornea", gp=gpar(lwd=3))
# grid.define(circleGrob(r=.07), name="lens", gp=gpar(lwd=3, fill="white"))
# pushViewport(viewport(clip=eyeballNot))
# pushViewport(viewport(x=.22, width=.5))
# grid.use("cornea")
# popViewport(2)
# pushViewport(viewport(x=.22, width=.5))
# grid.use("lens")
# popViewport()
# pushViewport(viewport(clip=rectGrob(x=.5, height=.1, just="left")))
# grid.circle(r=.28, gp=gpar(lwd=3))
# popViewport()
# grid.text("cones", x=.75, just="right")
# pushViewport(viewport(clip=polygonGrob(c(.5, 1, 1), c(.5, .65, 1.2))))
# grid.circle(r=.28, gp=gpar(lwd=3))
# popViewport()
# grid.text("rods", x=.7, y=.65, just="right")
# pushViewport(viewport(clip=polygonGrob(c(.5, 1, 1), c(.5, .3, -.2))))
# grid.circle(r=.28, gp=gpar(lwd=3))
# popViewport()
# grid.text("rods", x=.7, y=.35, just="right")
# pushViewport(viewport(clip=eyeballNot))
# grid.segments(.5, .5, 1, .35,
#               gp=gpar(lwd=3, fill="black"),
#               arrow=arrow(type="closed", length=unit(3, "mm")))
# popViewport()
# grid.text("brain\nthis\nway", y=.35,
#           x=unit(1, "npc") + unit(2, "mm"), just="left")


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
#| label: fig-extinction
#| fig-cap: The extinction illusion. Look at one corner of the image and then switch focus to a different corner of the image.  What happens to the white dots?
tile <- grobTree(rectGrob(gp=gpar(col=NA, fill="black")),
                 segmentsGrob(0, 0, 1, 1, gp=gpar(col="grey50", lwd=8)),
                 segmentsGrob(0, 1, 1, 0, gp=gpar(col="grey50", lwd=8)),
                 circleGrob(c(0, 0, .5, 1, 1), c(0, 1, .5, 1, 0),
                            r=unit(2, "mm"),
                            gp=gpar(col="black", lwd=2, fill="white")),
                 vp=viewport(width=unit(2, "cm"), height=unit(2, "cm")))
pat <- pattern(tile,
               width=unit(2, "cm"),
               height=unit(2, "cm"),
               extend="repeat")
grid.newpage()
grid.rect(gp=gpar(col=NA, fill=pat))
## grid.segments(0, .5, 1, .5, gp=gpar(col="white", lwd=170))
## grid.segments(.5, 0, .5, 1, gp=gpar(col="white", lwd=170))


## -----------------------------------------------------------------------------
#| echo: false
#| output: asis
cat(readLines("Memory/memory-1.svg", warn=FALSE), sep="\n")
cat(readLines("Memory/memory-3.svg", warn=FALSE), sep="\n")
cat(readLines("Memory/memory-5.svg", warn=FALSE), sep="\n")


## -----------------------------------------------------------------------------
#| echo: false
spread <- function(n, shift=1/n, seed=12345678) {
    set.seed(seed)
    xy <- grid(n)
    xd <- runif(n^2)
    yd <- runif(n^2)
    list(x=xy$x + shift*xd, y=xy$y + shift*yd)
}

grid <- function(n) {
    x <- rep(seq(0, 1, length.out=n), n)
    y <- rep(seq(0, 1, length.out=n), each=n)
    list(x=x, y=y)
}

resample <- function(x, ...) x[sample.int(length(x), ...)]

dots <- function(n, different=1, cols2=colsnpg, spread=FALSE, mix=FALSE, r2) {
    n2 <- n^2
    if (missing(r2)) {
        if (n < 10) {
            r2 <- c(3, 3)
        } else {
            r2 <- c(1.5, 1.5)
        }
    }
    if (spread) {
        xy <- spread(n)
        wh <- .9
    } else {
        xy <-grid(n)
        wh <- .8
    }
    draw <- function(xy, cols2, r2) {
        N <- length(xy$x)
        cols <- rep(cols2[1], N)
        r <- rep(r2[1], N)
        sub <- TRUE ## xy$x < xy$y
        i <- resample((1:N)[sub], different)
        cols[i] <- cols2[2]
        r[i] <- r2[2]
        grid.circle(xy$x, xy$y, default.units="native", r=unit(r, "mm"), 
                    gp=gpar(col=cols, fill=cols))
    }
    pushViewport(viewport(width=unit(.8, "snpc"), height=unit(.8, "snpc")))
    grid.rect()
    pushViewport(viewport(width=wh, height=wh,
                          xscale=range(xy$x), yscale=range(xy$y)))
    if (mix) {
        split <- sample(1:n2, round(n2/2))
        draw(lapply(xy, function(x) x[split]), rep(cols2[2], 2), r2)
        draw(lapply(xy, function(x) x[-split]), rep(cols2[1], 2), rev(r2))
    } else {
        draw(xy, cols2, r2)
    }
    popViewport(2)
}

mult2 <- c(1, -1)
pch2 <- c(4, 3)
lwd2 <- c(3, 3)
size2 <- c(4, 4)
len <- 1
lines <- function(n, col=FALSE, diffm=1, diffc=1, cols2=colsnpg, spread=FALSE) {
    n2 <- n^2
    if (n < 10) {
        size2 <- c(6, 6)
    } else {
        size2 <- c(4, 4)
    }
    if (spread) {
        xy <- spread(n)
        wh <- .9
    } else {
        xy <-grid(n)
        wh <- .8
    }
    mult <- rep(mult2[1], n2)
    pch <- rep(pch2[1], n2)
    lwd <- rep(lwd2[1], n2)
    size <- rep(size2[1], n2)
    sub <- TRUE ## xy$x < xy$y
    i <- resample((1:n2)[sub], diffm)
    mult[i] <- mult2[2]
    pch[i] <- pch2[2]
    lwd[i] <- lwd2[2]
    size[i] <- size2[2]
    pushViewport(viewport(width=unit(.8, "snpc"), height=unit(.8, "snpc")))
    grid.rect()
    pushViewport(viewport(width=wh, height=wh,
                          xscale=range(xy$x), yscale=range(xy$y)))
    if (col) {
        cols <- rep(cols2[1], n2)
        i <- c(resample(i, 1), resample((1:n2)[-i], diffc))
        cols[i] <- cols2[2]
    } else {
        cols <- "black"
    }
    if (FALSE) {
        grid.segments(unit(xy$x, "native") + unit(len, "mm"), 
                      unit(xy$y, "native") + mult*unit(len, "mm"), 
                      unit(xy$x, "native") - unit(len, "mm"), 
                      unit(xy$y, "native") - mult*unit(len, "mm"),
                      gp=gpar(col=cols, lwd=2))
    } else {
        grid.points(unit(xy$x, "native"), unit(xy$y, "native"),
                    pch=pch, gp=gpar(col=cols, lwd=lwd),
                    size=unit(size, "mm"))
    }
    popViewport(2)
}


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-pop-colour
#| layout-ncol: 2
#| fig.height: 6
#| fig-cap: Amongst a collection of dots, we effortlessly perceive one dot that has a different colour.
#| fig-subcap:
#|   - One teal dot amongst 24 orange dots.
#|   - One teal dot amongst 99 orange dots.
colsnpg <- pal_npg()(2)

grid.newpage()
dots(5)

grid.newpage()
dots(10)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-diff-size
#| layout-ncol: 2
#| fig.height: 6
#| fig-cap: Amongst a collection of dots, we effortlessly perceive one dot that differs only by size.
#| fig-subcap:
#|   - One larger dot amongst 24 smaller dots.
#|   - One larger dot amongst 99 smaller dots.
colssame <- rep("black", 2)

grid.newpage()
dots(5, cols2=colssame, r2=c(3, 5))

grid.newpage()
dots(10, cols2=colssame, r2=c(1.5, 3))


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-diff-salience
#| layout-ncol: 2
#| fig.height: 6
#| fig-cap: Amongst a collection of dots, we effortlessly perceive one dot that differs by both colour and size.
#| fig-subcap:
#|   - One larger teal dot amongst 24 smaller orange dots.
#|   - One larger teal dot amongst 99 smaller orange dots.
#col1 <- coords(as(sRGB(t(col2rgb(colsnpg[1])/255)), "polarLUV"))
#colssimilar <- hcl(c(col1[3], col1[3] + 10), col1[2], c(col1[1], col1[1] - 10))
colssimilar <- c(colsnpg[1], darken(colsnpg[1]))

grid.newpage()
dots(5, cols2=colsnpg, r2=c(3, 5))

grid.newpage()
dots(10, cols2=colsnpg, r2=c(1.5, 3))


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-diff-serial
#| layout-ncol: 2
#| fig.height: 6
#| fig-cap: We have to work much harder when there is more going on.
#| fig-subcap:
#|   - One larger teal dot amongst 24 dots that differ by colour and/or size.
#|   - One larger teal dot amongst 99 dots that differ by colour and/or size.
grid.newpage()
dots(5, cols2=colsnpg, r2=c(3, 5), spread=TRUE, mix=TRUE)

grid.newpage()
dots(10, cols2=colsnpg, r2=c(1.5, 3), spread=TRUE, mix=TRUE)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-gestalt-group
#| fig.height: 2
#| fig-cap: In each matrix of dots, we clearly see either rows or columns. In the pair of matrices on the left, making the dots slightly closer together horizontally produces rows, and making the dots slightly closer vertically produces columns.  In the middle pair of matrices, using the same colour on each row produces rows, and using the same colour on each column produces columns.  In the pair of matrices on the right, drawing vertical lines produces columns and drawing horizontal lines produces rows.
vp2 <- viewport(width=.9, y=unit(3, "lines"), 
                height=unit(1, "npc") - unit(6, "lines"), 
                just="bottom")
pushViewport(viewport(width=.8, 
                      layout=grid.layout(1, 5, widths=c(1, .5), 
                                         respect=TRUE),
                      gp=gpar(cex=1.5)))
pushViewport(viewport(layout.pos.col=1),
             vp2)
grid.circle(unit(.15, "npc") + rep(3.5*unit(-2:2, "mm"), 5), 
            unit(.5, "npc") + rep(5*unit(-2:2, "mm"), each=5), 
            r=unit(1, "mm"), gp=gpar(fill="black"))
grid.circle(unit(.85, "npc") + rep(5*unit(-2:2, "mm"), 7), 
            unit(.5, "npc") + rep(3.5*unit(-2:2, "mm"), each=7), 
            r=unit(1, "mm"), gp=gpar(fill="black"))
grid.text("proximity", y=unit(-2, "lines"), just="top")
popViewport(2)
pushViewport(viewport(layout.pos.col=3),
             vp2)
grid.circle(unit(.2, "npc") + rep(4*unit(-2:2, "mm"), 5), 
            unit(.5, "npc") + rep(4*unit(-2:2, "mm"), each=5), 
            r=unit(1, "mm"), 
            gp=gpar(col=rep(colsnpg, each=5), fill=rep(colsnpg, each=5)))
grid.circle(unit(.8, "npc") + rep(4*unit(-2:2, "mm"), 5), 
            unit(.5, "npc") + rep(4*unit(-2:2, "mm"), each=5), 
            r=unit(1, "mm"), 
            gp=gpar(col=rep(colsnpg, length.out=5), 
                    fill=rep(colsnpg, length.out=5)))
grid.text("similarity", y=unit(-2, "lines"), just="top")
popViewport(2)
pushViewport(viewport(layout.pos.col=5),
             vp2)
grid.circle(unit(.2, "npc") + rep(4*unit(-2:2, "mm"), 5), 
            unit(.5, "npc") + rep(4*unit(-2:2, "mm"), each=5), 
            r=unit(1, "mm"), 
            gp=gpar(fill="black"))
grid.segments(unit(.2, "npc") + 4*unit(-2:2, "mm"),
              unit(.5, "npc") + 4*unit(-2, "mm"), 
              unit(.2, "npc") + 4*unit(-2:2, "mm"),
              unit(.5, "npc") + 4*unit(2, "mm"),
              gp=gpar(lwd=1.5))
grid.circle(unit(.8, "npc") + rep(4*unit(-2:2, "mm"), 5), 
            unit(.5, "npc") + rep(4*unit(-2:2, "mm"), each=5), 
            r=unit(1, "mm"), 
            gp=gpar(fill="black"))
grid.segments(unit(.8, "npc") + 4*unit(-2, "mm"),
              unit(.5, "npc") + 4*unit(-2:2, "mm"), 
              unit(.8, "npc") + 4*unit(2, "mm"),
              unit(.5, "npc") + 4*unit(-2:2, "mm"),
              gp=gpar(lwd=2))
grid.text("connection", y=unit(-2, "lines"), just="top")
popViewport(2)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-gestalt-order
#| fig-cap: For each set of four dots, we automatically perceive two pairs of dots.  On the left, the enclosing grey rectangles produce an upper pair and a lower pair.  Second from left, the lines produce a left pair and a right pair.  Second from right, closeness produces an upper pair and a lower pair.  On the right, colour produces a left pair and a right pair.
#| fig.height: 2
cols <- rep(colsnpg, each=2)
vp2 <- viewport(width=.9, y=unit(2, "lines"), 
                height=unit(1, "npc") - unit(4, "lines"), 
                just="bottom")
pushViewport(viewport(layout=grid.layout(16, 4)))
pushViewport(viewport(layout.pos.col=1),
             vp2, viewport(width=unit(1, "snpc"), height=unit(1, "snpc")))
grid.roundrect(.5, .2, .6, .3, 
               gp=gpar(col=NA, fill="grey"))
grid.roundrect(.5, .8, .6, .3, 
               gp=gpar(col=NA, fill="grey"))
grid.segments(c(.4, .6), .2, c(.4, .6), .8,
              gp=gpar(lwd=3))
grid.circle(c(.4, .4, .6, .6), c(.2, .8, .2, .8),
            r=unit(2, "mm"), gp=gpar(fill="black"))
grid.text("enclosure", y=0, just="top")
popViewport()
grid.text(">", x=1, y=0, just="top")
popViewport(2)
pushViewport(viewport(layout.pos.col=2),
             vp2, viewport(width=unit(1, "snpc"), height=unit(1, "snpc")))
grid.segments(c(.4, .6), .2, c(.4, .6), .8,
              gp=gpar(lwd=3))
grid.circle(c(.4, .4, .6, .6), c(.2, .8, .2, .8),
            r=unit(2, "mm"), gp=gpar(fill="black"))
grid.text("connection", y=0, just="top")
popViewport()
grid.text(">", x=1, y=0, just="top")
popViewport(2)
pushViewport(viewport(layout.pos.col=3),
             vp2, viewport(width=unit(1, "snpc"), height=unit(1, "snpc")))
grid.circle(c(.4, .4, .6, .6), c(.2, .8, .2, .8),
            r=unit(2, "mm"), gp=gpar(col=cols, fill=cols))
grid.text("proximity", y=0, just="top")
popViewport()
grid.text(">", x=1, y=0, just="top")
popViewport(2)
pushViewport(viewport(layout.pos.col=4),
             vp2, viewport(width=unit(1, "snpc"), height=unit(1, "snpc")))
grid.circle(c(.4, .4, .6, .6), c(.4, .6, .4, .6),
            r=unit(2, "mm"), gp=gpar(col=cols, fill=cols))
grid.text("similarity", y=0, just="top")
popViewport(3)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-continuity
#| fig-cap: We automatically complete curves.
#| fig.height: 2
library(vwline)
curves <- function(col1="black", col2=col1, col3=col1, 
                   col4=col1, col5=col1, col6=col1, col7=col1) {
    r <- .3
    n1 <- 6
    n2 <- 1.5*n1
    step1 <- convertWidth(unit(2*r/n1, "npc"), "mm")
    step2 <- convertWidth(unit(pi*r/n2, "npc"), "mm")
    grid.brushXspline(circleBrush(), c(.5 - r, .5 - r - .2), c(.5, .5), 
                      w=unit(2, "mm"), spacing=step1, 
                      gp=gpar(col=col1, fill=col1))
    grid.brushXspline(circleBrush(), c(.5 - r, .5 + r), c(.5, .5), 
                      w=unit(2, "mm"), spacing=step1, 
                      gp=gpar(col=col2, fill=col2))
    grid.brushXspline(circleBrush(), c(.5 + r, .5 + r + .2), c(.5, .5), 
                      w=unit(2, "mm"), spacing=step1, 
                      gp=gpar(col=col3, fill=col3))
    t <- seq(0, pi, length.out=50)
    grid.draw(editGrob(brushXsplineGrob(circleBrush(), 
                                        .5 + r*cos(t), .5 + r*sin(t), 
                                        w=unit(2, "mm"), spacing=step2, 
                                        gp=gpar(col=col4, fill=col4)), 
                       tol=1e-8))
    grid.draw(editGrob(brushXsplineGrob(circleBrush(), 
                                        .5 + r*cos(t + pi), .5 + r*sin(t + pi), 
                                        w=unit(2, "mm"), spacing=step2, 
                                        gp=gpar(col=col5, fill=col5)), 
                       tol=1e-8))
    grid.circle(.5 - r, .5, r=unit(1, "mm"), 
                gp=gpar(col=col6, fill=col6))
    grid.circle(.5 + r, .5, r=unit(1, "mm"), 
                gp=gpar(col=col7, fill=col7))
}
grid.newpage()
pushViewport(viewport(layout=grid.layout(1, 7, 
                                         widths=unit(c(2, 1), c("cm", "null")), 
                                         respect=TRUE)))
pushViewport(viewport(layout.pos.col=2))
curves("grey")
popViewport()
pushViewport(viewport(layout.pos.col=4))
curves(col1=colsnpg[1], col4=colsnpg[2], col5=colsnpg[2])
popViewport()
pushViewport(viewport(layout.pos.col=6))
curves(col1=colsnpg[1], col2="grey", col3=colsnpg[1], 
       col4=colsnpg[1], col5="grey")
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-simplicity
#| fig-cap: We automatically perceive simple shapes over more complex interpretations.
#| fig.height: 2
grid.newpage()
pushViewport(viewport(layout=grid.layout(1, 7, 
                                         widths=unit(c(2, 1), c("cm", "null")), 
                                         respect=TRUE)))
pushViewport(viewport(layout.pos.col=2))
curves("grey")
popViewport()
pushViewport(viewport(layout.pos.col=4))
curves(col1=colsnpg[1], col4=colsnpg[2], col5=colsnpg[2])
popViewport()
pushViewport(viewport(layout.pos.col=6))
curves(col1=colsnpg[1], col2="grey", col3=colsnpg[2], 
       col4=colsnpg[1], col5=colsnpg[2], col7=colsnpg[2])
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-symmetry
#| fig-cap: We automatically perceive groups from symmetric arrangements.
#| fig.height: 2
library(vwline)
arc <- function(angle, offset, x=.5) {
    r <- .3
    n1 <- 6
    n2 <- 1.5*n1
    step2 <- convertWidth(unit(pi*r/n2, "npc"), "mm")
    t <- seq(-angle + offset, angle + offset, length.out=50)
    grid.draw(editGrob(brushXsplineGrob(circleBrush(), 
                                        x + r*cos(t), .5 + r*sin(t), 
                                        w=unit(2, "mm"), spacing=step2, 
                                        gp=gpar(fill="black")), 
                       tol=1e-8))
}
grid.newpage()
pushViewport(viewport(layout=grid.layout(1, 7, 
                                         widths=unit(c(2, 1), c("cm", "null")), 
                                         respect=TRUE)))
pushViewport(viewport(layout.pos.col=2))
arc(pi/2, 0, .55)
arc(pi/2, pi, .45)
## grid.segments(.5, 0, .5, 1)
popViewport()
pushViewport(viewport(layout.pos.col=4))
arc(pi/2, pi, .5)
arc(pi/2, pi, 1)
## grid.segments(.5, 0, .5, 1)
popViewport()
pushViewport(viewport(layout.pos.col=6))
arc(pi/5, 0, .5)
arc(pi/2, pi, .5)
## grid.segments(.5, 0, .5, 1)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-visual-task
#| fig-cap: On the left is a simple visual task, to estimate the relative length of the two lines.  On the right is a more complicated visual task, to decide which of the four bottom shapes is *not* a rotated version of the top shape.
#| fig.height: 2
grid.newpage()
grid.segments(.1, .2, .1, .4, gp=gpar(lwd=20, lineend="butt"))
grid.segments(.2, .2, .2, .7, gp=gpar(lwd=20, lineend="butt"))
x <- c(.2, .2, .4, .4, .6, .6, .8)
y <- c(.2, .7, .7, .4, .4, .5, .5)
pushViewport(viewport(x=1/3, width=2/3, just="left"))
grid.rect(gp=gpar(col=NA, fill="grey"))
pushViewport(viewport(x=.5, y=.75, 
                      width=unit(.3, "snpc"), height=unit(.3, "snpc")))
grid.polyline(x, y, gp=gpar(lwd=3))
popViewport()
pushViewport(viewport(x=.2, y=.3, angle=70,
                      width=unit(.3, "snpc"), height=unit(.3, "snpc")))
grid.polyline(x, y, gp=gpar(lwd=3))
popViewport()
pushViewport(viewport(x=.4, y=.3, angle=-70,
                      width=unit(.3, "snpc"), height=unit(.3, "snpc")))
grid.polyline(x, y, gp=gpar(lwd=3))
popViewport()
pushViewport(viewport(x=.6, y=.3, angle=-30,
                      width=unit(.3, "snpc"), height=unit(.3, "snpc")))
grid.polyline(1 - x, y, gp=gpar(lwd=3))
popViewport()
pushViewport(viewport(x=.8, y=.3, angle=200,
                      width=unit(.3, "snpc"), height=unit(.3, "snpc")))
grid.polyline(x, y, gp=gpar(lwd=3))
popViewport()
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| message: false
#| label: tbl-crime-group-total
#| tbl-cap: A table of the total number of offenders aged 14 to 16 from 2011 to 2021 for different ethnic groups.  
crimeGroupTotalTable <- subset(crimeGroupTotal, select=c("group", "total"))
kable_styling(kable(crimeGroupTotalTable),
              bootstrap_options=c("basic", "hover"), 
              full_width=FALSE)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-bar
#| fig-cap: A bar plot of the total number of offenders for different ethnic groups. The data symbols in this plot are the horizontal bars.
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
#| fig-cap: Some examples of basic visual features are **position**, **length**, **angle**, **area**, **colour**, and **shape**.[^list-of-features]
grid.newpage()
pushViewport(viewport(width=unit(1, "snpc"),
                      layout=grid.layout(6, 2, widths=1:2), gp=gpar(cex=1.5)))
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
#| label: fig-accuracy-demo
#| fig-cap: A demonstration of differences in accuracy between basic visual features. For each set of symbols, how much larger (or brighter) are the larger (or brighter) symbols?
colWidth <- .2
pushViewport(viewport(layout=grid.layout(1, 3)))
pushViewport(viewport(layout.pos.col=1), 
             viewport(width=.8, height=.8))
grid.rect(c(.25, .5, .75), 0, width=colWidth, height=c(.3, .6, .9), 
          just="bottom", gp=gpar(fill="black"))
popViewport(2)
pushViewport(viewport(layout.pos.col=2), 
             viewport(width=unit(.8, "snpc"), height=unit(.8, "snpc")))
grid.circle(c(.25, .5, .75), y=c(.1, .5, .9), 
            r=sqrt(c(.0125, .025, .0375)), gp=gpar(fill="black"))
popViewport(2)
pushViewport(viewport(layout.pos.col=3), 
             viewport(width=unit(.8, "snpc"), height=unit(.8, "snpc")))
regpoly <- function(x, y, n, offset=0) {
    t <- seq(0, 2*pi, length.out=n+1)[-(n+1)] + offset
    x <- x + .125*cos(t)
    y <- y + .125*sin(t)
    grid.polygon(x, y, gp=gpar(fill=NA))
}
regpoly(.25, .1, 3, 0)
regpoly(.5, .5, 6, pi/2)
regpoly(.75, .9, 9, pi/2)
popViewport(2)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-accuracy
#| fig-cap: An ordering of basic visual features in terms of their accuracy, with more accurate at the top.
grid.newpage()
pushViewport(viewport(width=unit(1, "snpc"),
                      layout=grid.layout(6, 2, widths=1:2), gp=gpar(cex=1.5)))
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
#| fig-cap: Mapping qualitative data values to colour works well for a small number of colours, but less well for a large number of colours.  It is quite easy to distinguish between the three colours on the left and even between the seven colours in the middle, but it becomes harder for the eleven colours on the right.
hpie <- function(n) {
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
hpie(3)
popViewport()
pushViewport(viewport(layout.pos.col=2))
hpie(7)
popViewport()
pushViewport(viewport(layout.pos.col=3))
hpie(11)
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
    geom_point(aes(x=total, y=group), size=3) +
    scale_x_continuous(name="total") +
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


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-bar-district
#| fig-cap: A bar plot of the number of offenders for different police districts, with the districts ordered from North to South.
crimeTemp <- crimeDistrictTotal
crimeTemp$district <- factor(crimeTemp$district,
		             levels=rev(c("Northland", "Waitematā",
			              "Auckland City",
			              "Counties/Manukau", "Waikato",
				      "Bay of Plenty",
				      "Eastern", "Central",
				      "Wellington",
				      "Tasman", "Canterbury",
				      "Southern")))
ggplot(crimeTemp) +
    geom_col(aes(x=total, y=district), width=.9) +
    scale_x_continuous(expand=expansion(c(0, .05))) +
    theme(aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-bar-district-ordered
#| fig-cap: A bar plot of the number of offenders for different police districts, with the districts ordered from largest number of offenders to smallest.
crimeDistrictTotal$districtOrdered <- 
    with(crimeDistrictTotal, reorder(district, total))
ggplot(crimeDistrictTotal) +
    geom_col(aes(x=total, y=districtOrdered), width=.9) +
    scale_x_continuous(expand=expansion(c(0, .05))) +
    scale_y_discrete(name="district") +
    theme(aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-distance-effect
#| fig-cap: Two bars are easier to compare if they are close to each other (top row).  If there is a distance between the bars and/or there are distractors (other bars) in between then comparisons are less accurate (bottom row).  On both the top and bottom row, the black bar on the left is slightly higher than the black bar on the right.
pushViewport(viewport(width=.6, layout=grid.layout(2, 1)))
pushViewport(viewport(layout.pos.row=1),
             viewport(width=.8, height=.8))
grid.segments(0, .2, 1, .2, gp=gpar(col="grey"))
grid.rect(.425, .2, .1, .6, just="bottom", gp=gpar(fill="black"))
grid.rect(.575, .2, .1, .55, just="bottom", gp=gpar(fill="black"))
popViewport(2)
pushViewport(viewport(layout.pos.row=2),
             viewport(width=.8, height=.8))
grid.segments(0, .2, 1, .2, gp=gpar(col="grey"))
grid.rect(.125, .2, .1, .6, just="bottom", gp=gpar(fill="black"))
grid.rect(seq(.275, .725, .150), .2, .1, runif(4, .2, .8), 
          just="bottom", gp=gpar(col=NA, fill="grey"))
grid.rect(.875, .2, .1, .55, just="bottom", gp=gpar(fill="black"))
popViewport(2)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-unaligned-length
#| fig-cap: It is easier to compare the lengths of the two bars on the left because they are **aligned**;  they share a common base.  The two bars on the right are harder to compare because they are **unaligned**.
pushViewport(viewport(x=0, width=.5, just="left"))
pushViewport(viewport(width=unit(.8, "snpc")))
grid.segments(.2, .1, .2, .9, gp=gpar(col="grey"))
grid.rect(.2, c(.3, .7), c(.5, .6), .2, just="left", gp=gpar(fill="black"))
popViewport(2)
pushViewport(viewport(x=.5, width=.5, just="left"))
pushViewport(viewport(x=.4, width=unit(.8, "snpc")))
grid.segments(.2, .1, .2, .5, gp=gpar(col="grey"))
grid.segments(.4, .5, .4, .9, gp=gpar(col="grey"))
grid.rect(c(.2, .4), c(.3, .7), c(.5, .6), .2, just="left",
          gp=gpar(fill="black"))
popViewport(2)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-stacked-bar
#| fig-cap: A stacked bar plot of the total number of offenders in each ethnic group.
gg <- ggplot(crimeGroupTotal) +
    geom_col(aes(total, y="", fill=group), position="stack") +
    scale_x_continuous(expand=expansion(0)) +
    scale_y_discrete(name=NULL, expand=expansion(0)) +
    theme(axis.ticks.y=element_blank())
pushViewport(viewport(width=.8, height=.6))
print(gg, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| message: false
#| label: tbl-crime-group-year
#| tbl-cap: A table of the number of offenders per year aged 14 to 16 for different ethnic groups from 2011 to 2021.  There are 44 rows of data, but just the first 6 rows are shown here.
crimeGroupTable <- subset(crimeGroup, select=c("group", "count", "year"))
kable(head(crimeGroupTable, row.names=FALSE))


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-stacked-bar-year
#| fig-cap: A stacked bar plot of the number of offenders in each ethnic group for each year from 2011 to 2021. 
ggplot(crimeGroup) +
    geom_col(aes(year, count, fill=group), position="stack") +
    scale_x_continuous(breaks=seq(2012, 2020, 2)) +    
    scale_y_continuous(expand=expansion(c(0, .05)))


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-side-bar-year
#| fig-cap: A side-by-side bar plot of the number of offenders in each ethnic group for each year from 2011 to 2021. 
ggplot(crimeGroup) +
    geom_col(aes(year, count, fill=group), position="dodge") +
    scale_x_continuous(breaks=seq(2012, 2020, 2)) +    
    scale_y_continuous(expand=expansion(c(0, .05)))


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-side-bar-group
#| fig-cap: A side-by-side bar plot of the number of offenders in each ethnic group for each year from 2011 to 2021. 
ggplot(crimeGroup) +
    geom_col(aes(group, count, fill=group, group=year), position="dodge",
             colour=figbg) +
    scale_y_continuous(expand=expansion(c(0, .05)))


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-relative
#| fig-cap: A demonstration that perception of differences is relative. The pairs of bars all differ by the same absolute amount, but the difference is more easily perceived when comparing the shorter bars.  In the shorter bars, the absolute difference between the bars is a larger difference relative to the length of the bars.
colWidth <- .2
offset <- c(0, .05)
pushViewport(viewport(height=.8, layout=grid.layout(1, 3)))
pushViewport(viewport(layout.pos.col=1), 
             viewport(width=.8, height=.8))
grid.rect(c(.35, .65), offset, width=colWidth, height=c(.1, .15), 
          just="bottom", gp=gpar(fill="black"))
popViewport(2)
pushViewport(viewport(layout.pos.col=2), 
             viewport(width=.8, height=.8))
grid.rect(c(.35, .65), offset, width=colWidth, height=c(.5, .55), 
          just="bottom", gp=gpar(fill="black"))
popViewport(2)
pushViewport(viewport(layout.pos.col=3), 
             viewport(width=.8, height=.8))
grid.rect(c(.35, .65), offset, width=colWidth, height=c(.8, .85), 
          just="bottom", gp=gpar(fill="black"))
popViewport(2)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-relative-ref
#| fig-cap: A demonstration that reference marks can make it easier to perceive small differences.  Each pair of black bars is the same and each pair differs by a small amount.  In the middle pair, grid lines make it easier to compare the distances from the ends of the bars to the nearest grid line and, in the right-hand pair, a border added to each bar, where the two borders are the same length, makes it easier to compare the white gaps rather than the bars themselves.
pushViewport(viewport(height=.8, layout=grid.layout(1, 3)))
pushViewport(viewport(layout.pos.col=1), 
             viewport(width=.8, height=.8))
grid.rect(c(.35, .65), offset, width=colWidth, height=c(.8, .85), 
          just="bottom", gp=gpar(fill="black"))
popViewport(2)
pushViewport(viewport(layout.pos.col=2), 
             viewport(width=.8, height=.8))
grid.segments(.2, seq(0, 1, .1), .8, seq(0, 1, .1), gp=gpar(col="black"))
grid.rect(c(.35, .65), offset, width=colWidth, height=c(.8, .85), 
          just="bottom", gp=gpar(fill="black"))
popViewport(2)
pushViewport(viewport(layout.pos.col=3), 
             viewport(width=.8, height=.8))
grid.rect(c(.35, .65), offset, width=colWidth, height=.95, 
          just="bottom")
grid.rect(c(.35, .65), offset, width=colWidth, height=c(.8, .85), 
          just="bottom", gp=gpar(fill="black"))
popViewport(2)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-bar-district-grid
#| fig-cap: A bar plot of the number of offenders for different police districts, with the districts ordered from North to South.  This is very similar to @fig-bar-district, just with some grid lines added.
crimeTemp <- crimeDistrictTotal
crimeTemp$district <- factor(crimeTemp$district,
		             levels=rev(c("Northland", "Waitematā",
			              "Auckland City",
			              "Counties/Manukau", "Waikato",
				      "Bay of Plenty",
				      "Eastern", "Central",
				      "Wellington",
				      "Tasman", "Canterbury",
				      "Southern")))
ggplot(crimeTemp) +
    geom_col(aes(x=total, y=district), width=.9) +
    scale_x_continuous(expand=expansion(c(0, .05))) +
    theme(panel.grid.major.x=element_line(colour="black", linewidth=.1),
          aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-pie-hue
#| fig-cap: A data visualisation of the number of offenders for different levels of crime seriousness.
ggplot(crimeLevelTotal) +
    geom_col(aes(x=total, y="", fill=level)) +
    coord_polar() +
    scale_fill_npg(guide=guide_legend(reverse=TRUE)) +
    theme(axis.title=element_blank(),
          axis.ticks=element_blank())


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-pie-ordinal
#| fig-cap: A data visualisation of the number of offenders for different levels of crime seriousness.
ggplot(crimeLevelTotal) +
    geom_col(aes(x=total, y="", fill=level), colour="black") +
    coord_polar() +
    scale_fill_ordinal(guide=guide_legend(reverse=TRUE)) +
    theme(axis.title=element_blank(),
          axis.ticks=element_blank())


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-hcl
#| fig-cap: "The perception of colour can be divided into three components: differences in **hue**, the colours of the rainbow; differences in **chroma**, from bright and colourful to dull and grey; and differences in **luminance**, from light to dark."
vp2 <- viewport(width=.9, y=unit(3, "lines"),
                height=unit(1, "npc") - unit(6, "lines"),
                just="bottom")
n <- 7
t <- seq(0, 2*pi, length.out=n+1)[-(n+1)]
hue <- t/pi*180
chroma <- 100*1:5/5
lum <- 100*1:5/6
x <- .2 + .3*cos(t + 2*pi/n)
y <- .5 + .3*sin(t + 2*pi/n)
grid.newpage()
grid.rect(width=unit(1, "npc") - unit(0/96, "in"),
          height=unit(1, "npc") - unit(0/96, "in"),
          gp=gpar(col=NA, fill="grey20"))
pushViewport(viewport(x=.55, width=.8, height=.8, layout=grid.layout(1, 3)))
pushViewport(viewport(layout.pos.col=1),
             vp2)
grid.move.to(.5, .5)
popViewport(2)
pushViewport(viewport(layout.pos.col=2),
             vp2)
grid.line.to(.5, .5, gp=gpar(col="grey40", lwd=2))
popViewport(2)
pushViewport(viewport(layout.pos.col=3),
             vp2)
grid.line.to(.5, .5, gp=gpar(col="grey40", lwd=2))
popViewport(2)
pushViewport(viewport(layout.pos.col=1),
             vp2)
pushViewport(viewport(width=unit(1, "snpc"), height=unit(1, "snpc")))
grid.circle(x, y, r=.1,
            gp=gpar(col="grey20", lwd=10))
grid.circle(x, y, r=.1,
            gp=gpar(col=figbg, fill=hcl(hue, chroma[3], lum[3]), 
                    lwd=c(rep(0, n - 1), 0)))
popViewport()
grid.text("hue", x=.2, y=unit(-2, "lines"), gp=gpar(col=figbg, fontface="bold"))
popViewport(2)
pushViewport(viewport(layout.pos.col=2),
             vp2)
grid.circle(.5, 0:4/4, r=.1,
            gp=gpar(col="grey20", lwd=10))
grid.circle(.5, 0:4/4, r=.1, 
            gp=gpar(col=figbg, fill=hcl(hue[7], chroma, lum[3]),
                    lwd=c(0,0,0,0,0)))
grid.text("chroma", y=unit(-2, "lines"), gp=gpar(col=figbg, fontface="bold"))
popViewport(2)
pushViewport(viewport(layout.pos.col=3),
             vp2)
grid.circle(.5, 0:4/4, r=.1,
            gp=gpar(col="grey20", lwd=10))
grid.circle(.5, 0:4/4, r=.1, 
            gp=gpar(col=figbg, fill=hcl(hue[7], chroma[3], lum), 
                    lwd=c(0,0,0,0,0)))
grid.text("luminance", y=unit(-2, "lines"), gp=gpar(col=figbg, fontface="bold"))
popViewport(2)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-lum-cap
#| fig-height: 3
#| fig-cap: Mapping qualitative values to luminance and chroma works well for representing nominal data values, but only for a very small number of values. The three values on the left are easily distringuishable, but it becomes difficult to clearly distinguish between adjacent colours in the middle and almost impossible for the colours on the right.  This figure can be compared with @fig-colour-cap, which demonstrates the capacity of hue for representing nominal data values.
clpie <- function(n) {
    t <- rev(seq(0, 2*pi, length.out=100*n) + pi/2)
    cols <- hcl.colors(n+1, "Purples")[-(n+1)]
    textcols <- c("white", "black")[rep(1:2, c(floor(n/3), n - floor(n/3)))]
    for (i in 1:n) {
        index <- 1:100 + (i-1)*100
        x <- cos(t[index])
        y <- sin(t[index])
        grid.polygon(c(.5, .5 + .3*x), c(.5, .5 + .3*y), 
                     gp=gpar(col=figbg, lwd=4, fill=cols[i]))
        grid.text(letters[i], .5 + mean(.25*x), .5 + mean(.25*y),
                  gp=gpar(cex=1, col=textcols[i]))
    }
}
pushViewport(viewport(layout=grid.layout(1, 3, respect=TRUE)))
pushViewport(viewport(layout.pos.col=1))
clpie(3)
popViewport()
pushViewport(viewport(layout.pos.col=2))
clpie(7)
popViewport()
pushViewport(viewport(layout.pos.col=3))
clpie(11)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-contrast-effect
#| fig-height: 3
#| fig-cap: The perception of colours is relative. In the top row, the inner rectangles are exactly the same shade of grey on the left and exactly the same shade of purple on the right.  The perception of the inner rectangles is affected by the surrounding colour, so the same shade appears darker against a light background and darker against a light background.  The bottom row draws a line connecting the two inner rectangles to show that they really are the same shade.
pushViewport(viewport(layout=grid.layout(2, 2, widths=2, respect=TRUE)))
pushViewport(viewport(layout.pos.col=1, layout.pos.row=1),
             viewport(width=.8, height=.8))
grid.rect(x=.25, width=.5, gp=gpar(col=NA, fill="grey20"))
grid.rect(x=.25, width=.25, height=.5, gp=gpar(col=NA, fill="grey60"))
grid.rect(x=.75, width=.5, gp=gpar(col=NA, fill="grey80"))
grid.rect(x=.75, width=.25, height=.5, gp=gpar(col=NA, fill="grey60"))
popViewport(2)
pushViewport(viewport(layout.pos.col=1, layout.pos.row=2),
             viewport(width=.8, height=.8))
grid.rect(x=.25, width=.5, gp=gpar(col=NA, fill="grey20"))
grid.rect(x=.25, width=.25, height=.5, gp=gpar(col=NA, fill="grey60"))
grid.rect(x=.75, width=.5, gp=gpar(col=NA, fill="grey80"))
grid.rect(x=.75, width=.25, height=.5, gp=gpar(col=NA, fill="grey60"))
grid.segments(.25, .5, .75, .5, gp=gpar(col="grey60", lwd=20))
popViewport(2)
purples <- pal_brewer(palette="Purples")(4)[-1]
pushViewport(viewport(layout.pos.col=2, layout.pos.row=1),
             viewport(width=.8, height=.8))
grid.rect(x=.25, width=.5, gp=gpar(col=NA, fill=purples[1]))
grid.rect(x=.25, width=.25, height=.5, gp=gpar(col=NA, fill=purples[2]))
grid.rect(x=.75, width=.5, gp=gpar(col=NA, fill=purples[3]))
grid.rect(x=.75, width=.25, height=.5, gp=gpar(col=NA, fill=purples[2]))
popViewport(2)
pushViewport(viewport(layout.pos.col=2, layout.pos.row=2),
             viewport(width=.8, height=.8))
grid.rect(x=.25, width=.5, gp=gpar(col=NA, fill=purples[1]))
grid.rect(x=.25, width=.25, height=.5, gp=gpar(col=NA, fill=purples[2]))
grid.rect(x=.75, width=.5, gp=gpar(col=NA, fill=purples[3]))
grid.rect(x=.75, width=.25, height=.5, gp=gpar(col=NA, fill=purples[2]))
grid.segments(.25, .5, .75, .5, gp=gpar(col=purples[2], lwd=20))
popViewport(2)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-pie-contrast
#| fig-cap: This pie chart demonstrates that the perception of hues is relative. The colours within each wedge appear to have a subtle gradient within the wedge because the perception of the wedge colour is affected differently by the neighbouring wedge colours.
ggplot(crimeDistrictTotal) + 
    geom_col(aes(x=total, y="", fill=district)) +
    coord_polar() +
    scale_fill_hue() +
    theme(aspect.ratio=1,
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank())


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-pie-contrast-solved
#| fig-cap: The hues in this pie chart are perceived more accurately than those in @fig-pie-contrast because each wedge is surrounded by a white border.
ggplot(crimeDistrictTotal) + 
    geom_col(aes(x=total, y="", fill=district), colour="white", linewidth=1) +
    coord_polar() +
    scale_fill_hue() +
    theme(aspect.ratio=1,
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank())


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-pie-hue-cvd
#| fig-cap: A data visualisation of the number of offenders for different levels of crime seriousness.
ggplot(crimeLevelTotal) +
    geom_col(aes(x=total, y="", fill=level)) +
    coord_polar() +
    scale_fill_manual(values=deutan(pal_npg()(5)),
                      guide=guide_legend(reverse=TRUE)) +
    theme(axis.title=element_blank(),
          axis.ticks=element_blank())


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-pie-ordinal-cvd
#| fig-cap: A data visualisation of the number of offenders for different levels of crime seriousness.
ggplot(crimeLevelTotal) +
    geom_col(aes(x=total, y="", fill=level), colour="black") +
    coord_polar() +
    scale_fill_manual(values=deutan(pal_brewer(palette="Purples")(5)),
                      guide=guide_legend(reverse=TRUE)) +
    theme(axis.title=element_blank(),
          axis.ticks=element_blank())


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-colour-size
#| fig-height: 3
#| fig-cap: The perception of colours varies based on the size of the coloured regions.
grid.newpage()
grid.rect(width=unit(1, "npc") - unit(0/96, "in"),
          height=unit(1, "npc") - unit(0/96, "in"),
          gp=gpar(col=NA, fill="grey20"))
cols <- c("#FFF200", "#00ADEF")
pushViewport(viewport(layout=grid.layout(2, 2)))
pushViewport(viewport(layout.pos.col=1, layout.pos.row=1),
             viewport(width=.8, height=.6, y=.4))
grid.rect(x=0:8/9, width=1/9, just="left", gp=gpar(col=NA, fill=cols))
popViewport(2)
pushViewport(viewport(layout.pos.col=1, layout.pos.row=2),
             viewport(width=.8, height=.6))
grid.rect(x=0:58/59, width=1/59, just="left", gp=gpar(col=NA, fill=cols))
popViewport(2)
x <- rep(1:5/6, each=2)
y <- c(1, 2, 4, 3, 3, 4, 3, 3, 5, 2)/6
pushViewport(viewport(layout.pos.col=2, layout.pos.row=1),
             viewport(width=.8, height=.8))
grid.rect(x=unit(x, "npc") + unit(c(-5, 0), "mm"), width=unit(5, "mm"),
          y=0, height=y, hjust=0, vjust=0,
          gp=gpar(col=NA, fill=cols))
popViewport(2)
pushViewport(viewport(layout.pos.col=2, layout.pos.row=2),
             viewport(width=.8, height=.8))
grid.lines(x[c(TRUE, FALSE)], y[c(TRUE, FALSE)], gp=gpar(col=cols[1]))
grid.lines(x[c(FALSE, TRUE)], y[c(FALSE, TRUE)], gp=gpar(col=cols[2]))
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-pie-order
#| fig-cap: The hues in this pie chart are ordered so that adjacent hues are distant from each other.
ggplot(subset(crimeDistrict, year %in% c(2011, 2021))) + 
    geom_col(aes(x=year, y=rate, fill=district), position="dodge",
             colour=figbg) +
    scale_fill_manual(values=pal_hue()(12)[t(matrix(1:12, 
                                                    ncol=2))]) +
    scale_x_continuous(breaks=c(2011, 2021)) +
    scale_y_continuous(expand=expansion(c(0, .05))) +
    theme(axis.title.x=element_blank())


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-pie-repeat
#| fig-cap: The hues in this pie chart are repeated so that adjacent hues are more distant from each other.
ggplot(subset(crimeDistrict, year %in% c(2011, 2021))) + 
    geom_col(aes(x=year, y=rate, fill=district), position="dodge",
             colour=figbg) +
    scale_fill_manual(values=rep(pal_hue()(6), 2)) +
    scale_x_continuous(breaks=c(2011, 2021)) +
    scale_y_continuous(expand=expansion(c(0, .05))) +
    theme(axis.title.x=element_blank())


## -----------------------------------------------------------------------------
#| echo: false
#| message: false
#| label: tbl-rwc-all-year
#| tbl-cap: A table of the total number of points scored and conceded by Tier One nations in Rugby World Cup matches at each World Cup.  Each row represents thetotal points scored by one team in one World Cup.  There are 100 rows in total, with just the first 6 rows shown here.
kable(head(rwcAllyear[,c("team", "year", "scored", "conceded", "diff")]), digits=0)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-neg-tile
#| fig-cap: A heatmap of the total points differentials for Tier One nations at individual Rugby World Cups.  South Africa was excluded from the 1987 and 1991 tournament due to its apartheid policies.[^gggrid-bite]
edge <- function(data, coords) {
    polygonGrob(c(0, 0,
                  coords$x[data$absent][2] + .05, 
                  coords$x[data$absent][2] + .05, 
                  0, 0, 1, 1),
                c(1, 
                  coords$y[data$absent][1] + .05, 
                  coords$y[data$absent][1] + .05, 
                  coords$y[data$absent][1] - .05, 
                  coords$y[data$absent][1] - .05,
                  0, 0, 1),
                gp=gpar(fill=NA))
}
ggplot(rwcAllyear) +
    geom_tile(aes(x=year, y=team, fill=diff)) +
    scale_fill_continuous_diverging(palette="Purple-Brown", rev=TRUE,
                                    na.value="transparent") +
    scale_x_continuous(breaks=unique(rwcAllyear$year), expand=expansion(0)) +
    scale_y_discrete(expand=expansion(0)) +
    grid_panel(edge, aes(x=year, y=team, absent=absent)) +
    theme(panel.border=element_blank(),
          aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-prop
#| fig.width: 4
#| fig-cap: Data visualisations of the proportion of offenders from different ethnic groups.  
#| fig-subcap:
#|   - A pie chart.
#|   - A bar plot.
#| layout-ncol: 2
ggplot(crimeGroupTotal) + 
    geom_col(aes(x=total/sum(total), y="", fill=group), colour="black") +
    coord_polar() +
    labs(title="Proportions of Youth Offenders 2011 to 2021") +
    theme(aspect.ratio=1,
          axis.title=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.y=element_blank())

gg <- ggplot(crimeGroupTotal) + 
    geom_col(aes(x=total/sum(total), y=group), colour="black") +
    scale_x_continuous(expand=expansion(c(0, .05))) +
    theme(aspect.ratio=1,
          axis.title=element_blank(),
          axis.ticks.y=element_blank())
grid.newpage()
pushViewport(viewport(height=.8, width=.8))
print(gg, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-bar-part
#| fig-cap: A bar plot of the proportion of offenders from different ethnic groups with reference rectangles to enhance the part-to-whole relationship.  
gg <- ggplot(crimeGroupTotal) + 
    geom_col(aes(x=1, y=group), colour="black", fill="grey80") +
    geom_col(aes(x=total/sum(total), y=group), colour="black") +
    scale_x_continuous(expand=expansion(0)) +
    theme(aspect.ratio=1,
          axis.title=element_blank(),
          axis.ticks.y=element_blank())
pushViewport(viewport(height=.8, width=.8))
print(gg, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-more-is-more
#| fig.width: 4
#| fig-cap: A bar plot and a dot plot of the total number of offenders for different ethnic groups.
#| fig-subcap:
#|   - Bars.
#|   - Dots.
#| layout-ncol: 2

gg <- ggplot(crimeGroupTotal) + 
    geom_col(aes(x=total, y=group)) +
    scale_x_continuous(expand=expansion(c(0, .05))) +
    theme(aspect.ratio=1)
grid.newpage()
pushViewport(viewport(width=.9, height=.9))
print(gg, newpage=FALSE)
popViewport()

gg <- ggplot(crimeGroupTotal) + 
    geom_segment(aes(x=-Inf, xend=Inf, y=group, yend=group), 
                 linetype="dotted") +
    geom_point(aes(x=total, y=group)) +
    scale_x_continuous(name="total") +
    theme(aspect.ratio=1)
grid.newpage()
pushViewport(viewport(width=.9, height=.9))
print(gg, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| message: false
#| label: tbl-rwc-all-time
#| tbl-cap: A table of the total number of points scored and conceded for Tier One nations in Rugby World Cup matches.  
kable(rwcAlltime[,c("team", "scored", "conceded", "diff")], digits=0)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-neg-bar
#| fig-cap: A bar plot of the total points differential for Tier One nations at Rugby World Cups.
rwcAlltime$team <- reorder(rwcAlltime$team, rwcAlltime$diff)
ggplot(rwcAlltime) +
    geom_col(aes(x=diff, y=team)) +
    scale_x_continuous(name="points differential") +
    theme(aspect.ratio=2/3)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-neg-dot
#| fig-cap: A dot plot of the total points differential for Tier One nations at Rugby World Cups.
ggplot(rwcAlltime) +
    geom_point(aes(x=diff, y=team)) +
    geom_segment(aes(x=-Inf, xend=Inf, y=team, yend=team), linetype="dotted") +
    annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, linetype="solid") +
    scale_x_continuous(name="points differential") +
    theme(aspect.ratio=2/3)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-bar-orientation
#| fig.width: 4
#| fig-cap: Data visualisations of the number of offenders from different ethnic groups.  
#| fig-subcap:
#|   - Horizontal bars.
#|   - Vertical bars.
#| layout-ncol: 2
ggplot(crimeGroupTotal) + 
    geom_col(aes(x=total, y=group)) +
    scale_x_continuous(expand=expansion(c(0, .05))) +
    scale_y_discrete(name=NULL) +
    force_panelsizes(rows = unit(2.5, "in"),
                     cols = unit(2.5, "in"))

temp <- crimeGroupTotal
temp$group <- gsub("/", "/\n", temp$group)
ggplot(temp) + 
    geom_col(aes(y=total, x=group)) +
    scale_x_discrete(name=NULL) +
    scale_y_continuous(expand=expansion(c(0, .05))) +
    force_panelsizes(rows = unit(2.5, "in"),
                     cols = unit(2.5, "in"))


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-bar-not-zero
#| fig-cap: The number of times each team entered the opposition's final third in the Women's World Cup match between the Netherlands and South Africa.[^final-third]
entries <- data.frame(team=rep(c("Netherlands", "South Africa"), each=5),
                      region=rep(1:5, 2),
                      count=c(10, 4, 1, 2, 3, 
                              4, 2, 2, 0, 2))
hue <- 120
grid.newpage()
pushViewport(viewport(width=.5, height=.9,
                      gp=gpar(lwd=1, fill=NA)),
             viewport(y=0, height=.75, just="bottom"))
grid.rect(gp=gpar(fill=hcl(hue, 70, 80)))
pushViewport(viewport(clip="on"))
grid.circle(y=0, r=10/80)
popViewport()
grid.rect(y=1, height=2/3, just="top", 
          gp=gpar(col=NA, fill=hcl(hue, 70, 60)))
grid.rect(y=1, height=2/3, just="top", 
          width=44/80, gp=gpar(col=NA, fill=hcl(hue, 70, 50)))
grid.rect(y=1, height=2/3, just="top", 
          width=20/80, gp=gpar(col=NA, fill=hcl(hue, 70, 40)))
## grid.rect(y=1, height=2/3, just="top")
grid.rect(y=1, height=6/50, width=20/80, just="top")
grid.rect(y=1, height=18/50, width=44/80, just="top")
pushViewport(viewport(clip=rectGrob(y=1 - 18/50, just="top")))
grid.circle(y=1 - 12/50, r=.2)
popViewport()
grid.circle(y=1 - 12/50, r=unit(1, "mm"), gp=gpar(fill="black"))
x <- unit(rep(c(9/80, 24/80, .5, 56/80, 71/80), 2), "npc") +
     unit(rep(c(-3, 3), each=5), "mm")
y <- 1/3 + (entries$count)/max(entries$count)*2/3
col <- rep(c("pink", "orange"), each=5)
grid.segments(x, 0, x, y, 
              arrow=arrow(type="closed"),
              gp=gpar(col=col, fill=col, lwd=1, 
                      linejoin="mitre", lineend="butt"))
grid.segments(x, 0, x, y - .05, 
              gp=gpar(col=col, fill=col, lwd=15, lineend="butt"))
grid.text(entries$count, x, unit(10, "mm"), gp=gpar(fontface="bold"))
grid.rect()
pushViewport(viewport(y=unit(1, "npc") + unit(10, "pt"), height=unit(20, "pt"),
                      just="bottom"))
grid.text("Final Third Entries",
          gp=gpar(fontsize=20))
popViewport()
pushViewport(viewport(y=unit(1, "npc") + unit(40, "pt"), height=unit(20, "pt"),
                      just="bottom"))
grid.rect(gp=gpar(fill=hcl(hue, 70, 40)))
grid.text(c("Netherlands", "South Africa"), 
          x=unit(0:1, "npc") + unit(c(2, -2), "mm"),
          hjust=c(0, 1),
          gp=gpar(col=c("pink", "orange"), fontface="bold"))
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-listener-cakes
#| fig-cap:  A data visualisation of the number of females aged over 100 relative to the number of makes aged over 100.[^listener-cakes]
#| warning: false
cake <- readPNG("Images/birthday-cake-cropped.png")
old <- data.frame(gender=c("Male", "Female"),
                  count=c(2, 5))
cakes <- function(data, coords) {
    grobTree(rasterGrob(cake, 
                        rep(coords$x, data$y + 1) + c(.05, -.05), 
                        unlist(mapply(function(x, y) {
                                          seq(0, x, length.out=y)
                                       },
                                       coords$y, data$y + 1)), 
                        just="bottom", 
                        height=.2))
}
ggplot(old) +
    geom_col(aes(x=gender, y=count), fill="transparent") +
    grid_panel(cakes, aes(x=gender, y=count - 1)) +
    scale_x_discrete(name=NULL, labels=c("Males 100+", "Females 100+")) +
    scale_y_continuous(expand=expansion(0, .05)) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major.y=element_line(linewidth=.5),
          aspect.ratio=1) +
    labs(title="For every five females age 100+,\nthere are two males aged 100+")


## -----------------------------------------------------------------------------
#| echo: false
#| message: false
#| label: tbl-rwc-all
#| tbl-cap: A table of the number of points scored by Tier One nations in Rugby World Cup matches, along with the global hemisphere of origin.  Each row represents the points scored by one team in one match.  There are 294 rows in total, with just the first 6 rows shown here.
kable(head(rwcAll[,c("team", "hemisphere", "scored")]), digits=0)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-jitter
#| fig-cap: The number of points scored in Rugby World Cup games by teams from the northern and southern hemispheres.
ggplot(rwcAll) +
    geom_jitter(aes(x=scored, y=hemisphere), height=.2, width=0, alpha=.5) +
    scale_x_continuous(name="points scored")


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-boxplot
#| fig-cap: Box plots of the number of points scored in all Rugby World Cup games by Tier One nations.
gg <- ggplot(rwcAll) +
    geom_boxplot(aes(scored, hemisphere)) +
    scale_x_continuous(name="points scored") +
    theme(axis.title.y=element_blank())
pushViewport(viewport(width=.8, height=.8))
print(gg, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| message: false
#| label: tbl-rwc-all-stats
#| tbl-cap: A table of the five-number summaries for the number of points scored by Tier One nations in Rugby World Cup matches, along with the country name.  These data summaries are the values that are mapped to visual features to produce the box plots in @fig-boxplot.
rwcStats <- do.call(rbind, 
                    lapply(split(rwcAll, rwcAll$hemisphere), 
                           function(x) fivenum(x$scored)))
colnames(rwcStats) <- c("minimum", "lower quartile", "median", 
                        "upper quartile", "maximum")
kable(rwcStats, digits=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-rwc-dotplot
#| fig-cap: Stacked dotplots of the number of points scored by Tier One nations at Rugby World Cup matches.
gg <- ggplot(rwcAll) +
    geom_segment(aes(x=-Inf, xend=Inf, y=hemisphere, yend=hemisphere),
                 colour="grey", linewidth=.1) +
    geom_dotplot(aes(scored, hemisphere), binwidth=1, dotsize=.8, fill=NA) +
    scale_x_continuous(name="points scored", breaks=seq(0, 100, 10)) +
    theme(axis.title.y=element_blank())
pushViewport(viewport(width=.8, height=.8))
print(gg, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-hist
#| fig-cap: A histogram of the points scored per game by Tier One nations at Rugby World Cups.
breaks <- seq(0, 105, by=5)
gg <- ggplot(rwcAll) +
    geom_histogram(aes(scored), colour="black", fill="grey", breaks=breaks) +
    scale_x_continuous(name="points scored") +
    scale_y_continuous(expand=expansion(c(0, .05)))
pushViewport(viewport(height=.8, width=.8))
print(gg, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| message: false
#| label: tbl-hist-stats
#| tbl-cap: A table of intervals the cover the range of the number of points scored by Tier One nations in Rugby World Cup matches, along with the count of the number of points scored that fall within each interval.  These data summaries are the values that are mapped to visual features to produce the histogram in @fig-boxplot.
hist <- hist(rwcAll$scored, breaks=breaks, plot=FALSE)
histStats <- rbind(interval=paste0(hist$breaks[-length(hist$breaks)], "-",
                                   hist$breaks[-1]),
                   count=hist$counts)
kable(histStats, align="r")


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-boxplot-concede
#| fig-cap: Box plots of the number of points conceded in all Rugby World Cup games by Tier One nations.
gg <- ggplot(rwcAll) +
    geom_boxplot(aes(conceded, hemisphere)) +
    scale_x_continuous(name="points conceded") +
    theme(axis.title.y=element_blank())
pushViewport(viewport(width=.8, height=.8))
print(gg, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-rwc-dotplot-concede
#| fig-cap: Stacked dotplots of the number of points conceded by Tier One nations at Rugby World Cup matches.
gg <- ggplot(rwcAll) +
    geom_segment(aes(x=-Inf, xend=Inf, y=hemisphere, yend=hemisphere),
                 colour="grey", linewidth=.1) +
    geom_dotplot(aes(conceded, hemisphere), binwidth=1, dotsize=.8, fill=NA) +
    scale_x_continuous(name="points conceded", breaks=seq(0, 100, 10)) +
    theme(axis.title.y=element_blank())
pushViewport(viewport(width=.8, height=.8))
print(gg, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-hist-thin
#| fig-cap: A histogram of the points scored per game by Tier One nations at Rugby World Cups.  This histogram uses the same data as @fig-hist, but bins the data using narrower intervals.
gg <- ggplot(rwcAll) +
    geom_histogram(aes(scored), colour="black", fill="grey", 
                   binwidth=1, boundary=.5) +
    scale_x_continuous(name="points scored", breaks=seq(0, 100, 10)) 
pushViewport(viewport(height=.8, width=.8))
print(gg, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| warning: false
#| label: fig-bars-at-zero
#| fig-cap: A bar plot of the increase in the top tax rate in the United States of America from President George W. Bush to President Barack Obama.
years <- c("Now", "Jan. 2013")
tax <- data.frame(year=factor(years, levels=years),
                  rate=c(35, 39.6))
barlabs <- function(data, coords) {
    grobTree(textGrob(paste0(data$y[1], "%"), x=coords$x[1],
                      y=unit(coords$y, "npc") + unit(1, "lines")),
             textGrob("?", x=coords$x[2],
                      gp=gpar(col=figbg, fontsize=100, fontface="bold")))
}
ggplot(tax) +
    geom_col(aes(year, rate, fill=year)) +
    scale_fill_manual(values=c("black", highlight)) +
    grid_panel(segmentsGrob(0, 0, 1, 0, gp=gpar(lwd=3),
               vp=viewport(clip=FALSE))) +
    grid_panel(textGrob("If Bush\ntax cuts\nexpire",
               x=0, y=1, just=c(0, 1),
               gp=gpar(fontsize=30, fontface="bold", lineheight=.8))) +
    grid_panel(textGrob("Top tax rate:",
               x=0, y=.5, just="left",
               gp=gpar(fontsize=20, fontface="bold"))) +
    grid_panel(barlabs, aes(year, rate)) +
    coord_cartesian(ylim=c(34, NA)) +
    scale_y_continuous(expand=expansion(0)) +
    theme_minimal() +
    theme(plot.margin=unit(c(2, 0, 1, 0), "lines"),
          panel.grid=element_blank(),
          title=element_blank(),
          axis.text.y=element_blank(),
          axis.text.x=element_text(size=20, face="bold", 
                                   colour=c("black", highlight)),
          legend.position="none",
          aspect.ratio=.8)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-cog-load
#| fig-cap: A bar plot comparing two fractions.
fractions <- data.frame(year=factor(c(1950, 2019)), frac=c(2/3, 1/11))
ggplot(fractions) +
    geom_col(aes(year, frac)) +
    scale_y_continuous(expand=expansion(c(0, .05))) +
    xlab(NULL) +
    ylab(NULL) +
    labs(title="Proportion of Population Suffering from Malnutrition") +
    theme(aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-cog-load-hi
#| fig-cap: A bar plot comparing two fractions.
counts <- data.frame(year=factor(c(1950, 1950, 2019, 2019)), 
                     group=factor(c("yes", "out of", "yes", "out of"),
                                  levels=c("out of", "yes")),
                     count=c(2, 3, 1, 11))
fills <- pal_npg()(2)
ggplot(counts) +
    geom_col(aes(year, count, fill=group), position="dodge") +
    scale_y_continuous(expand=expansion(c(0, .05))) +
    xlab(NULL) +
    ylab(NULL) +
    labs(title=paste0('<span style="color: ', fills[2], '">X</span> ',
                      'people out of ',
                      '<span style="color: ', fills[1], '">Y</span> ',
                      'suffer from Malnutrition')) +
    theme(legend.position="none",
          aspect.ratio=1,
          plot.title=element_markdown())


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-bar-colour
#| fig-cap: A bar plot of the total number of offenders for different ethnic groups. The data symbols in this plot come from mapping ethnic group to the vertical positions and colours of the bars and mapping the total number of offenders to the lengths of the bars.
ggplot(crimeGroupTotal) + 
    geom_col(aes(x=total, y=group, fill=group)) +
    scale_x_continuous(expand=expansion(c(0, .05))) +
    theme(aspect.ratio=1,
          legend.position="none")


## -----------------------------------------------------------------------------
#| echo: false
#| message: false
#| label: tbl-rwc-2023
#| tbl-cap: Performance measures for all twenty teams at the 2023 Rugby World Cup. Each measure is a per-game average because some teams played more games than others.
kable(RWCperGame, digits=1, row.names=FALSE)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-scatter
#| fig-cap: A scatter plot of the number of times a team breaks through the opposition defence and the number of tries that a team scores (both are per-game averages) for teams at the 2023 Rugby World Cup.
gg <- ggplot(RWCperGame) +
    geom_point(aes(breaks, tries)) +
    scale_x_continuous(name="clean breaks") +
    scale_y_continuous(name="tries scored") +
    theme(aspect.ratio=1)
pushViewport(viewport(height=.8))
print(gg, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: tbl-offenders
#| tbl-cap: Counts of offences committed in New Zealand in 2021 by the sex of the offender and the action taken against the offender. 
offenderTable <- with(offenders, table(Mop.Division, SEX))
kable(offenderTable)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-spine
#| warning: false
#| fig-cap: A spine plot
offenders$action <- factor(offenders$Mop.Division, 
                           levels=rev(sort(unique(offenders$Mop.Division))))
gg <- ggplot(offenders) +
    geom_mosaic(aes(x=product(action, SEX), fill=action)) +
    scale_fill_npg(guide=guide_legend(reverse=TRUE)) +
    coord_cartesian(expand=FALSE) +
    theme(panel.border=element_blank(),
          axis.title=element_blank(),
          legend.title=element_blank(),
          aspect.ratio=1)
pushViewport(viewport(width=.8, height=.8))
print(gg, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: tbl-offenders-summary
#| tbl-cap: The data summaries calculated from @tbl-offenders that are encoded as the widths and heights of the rectangles in @fig-spine.
#| tbl-subcap:
#|   - Widths
#|   - Heights
#| layout-nrows: 2
widths <- proportions(t(marginSums(offenderTable, 2)), 1)
heights <- proportions(offenderTable, 2)
props <- cbind(matrix(rep(widths, 3), nrow=3, byrow=TRUE), heights)
colnames(props) <- rep(colnames(props)[3:4], 2)
kable(props[,1:2], digits=2)
kable(props[,3:4], digits=2)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-spine-weak
#| warning: false
#| fig-cap: A spine plot with problems
#| fig-keep: last
actions <- c("Prosecution", "Formal Warning", "Informal Warning",
             "Community Justice Panel", "Alternative Action Plan",
             "No Further Action")
offendersMain <- subset(offenders, 
                        !(Age.Group %in% 
                          c("0-4", "5-9", "80yearsorover", "NotSpecified")) &
                        (Mop.Group %in% actions),
                        drop=TRUE)
offendersMain$Age.Group <- gsub("([0-9]+).+", "\\1", offendersMain$Age.Group)
offendersMain$Age.Group <- factor(offendersMain$Age.Group)
offendersMain$Mop.Group <- factor(offendersMain$Mop.Group, levels=rev(actions))
gg <- ggplot(offendersMain) +
    geom_mosaic(aes(x=product(Mop.Group, Age.Group), fill=Mop.Group)) +
    coord_cartesian(expand=FALSE) +
    scale_fill_npg(guide=guide_legend(reverse=TRUE)) +
    theme(panel.border=element_blank(),
          axis.title=element_blank(),
          legend.title=element_blank(),
          axis.text.x=element_text(size=6),
          aspect.ratio=.8)
pushViewport(viewport(width=1, height=1))
print(gg, newpage=FALSE)
popViewport()
grid.force()
grid.edit("axis.1-2-1-2::titleGrob::text", grep=TRUE, vjust=c(.5, .5, 1, 0, .5, .5))
grid.edit("axis.3-1-3-1::titleGrob::text", grep=TRUE, hjust=c(rep(.5, 13), .3))


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-mosaic
#| warning: false
#| fig-cap: A mosaic plot of three variables
offenders$ageCat <- ifelse(offenders$youth, "Youth", "Adult")
gg <- ggplot(offenders) +
    geom_mosaic(aes(x=product(action, SEX, ageCat), fill=action),
                divider=mosaic("v")) +
    scale_fill_npg(guide=guide_legend(reverse=TRUE)) +
    coord_cartesian(expand=FALSE) +
    theme(panel.border=element_blank(),
          axis.title=element_blank(),
          legend.title=element_blank(),
          aspect.ratio=1)
pushViewport(viewport(width=1, height=1))
print(gg, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-area-shape
#| fig-cap: Three different shapes that all have the same area.
#| fig-height: 3
grid.newpage()
pushViewport(viewport(layout=grid.layout(1, 3, respect=TRUE)))
pushViewport(viewport(layout.pos.col=1))
grid.rect(width=.8, height=.2, gp=gpar(fill="black"))
popViewport()
pushViewport(viewport(layout.pos.col=2))
grid.rect(width=.4, height=.4, gp=gpar(fill="black"))
popViewport()
pushViewport(viewport(layout.pos.col=3))
grid.rect(width=.2, height=.8, gp=gpar(fill="black"))
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: tbl-interaction
#| tbl-cap: The number of offenders in different ethnic groups in 2011 and in 2021.
crimeGroupSub <- subset(crimeGroup, year %in% c(2011, 2021))
kable(crimeGroupSub[c("group", "year", "count")],
      row.names=FALSE)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-interaction
#| fig-cap: An interaction plot showing the change in the number of offenders in different ethnic groups between 2011 and 2021.
ggplot(crimeGroupSub) +
    geom_line(aes(x=year, y=count, colour=group)) +
    scale_x_continuous(name=NULL, breaks=c(2011, 2021)) +
    theme(aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-interaction-bar
#| fig-cap: A bar plot showing the number of offenders in different ethnic groups in 2011 and in 2021.
ggplot(crimeGroupSub) +
    geom_col(aes(x=year, y=count, fill=group), position="dodge") +
    scale_x_continuous(name=NULL, breaks=c(2011, 2021)) +
    scale_y_continuous(expand=expansion(c(0, .05))) +
    theme(aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-nz-doctor-bar
#| fig-cap: A plot showing the differences in health spending for successive New Zealand governments using filled bars.
nzdoctor <- data.frame(year=c(1999, 1999:2023), 
                       block=rep(1:5, c(1, 9, 9, 6, 1)),
                       percent=rep(c(0, 4.7, 1.3, 4.6, -3), c(1, 9, 9, 6, 1)),
                       party=rep(rep(c(NA, rep(c("Labour", "National"), 2)),
                                 c(1, 9, 9, 6, 1))))
block <- function(data, coords) {
    blockCoords <- split(coords, coords$block)
    zero <- blockCoords[[1]]$y
    step <- diff(coords$x)[2]
    blockRect <- function(x) {
        l <- x$x[1]
        r <- l + diff(range(x$x)) + step
        b <- zero
        t <- x$y[1]
        polygonGrob(c(l, l, r, r), c(b, t, t, b),
                    gp=gpar(col=NA, fill=x$colour[1]))
    }
    blocks <- lapply(blockCoords[-1], blockRect)
    do.call(grobTree, blocks)
}
blockYear <- function(data, coords) {
    zero <- coords$y[1]
    step <- diff(coords$x)[2]
    years <- data$x[-1]
    textGrob(paste0(" ", years, "-", substring(years + 1, 3, 4)),
             coords$x[-1] + step/2, zero, hjust=0, rot=-90,
             gp=gpar(fontsize=10))
}
partyColours <- c(rgb(217, 42, 31, max=255),
                  rgb(46, 140, 204, max=255))
ggDoctor <- ggplot(nzdoctor) +
    grid_panel(block, aes(year, percent, colour=party, block=block)) +
    grid_panel(blockYear, aes(year, percent)) +
    scale_y_continuous(limits=c(-4, 6), expand=expansion(0), 
                       breaks=-4:6, 
                       labels=paste0(sprintf("%1.1f", -4:6), "%")) +
    scale_x_continuous(expand=expansion(add=c(0, 1))) +
    scale_colour_manual(values=partyColours) +
    labs(title=paste("New Zealand Real Terms", 
                     "Average Annual Health Spend Change Per Person",
                     sep="\n")) +
    theme(panel.border=element_blank(),
          axis.ticks=element_blank(),
          axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
pushViewport(viewport(width=.9, height=.9))
print(ggDoctor, newpage=FALSE)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-nz-doctor-line
#| fig-cap: A plot showing the differences in health spending for successive New Zealand governments using horizontal lines.
seg <- function(data, coords) {
    segCoords <- split(coords, coords$seg)
    zero <- segCoords[[1]]$y
    step <- diff(coords$x)[2]
    segLine <- function(x) {
        l <- x$x[1]
        r <- l + diff(range(x$x)) + step
        y <- x$y[1]
        grobTree(## segmentsGrob(l, zero, l, y,
                 ##              gp=gpar(col=x$colour[1], lty="dotted")),
                 segmentsGrob(l, y, r, y, 
                              gp=gpar(col=x$colour[1], lwd=3)),
                 circleGrob(l, y, r=unit(1, "mm"), 
                            gp=gpar(col=x$colour[1], fill=x$colour[1], 
                                    lwd=3)),
                 circleGrob(r, y, r=unit(1, "mm"),
                            gp=gpar(col=x$colour[1], fill=figbg, lwd=3)))
    }
    segs <- lapply(segCoords[-1], segLine)
    do.call(grobTree, segs)
}
segNeg <- function(data, coords) {
    zero <- coords$y[1]
    polygonGrob(c(0, 0, 1, 1), c(0, zero, zero, 0),
             gp=gpar(col=NA, fill="white"))
}
segYear <- function(data, coords) {
    step <- diff(coords$x)[2]
    years <- data$x[-1]
    textGrob(paste0(" ", years, "-", substring(years + 1, 3, 4)),
             coords$x[-1] + step/2, 0, hjust=0, rot=-90,
             gp=gpar(fontsize=10))
}
border <- segmentsGrob(0, 0:1, 1, 0:1)
ggDoctorLine <- ggplot(nzdoctor) +
    grid_panel(segNeg, aes(year, percent)) +
    grid_panel(seg, aes(year, percent, colour=party, seg=block)) +
    geom_hline(yintercept=0, colour="grey") +
    grid_panel(segYear, aes(year, percent)) +
    ## grid_panel(border) +
    scale_y_continuous(limits=c(-4, 6), expand=expansion(0), 
                       breaks=-4:6, 
                       labels=paste0(sprintf("%1.1f", -4:6), "%")) +
    scale_x_continuous(expand=expansion(add=c(0, 1))) +
    scale_colour_manual(values=partyColours) +
    labs(title=paste("New Zealand Real Terms", 
                     "Average Annual Health Spend Change Per Person",
                     sep="\n")) +
    theme(panel.border=element_blank(),
          axis.ticks=element_blank(),
          axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank()) +
    coord_cartesian(clip="off")
pushViewport(viewport(width=.9, height=.9),
             viewport(y=1, 
                      height=unit(1, "npc") - 
                             grobWidth(textGrob(" 0000-00",
                                                gp=gpar(fontsize=10))), 
                      just="top"))
print(ggDoctorLine, newpage=FALSE)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-line
#| fig-cap: A line plot of the number of offenders per year in different ethnic groups from 2011 to 2021.
gg <- ggplot(crimeGroup) +
    geom_line(aes(year, count, colour=group)) +
    scale_x_continuous(name=NULL, breaks=seq(2012, 2020, 2)) +    
    scale_colour_npg(name="ethnic group") +
    theme(aspect.ratio=1)
pushViewport(viewport(width=.8, height=.8))
print(gg, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-line-points
#| fig-cap: A plot of the number of offenders per year in different ethnic groups from 2011 to 2021 with both lines and points.
gg <- ggplot(crimeGroup) +
    geom_line(aes(year, count, colour=group)) +
    geom_point(aes(year, count, colour=group), 
               pch=21, fill=figbg, stroke=1) +
    scale_x_continuous(name=NULL, breaks=seq(2012, 2020, 2)) +    
    scale_colour_npg(name="ethnic group") +
    theme(aspect.ratio=1)
pushViewport(viewport(width=.8, height=.8))
print(gg, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-density
#| fig-cap: A density plot of the points scored per game by Tier One nations at Rugby World Cups.
gg <- ggplot(rwcAll) +
    geom_density(aes(scored), colour="black", linewidth=1) +
    scale_x_continuous(name="points scored") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
pushViewport(viewport(height=.8, width=.8))
print(gg, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| message: false
#| label: tbl-density-stats
#| tbl-cap: A table of density estimates at a sample of values over the range of the number of points scored by Tier One nations in Rugby World Cup matches.  These data summaries are the values that are mapped to visual features to produce the density plot in @fig-density.  There are 512 rows of values, but only the first 6 are shown here.
d <- density(rwcAll$scored, from=min(rwcAll$scored), to=max(rwcAll$scored))
dStats <- cbind(scored=d$x, density=d$y)
kable(head(dStats), digits=c(2, 4))


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-multiple-density
#| fig-cap: Density plots of the points scored per game by Tier One nations at Rugby World Cups, broken down by hemisphere.
gg <- ggplot(rwcAll) +
    geom_density(aes(scored, colour=hemisphere), linewidth=1) +
    scale_x_continuous(name="points scored") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
pushViewport(viewport(height=.8, width=.8))
print(gg, newpage=FALSE)
popViewport()

## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-multiple-hist
#| fig-cap: Histograms of the points scored per game by Tier One nations at Rugby World Cups, broken down by hemisphere.
breaks <- seq(0, 105, by=5)
gg <- ggplot(rwcAll) +
    geom_histogram(aes(scored, colour=hemisphere, fill=hemisphere), 
                   breaks=breaks, alpha=.2, position="identity") +
    scale_x_continuous(name="points scored") +
    scale_y_continuous(expand=expansion(c(0, .05)))
pushViewport(viewport(height=.8, width=.8))
print(gg, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-multiple
#| fig-cap: A data visualisation of the performance measures for teams in the 2023 Rugby World Cup.
ggplot(RWCperGame) + 
    geom_point(aes(x=breaks, y=tries, 
               shape=sphere, size=points, fill=runs)) +
    scale_x_continuous(name="clean breaks") +
    scale_y_continuous(name="tries scored") +
    scale_shape_manual(values=c(21, 24)) +
    scale_size_continuous(guide=guide_legend(position="top")) +
    theme(aspect.ratio=1,
          legend.frame=element_rect(colour="black", linewidth=.2))


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-parallel
#| fig-cap: A data visualisation of the performance measures for teams in the 2023 Rugby World Cup.
gg <- ggparcoord(RWCperGame, 
           columns=c(11, 5, 10, 7), groupColumn="sphere",
           scale="uniminmax", alpha=.5) +
    scale_x_discrete(expand=expansion(.02)) +
    scale_colour_discrete(guide=guide_legend(reverse=TRUE))
pushViewport(viewport(width=.8, height=.8))
print(gg, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| message: false
#| label: tbl-parallel-stats
#| tbl-cap: The data summaries that are mapped to visual features in @fig-parallel.
vars <- subset(RWCperGame, 
               select=c("country", "sphere", 
                        "runs", "breaks", "tries", "points"))
vars[3:6] <- apply(vars[3:6], 2, 
                   function(x) {
                       (x - min(x))/diff(range(x))
                   })
vars$country <- reorder(vars$country, vars$points)
long <- melt(vars, id.vars=1:2)
kable(head(long), digits=2)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-profile
#| fig-cap: A data visualisation of the performance measures for teams in the 2023 Rugby World Cup.
gg <- ggplot(long) +
    geom_area(aes(as.numeric(variable), value, fill=sphere)) +
    scale_x_continuous(expand=expansion(0)) +
    scale_y_continuous(expand=expansion(0)) +
    coord_cartesian(clip="off") +
    facet_wrap(vars(country), strip.position="bottom") +
    theme(panel.border=element_blank(),
          strip.background=element_blank(),
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          aspect.ratio=1)
pushViewport(viewport(width=1, height=1))
print(gg, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-facet-before
#| fig-cap: A data visualisation of the number of offenders per year in different Police Districts of New Zealand, from 2011 to 2021.
ggplot(crimeDistrict) +
    geom_line(aes(year, count, colour=district)) +
    scale_x_continuous(breaks=seq(2012, 2020, 2)) +    
    scale_colour_manual(values=carto_pal(12, "Safe")) +
    theme(aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-facet-after
#| fig-cap: A data visualisation of the number of offenders per year in different Police Districts of New Zealand, from 2011 to 2021.
crimeDistrict$district <- 
    reorder(crimeDistrict$district, crimeDistrict$count, max)
crime2 <- crimeDistrict
crime2$district2 <- crime2$district
crime2$district <- NULL
ggplot(crimeDistrict) +
    geom_line(aes(year, count, group=district2), data=crime2, 
              col="grey", linewidth=.2) +
    geom_line(aes(year, count)) +
    scale_x_continuous(breaks=seq(2012, 2020, 4)) +    
    facet_wrap(vars(district)) +
    theme(aspect.ratio=.8)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-splom
#| fig-cap: A scatter plot matrix of the quantitative performance measures for teams in the 2023 Rugby World Cup.
quants <- c("runs", "breaks", "tries", "points")
mat <- subset(RWCperGame, 
              select=c(quants, "sphere"))
## https://stackoverflow.com/questions/3735286/create-a-matrix-of-scatterplots-pairs-equivalent-in-ggplot2
mat <- mutate(mat, id = rownames(mat)) 

## Prepare data to be plotted on the x axis
x_vars <- pivot_longer(data = mat,
             cols = 1:4,
             names_to = "variable_x",
             values_to = "x")

## Prepare data to be plotted on the y axis  
y_vars <- pivot_longer(data = mat,
                       cols = 1:4,
                       names_to = "variable_y",
                       values_to = "y") 

## Join data for x and y axes and make plot
all <- full_join(x_vars, y_vars, 
          by = c("id", "sphere"),
          relationship = "many-to-many")
all$variable_x <- factor(all$variable_x, levels=quants)
all$variable_y <- factor(all$variable_y, levels=quants)
all <- subset(all, 
              variable_x != variable_y &
              variable_x != "points" &
              variable_y != "runs" &
              !(variable_x == "tries" & variable_y == "breaks"))
ggplot(all) + 
    aes(x = x, y = y, color = sphere) +
    geom_point() +
    facet_grid(variable_y ~ variable_x, scales="free") +
    theme(axis.title=element_blank(),
          aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-scatter-country
#| fig-cap: A scatter plot with a different shape per country
ggplot(RWCperGame) +
    geom_point(aes(breaks, tries, shape=country), fill="grey", size=3) +
    scale_shape_manual(values=(1:25)[-c(15:18, 20)], 
                       guide=guide_legend(ncol=2), name=NULL) +
    theme(aspect.ratio=2/3)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-flags
#| fig-cap: A scatter plot with flags as data symbols
size <- 10
flagGroup <- function(data, coords) {
    width <- unit(size, "mm")
    height <- data$ar * width
    grobTree(rectGrob(coords$x, coords$y, 
                      width, height, 
                      gp=gpar(fill="white")),
             rasterGrob(readPNG(data$path), 
                        coords$x, coords$y, width))
}
flag_key <- function(data, ...) {
    i <- data$shape
    data <- rwcFlags[i, ]
    width <- unit(size, "mm")
    height <- data$ar * width
    grobTree(rectGrob(.5, .5,
                      width, height, 
                      gp=gpar(fill="white")),
             rasterGrob(readPNG(data$path), 
                        .5, .5, width))
}
ggplot(rwcFlags) +
    grid_group(flagGroup, 
               aes(breaks, tries, path=path, ar=ar, 
                   shape=country),
               inherit.aes=FALSE,
               show.legend=TRUE, key_glyph=flag_key) +
    scale_shape_manual(values=1:nrow(rwcFlags), guide=guide_legend(ncol=2),
                       name=NULL) +
    theme(legend.key.width=unit(size, "mm"),
          legend.key.height=unit(max(rwcFlags$ar)*size + 1, "mm"),
          aspect.ratio=2/3)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-learned
#| fig.width: 4
#| fig.height: 1
#| fig-cap: An example of visual symbols that have pre-existing, learned decodings.  
## https://www.color-hex.com/color-palette/35021
traffic <- c("#cc3232", "#e7b416", "#2dc937")
grid.newpage()
grid.roundrect(width=unit(10, "mm"), height=.9, gp=gpar(fill="black"))
grid.circle(y=c(.75, .5, .25), r=unit(2, "mm"),
            gp=gpar(col=traffic, fill=traffic))


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-flags-direct
#| fig-cap: A scatter plot with flags as data symbols
flagPanel <- function(data, coords) {
    width <- unit(size, "mm")
    height <- data$ar * width
    flags <- lapply(1:nrow(data),
                    function(i) {
                        grobTree(rectGrob(coords$x[i], coords$y[i], 
                                          width, height[i], 
                                          gp=gpar(fill="white")),
                                 rasterGrob(readPNG(data$path[i]), 
                                            coords$x[i], coords$y[i], width))
                    })
    do.call(grobTree, flags)
}
ggplot(rwcFlags) +
    grid_panel(flagPanel, 
               aes(breaks, tries, path=path, ar=ar)) +
    theme(aspect.ratio=2/3)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-scatter-country-no-legend
#| fig-cap: A scatter plot with a different shape per country
ggplot(RWCperGame) +
    geom_point(aes(breaks, tries, shape=country), fill="grey", size=3) +
    scale_shape_manual(values=(1:25)[-c(15:18, 20)], 
                       guide="none") +
    theme(aspect.ratio=2/3)


## -----------------------------------------------------------------------------
#| echo: false
#| label: tbl-map-data
#| tbl-cap: The average crime rate over the period 2011 to 2021 for each police district of New Zealand.
kable(crimeDistrictTotal[c("district", "rate")], digits=2)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-map
#| fig-cap: A choropleth map of the average crime rate over the period 2011 to 2021 for each police district.
districts <- 
    st_read("Data/YouthCrime/SHP/nz-police-district-boundaries-29-april-2021.shp",
            quiet=TRUE)
centroids <- st_coordinates(st_centroid(st_geometry(districts)))
districts$X <- centroids[,1]
districts$Y <- centroids[,2]
districts <- inner_join(districts, crimeDistrictTotal,
                        by=join_by(D_MACRON == district))
ggplot(districts) +
    geom_sf(aes(fill=rate)) +
    scale_fill_continuous(name="crime rate") +
    theme(axis.ticks=element_blank(),
          axis.text=element_blank())


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-district-bar
crimeDistrictBar <- crimeDistrictTotal
crimeDistrictBar$district <- 
    reorder(crimeDistrictBar$district, 
            crimeDistrictBar$total/crimeDistrictBar$avgPop)
ggplot(crimeDistrictBar) +
    geom_col(aes(total/avgPop, district)) +
    scale_x_continuous(name="crime rate", expand=expansion(c(0, .05))) +
    scale_y_discrete(name=NULL) + 
    theme(aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-map-alt
#| layout-ncol: 2
#| fig-cap: Alternative map-based representations of the average crime rate over the period 2011 to 2021 for each police district.
#| fig.width: 4
#| fig-subcap:
#|   - Dots as data symbols.
#|   - Bars as data symbols.
ggplot(districts) +
    geom_sf() +
    geom_point(aes(X, Y, size=rate)) +
    scale_fill_continuous(name="crime rate") +
    theme(axis.title=element_blank(),
          axis.ticks=element_blank(),
          axis.text=element_blank())

w <- 2
h <- 4
therm <- function(data, coords) {
    grobTree(rectGrob(coords$x, coords$y, 
                      width=unit(w, "mm"), height=unit(h, "mm"),
                      just="bottom",
                      gp=gpar(fill="white")),
             rectGrob(coords$x, coords$y, 
                      width=unit(w, "mm"), 
                      height=unit(h*data$height/max(data$height), "mm"),
                      just="bottom",
                      gp=gpar(fill="black")))
}
thermKey <- function(data, coords) {
    vp <- vpTree(viewport(.8, .2,
                          layout=grid.layout(5, 1, 
                                             widths=unit(1, "cm"),
                                             heights=unit(h, "mm")),
                          gp=gpar(fontsize=9),
                          name="layout"),
                 vpList(vpStack(viewport(layout.pos.row=5, name="c1"),
                                viewport(x=0, width=unit(w, "mm"), name="k1")),
                        vpStack(viewport(layout.pos.row=3, name="c2"),
                                viewport(x=0, width=unit(w, "mm"), name="k2")),
                        vpStack(viewport(layout.pos.row=1, name="c3"),
                                viewport(x=0, width=unit(w, "mm"), name="k3"))))
    grobTree(rectGrob(gp=gpar(fill="white"), 
                      vp="layout::c1::k1"),
             textGrob(0, unit(1, "npc") + unit(1, "mm"), hjust=0,
                      vp="layout::c1::k1"),
             rectGrob(gp=gpar(fill="white"), 
                      vp="layout::c2::k2"),
             rectGrob(y=0, height=.5, just="bottom",
                      gp=gpar(fill="black"), 
                      vp="layout::c2::k2"),
             textGrob(round(max(districts$rate)/2, 2), 
                      unit(1, "npc") + unit(1, "mm"), hjust=0,
                      vp="layout::c2::k2"),
             rectGrob(gp=gpar(fill="black"), 
                      vp="layout::c3::k3"),
             textGrob(round(max(districts$rate), 2), 
                      unit(1, "npc") + unit(1, "mm"), hjust=0,
                      vp="layout::c3::k3"),
             textGrob("rate", 
                      y=unit(1, "lines") + unit(2, "mm"), vjust=0,
                      gp=gpar(fontsize=10),
                      vp="layout::c3::k3"),
             childrenvp=vp)
}
ggplot(districts) +
    geom_sf() +
    grid_panel(therm, aes(X, Y, height=rate)) +
    grid_panel(thermKey) +
    scale_fill_continuous(name="crime rate") +
    theme(axis.title=element_blank(),
          axis.ticks=element_blank(),
          axis.text=element_blank())


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-junk
#| fig-cap: A choropleth map of the average crime rate over the period 2011 to 2021 for each police district (like @fig-map), with cartoon villian images added.
#| fig.keep: last
runonce <- function() {
    PostScriptTrace("Images/criminal.eps", "Images/criminal.xml")
}
bbox <- st_bbox(districts$geometry)
bboxDF <- data.frame(t(as.numeric(bbox)))
names(bboxDF) <- names(bbox)
## [-1] to remove the background
crimPic <- grImport::readPicture("Images/criminal.xml")[-1]
dx <- diff(crimPic@summary@xscale)
dy <- diff(crimPic@summary@yscale)
ar <- dy/dx
crim <- function(data, coords) {
    vp <- viewport(x=coords$xmin + .2, y=.7, width=.4, 
                   height=unit(ar*.4, "snpc"))
    crimDef <- defineGrob(grImport::pictureGrob(crimPic, exp=0), 
                          vp=vp,
                          name="crim")
    crimTL <- useGrob("crim", vp=vp)
    crimBR <- useGrob("crim",
                      transform=function(group, ...) {
                          viewportTransform(group, flip=groupFlip(flipX=TRUE))
                      },
                      vp=viewport(x=coords$xmax - .2, 
                                  y=unit(coords$ymin, "npc") + 
                                    unit(ar*.2, "snpc"),
                                  width=.4, height=unit(ar*.4, "snpc")))
    grobTree(crimDef, crimTL, crimBR)
}
ggplot(districts) +
    geom_sf(aes(fill=rate), colour="black", linewidth=.5) +
    grid_panel(crim, aes(xmin=xmin, xmax=xmax, ymin=ymin), data=bboxDF) +
    scale_y_continuous(expand=expansion(0)) +
    scale_fill_continuous(name="crime rate", 
                          guide=guide_colourbar(display="gradient")) +
    theme(panel.border=element_blank(),
          axis.ticks=element_blank(),
          axis.text=element_blank(),
          legend.margin=margin(0, 0, 0, 0),
          legend.justification="bottom")
grid.force()
grid.edit("bar::rect", grep=TRUE, gp=gpar(col="black"))


## -----------------------------------------------------------------------------
#| echo: false
## https://www.statista.com/statistics/1044386/dog-and-cat-pet-population-worldwide/
pets <- data.frame(pet=c("dog", "cat"), count=c(471, 373))
## https://openclipart.org/download/319840/cat-silhouette.svg
## https://openclipart.org/download/276049/Dog.svg
runonce <- function() {
    rsvg_svg("Images/cat.svg", "Images/cat-cairo.svg")
    rsvg_svg("Images/dog-crop-manual.svg", "Images/dog-cairo.svg")
}
## cat <- readPNG("Images/cat.png")
## dog <- readPNG("Images/dog.png")
cat <- grImport2::readPicture("Images/cat-cairo.svg")
dog <- grImport2::readPicture("Images/dog-cairo.svg")


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-pic
#| fig-cap: A data visualisation of the number of pet cats and pet dogs worldwide.
petGlyph <- function(data, coords) {
    grobTree(grImport2::pictureGrob(cat, coords$x[2], 0, height=coords$y[2], 
                         just="bottom", expansion=0),
             grImport2::pictureGrob(dog, coords$x[1], 0, height=coords$y[1], 
                         just="bottom", expansion=0))
}
ggplot(pets) + 
    grid_panel(petGlyph, aes(x=pet, y=count)) +
    scale_y_continuous(limits=c(0, NA), expand=expansion(c(0, .05))) +
    scale_x_discrete(expand=expansion(c(1, 1))) +
    coord_cartesian(clip="off") +
    xlab("") +
    ylab("millions") +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          aspect.ratio=1/2)


## -----------------------------------------------------------------------------
#| echo: false
#| layout-ncol: 2
#| fig.width: 4
#| label: fig-pic-alt
#| fig-cap: Two alternative uses of icons.
#| fig-subcap:
#|   - distorted icons
#|   - icon labels
petStretch <- function(data, coords) {
    grobTree(rectGrob(coords$x[2], width=unit(3, "cm"),
                      0, height=coords$y[2], 
                      just="bottom", gp=gpar(col=NA, fill="grey80")),
             grImport2::pictureGrob(cat, coords$x[2], width=unit(3, "cm"),
                         0, height=coords$y[2], 
                         just="bottom", expansion=0, distort=TRUE),
             rectGrob(coords$x[1], width=unit(3, "cm"),
                      0, height=coords$y[1], 
                      just="bottom", gp=gpar(col=NA, fill="grey80")),
             grImport2::pictureGrob(dog, coords$x[1], width=unit(3, "cm"),
                         0, height=coords$y[1], 
                         just="bottom", expansion=0, distort=TRUE))
}
ggplot(pets) + 
    grid_panel(petStretch, aes(x=pet, y=count)) +
    scale_y_continuous(limits=c(0, NA), expand=expansion(c(0, .05))) +
    scale_x_discrete(expand=expansion(c(.5, .5))) +
    coord_cartesian(clip="off") +
    xlab("") +
    ylab("millions") +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          aspect.ratio=1)

petKey <- function(data, coords) {
    grobTree(grImport2::pictureGrob(cat, coords$x[2], 0, height=.2, 
                         just="bottom", expansion=0),
             grImport2::pictureGrob(dog, coords$x[1], 0, height=.2, 
                         just="bottom", expansion=0))
}
ggplot(pets) + 
    geom_col(aes(x=pet, y=count), width=.5, fill="grey80") +
    grid_panel(petKey, aes(x=pet, y=count)) +
    scale_y_continuous(limits=c(0, NA), expand=expansion(c(0, .05))) +
    scale_x_discrete(expand=expansion(c(.5, .5))) +
    coord_cartesian(clip="off") +
    xlab("") +
    ylab("millions") +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| layout-ncol: 2
#| fig.width: 4
#| label: fig-pic-icon
#| fig-cap: Two more alternative data visualisations of the pet data.
#| fig-subcap: 
#|   - Icons are repeated to represent the number of pets.
#|   - A basic bar plot
petIcon <- function(data, coords) {
    height <- .2
    ncat <- coords$y[2] %/% height + 1
    ndog <- coords$y[1] %/% height + 1
    cats <- do.call(grobTree, 
                    lapply(1:ncat,
                           function(i) {
                               grobTree(rectGrob(coords$x[2], 
                                                 (i - 1)*height,
                                                 height + .05, height,
                                                 just="bottom",
                                                 gp=gpar(col=NA, 
                                                         fill="grey80")),
                                        grImport2::pictureGrob(cat, 
                                                    coords$x[2], 
                                                    (i - 1)*height, 
                                                    height=height, 
                                                    just="bottom",
                                                    expansion=0,
                                                    clip="inherit"))
                           }))
    catPile <- grobTree(cats,
                        vp=viewport(clip=rectGrob(y=0, height=coords$y[2], 
                                    just="bottom")))
    dogs <- do.call(grobTree, 
                    lapply(1:ndog,
                           function(i) {
                               grobTree(rectGrob(coords$x[1], 
                                                 (i - 1)*height,
                                                 height + .05, height,
                                                 just="bottom",
                                                 gp=gpar(col=NA, 
                                                         fill="grey80")),
                                        grImport2::pictureGrob(dog, 
                                                    coords$x[1], 
                                                    (i - 1)*height,  
                                                    height=height, 
                                                    just="bottom",
                                                    expansion=0,
                                                    clip="inherit"))
                           }))
    dogPile <- grobTree(dogs,
                        vp=viewport(clip=rectGrob(y=0, height=coords$y[1], 
                                    just="bottom")))
    grobTree(catPile, dogPile)
}
ggplot(pets) + 
    grid_panel(petIcon, aes(x=pet, y=count)) +
    scale_y_continuous(limits=c(0, NA), expand=expansion(c(0, .05))) +
    scale_x_discrete(expand=expansion(c(.5, .5))) +
    coord_cartesian(clip="off") +
    xlab("") +
    ylab("millions") +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          aspect.ratio=1)

ggplot(pets) + 
    geom_col(aes(x=pet, y=count), width=.5, fill="grey80") +
    scale_y_continuous(limits=c(0, NA), expand=expansion(c(0, .05))) +
    scale_x_discrete(expand=expansion(c(.5, .5))) +
    coord_cartesian(clip="off") +
    xlab("") +
    ylab("millions") +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-chernoff
#| fig-cap: A Chernoff faces plot of the ...
rwcChernoff <- RWCperGame
rwcChernoff$country <- reorder(rwcChernoff$country, rwcChernoff$points)
ggplot(rwcChernoff) +
    geom_chernoff(aes(x="", y="", smile=points, brow=tries, 
                      eyes=breaks),
                  size=20, fill="white") +
    facet_wrap(vars(country), strip.position="bottom") +
    coord_cartesian(clip="off") +
    theme(panel.border=element_blank(),
          strip.background=element_blank(),
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          legend.position="none",
          aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-stroop
#| fig-cap:  The Stroop effect. Try to name the *colour* of each word.
#| fig-height: 1
grid.newpage()
pushViewport(viewport(width=.5))
grid.text(c("red", "green", "blue"), 1:3/4, 
          gp=gpar(col=c("green", "blue", "red"), fontsize=20, fontface="bold"))


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-andrews
#| warning: false
#| results: hide
#| fig-cap: An Andrews plot of the performance measures for teams in the 2023 Rugby World Cup.
## remotes::install_github("gbasulto/drewcurves")
library(drewcurves)
drew <- drewcurves(RWCperGame[c(2, 11, 5, 10, 7)], # RWCperGame[-1]
                   group=1, # sphere
                   return_dataframe=TRUE)
gg <- ggplot(drew) +
    geom_line(aes(t, value, colour=sphere, group=key), alpha=.5) +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank())
pushViewport(viewport(width=.8, height=.8))
print(gg, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-no-text
#| fig-cap:  A line plot with all text removed.  Without text, it is impossible to know what data values are represented in this plot.
ggNoText <- ggplot(crimeAgeSimple) +
    geom_line(aes(x=year, y=rate, colour=ageFactor)) +
    scale_x_continuous(breaks=seq(2012, 2020, 2)) +    
    crimeAgeLineTitle +
    crimeAgeLineXlab +
    crimeAgeLineYlab +
    theme(text=element_text(colour="transparent"),
          axis.text=element_text(colour="transparent"),
          panel.grid.major.y=element_line(colour="black", linewidth=.1),
          aspect.ratio=1)
pushViewport(viewport(width=.9, height=.9))
print(ggNoText, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-bar-text
#| fig-cap: A bar plot of the total number of offenders for different ethnic groups. The data symbols in this plot are the horizontal bars *and* the text values at the end of each bar.
ggplot(crimeGroupTotal) + 
    geom_col(aes(x=total, y=group)) +
    geom_text(aes(label=paste0(total, " "), x=total, y=group), 
              hjust=1, colour="white", fontface="bold") +
    scale_x_continuous(expand=expansion(c(0, .05))) +
    theme(aspect.ratio=1,
          axis.title.y=element_blank())


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-crime-guides
#| fig-cap: A line plot of the crime rate amongst youth offenders aged 14 to 16 from 2011 to 2021. The guides in this plot have been highlighted in purple.
#| fig-keep: last
ggCrimeGuides <- ggplot(crimeAgeSimple) +
    geom_line(aes(x=year, y=rate, colour=ageFactor), linewidth=.1) +
    crimeAgeLineTitle +
    crimeAgeLineXlab +
    crimeAgeLineYlab +
    geom_line(aes(x=year, y=rate, group=ageFactor), colour="grey90") +
    scale_x_continuous(breaks=seq(2012, 2020, 2)) +    
    scale_colour_manual(name="age",
                        values=pal_brewer(palette="Purples")(5)[-(1:2)]) +
    theme(panel.border=element_rect(colour="grey"),
          plot.title=element_text(colour="grey"),
          plot.subtitle=element_text(colour="grey"),
          panel.grid.major.y=element_line(colour=highlight),
          axis.line=element_line(colour="grey"),
          axis.ticks=element_line(colour=highlight),
          axis.text=element_text(colour=highlight, face="bold"),          
          axis.title=element_text(colour="grey"),
          legend.text=element_text(colour=highlight, face="bold"),
          legend.title=element_text(colour="grey"),
          aspect.ratio=1)
pushViewport(viewport(width=.9, height=.9))
print(ggCrimeGuides, newpage=FALSE)
popViewport()
grid.force()
grid.edit("key::segments", grep=TRUE, global=TRUE,
          gp=gpar(lwd=2))


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-crime-labels
#| fig-cap: A line plot of the crime rate amongst youth offenders aged 14 to 16 from 2011 to 2021. The labels in this plot have been highlighted in purple.
ggCrimeLabels <- ggplot(crimeAgeSimple) +
    geom_line(aes(x=year, y=rate, colour=ageFactor), linewidth=.1) +
    crimeAgeLineTitle +
    crimeAgeLineXlab +
    crimeAgeLineYlab +
    geom_line(aes(x=year, y=rate, group=ageFactor), colour="grey90") +
    scale_x_continuous(breaks=seq(2012, 2020, 2)) +    
    scale_colour_manual(name="age",
                        values=grey(0:2/4)) +
    theme(plot.title=element_text(colour=highlight, face="bold"),
          plot.subtitle=element_text(colour=highlight, face="bold"),
          panel.border=element_rect(colour="grey"),
          panel.grid.major.y=element_line(colour="grey", linewidth=.2),
          axis.ticks=element_line(colour="grey"),
          axis.text=element_text(colour="grey"),
          axis.title=element_text(colour=highlight, face="bold"),
          legend.text=element_text(colour="grey"),
          legend.title=element_text(colour=highlight, face="bold"),
          aspect.ratio=1)
pushViewport(viewport(width=.9, height=.9))
print(ggCrimeLabels, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| message: false
library(xdvir)
markStr <- r"(
\begin{minipage}{1.4in}
Despite recording the most clean breaks and
tries in the tournament,
\zsavepos{leftNZ}\mbox{New Zealand}\zsavepos{rightNZ},
came second to a much more defensive 
\zsavepos{leftSA}\mbox{South Africa}\zsavepos{rightSA}.
\Rzmark{leftNZ}\Rzmark{rightNZ}
\Rzmark{leftSA}\Rzmark{rightSA}
\end{minipage})"
markTeX <- function(data, coords) {
    latexGrob(markStr, packages="zref",
              x=unit(0, "npc") + unit(2, "mm"),
              y=unit(1, "npc") - unit(2, "mm"),
              hjust="left", vjust="top",
              gp=gpar(fontsize=10))
}
makeContent.markCurve <- function(x) {
    ## Delay this calculation until drawing time so that we
    ## are in the correct viewport
    devLoc <- deviceLoc(x$x, x$y)
    addMark(x$name, devLoc$x, devLoc$y)
    x
}
markCurve <- function(data, coords) {
    grobTree(gTree(x=unit(coords$x[data$name == "New Zealand"], "npc"),
                   y=unit(coords$y[data$name == "New Zealand"], "npc"),
                   name="NZ", cl="markCurve"),
             gTree(x=unit(coords$x[data$name == "South Africa"], "npc"),
                   y=unit(coords$y[data$name == "South Africa"], "npc"),
                   name="SA", cl="markCurve"))
}
ggTeX <- ggplot(RWCperGame) +
    geom_point(aes(breaks, tries), colour="grey") +
    geom_point(aes(breaks, tries), colour="black", 
               data=subset(RWCperGame, 
                           country %in% c("New Zealand", "South Africa"))) +
    grid_panel(markTeX) +
    grid_panel(markCurve, aes(breaks, tries, name=country)) +
    scale_x_continuous(name="clean breaks") +
    scale_y_continuous(name="tries scored") +
    theme(aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-ann
#| fig-cap: The number of times a team breaks through the opposition defence and the number of tries that a team scores (both are per-game averages) for teams in the 2023 Rugby World Cup.
library(gridGeometry)
pushViewport(viewport(width=.9, height=.9))
print(ggTeX, newpage=FALSE)
popViewport()
leftNZ <- getMark("leftNZ")
rightNZ <- getMark("rightNZ")
ptNZ <- getMark("NZ")
grid.segments(leftNZ$devx, leftNZ$devy - unit(1, "mm"), 
              rightNZ$devx, rightNZ$devy - unit(1, "mm"))
grid.bezier(unit.c(rightNZ$devx, ptNZ$devx, ptNZ$devx, ptNZ$devx),
            unit.c(rightNZ$devy - unit(1, "mm"),
                   rightNZ$devy - unit(1, "mm"),
                   rightNZ$devy - unit(1, "mm"),
                   ptNZ$devy))
leftSA <- getMark("leftSA")
rightSA <- getMark("rightSA")
ptSA <- getMark("SA")
grid.segments(leftSA$devx, leftSA$devy - unit(1, "mm"), 
              rightSA$devx, rightSA$devy - unit(1, "mm"))
grid.bezier(unit.c(rightSA$devx, 
                   rightSA$devx + unit(3, "mm"),
                   rightSA$devx + unit(3, "mm"),
                   ptSA$devx),
            unit.c(rightSA$devy - unit(1, "mm"),
                   rightSA$devy - unit(1, "mm"),
                   rightSA$devy - unit(1, "mm"),
                   ptSA$devy))


## -----------------------------------------------------------------------------
#| echo: false
#| message: false
#| label: tbl-wiki-rwc
#| tbl-cap: A table of the number of times different words occur in the Wikipedia page for the Rugby World Cup (for words that appear more than once).  There are 287 different words, but only the first 6 are shown here.
kable(head(wikiRWC), row.names=FALSE)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-wordcloud-bar
#| fig-cap: A bar plot of the frequency of different words in the Wikipedia page for the Rugby World Cup.
temp <- subset(wikiRWC, freq > 1)
padding <- 300 - nrow(temp)
maxlen <- max(nchar(temp$word))
temp$word <- sprintf(paste0("%", maxlen, "s"), temp$word)
temp <- rbind(temp, 
              data.frame(word=sapply(1:padding, 
                                     function(i) 
                                         paste(rep(" ", i), collapse="")),
                         freq=rep(0, padding)))
temp$word <- reorder(temp$word, temp$freq)
temp$group <- rep(1:6, each=50)
ggplot(temp) + 
    geom_col(aes(freq, word)) + 
    scale_x_continuous(breaks=c(0, 4, 8, 20, 40, 60),
                       expand=expansion(add=c(0, 5))) +
    facet_row(vars(group), scales="free", space="free") +
    theme(axis.text.y=element_text(size=7),
          axis.title=element_blank(),
          axis.ticks.y=element_blank(),
          strip.background=element_blank(),
          strip.text=element_blank())


## -----------------------------------------------------------------------------
#| echo: false
#| message: false
#| label: tbl-rwc-table
#| tbl-cap: Performance measures for the top 8 teams at the 2023 Rugby World Cup. Each measure is a per-game average because some teams played more games than others.
rwc8 <- subset(RWCperGame, country %in% topNations,
               select=c("country", "sphere", 
                        "runs", "breaks", "tries", "points"))
rwc8mess <- data.frame(lapply(rwc8, function(x) sapply(x, format, digits=4)))
kable(rwc8mess, row.names=FALSE)


## -----------------------------------------------------------------------------
#| echo: false
#| message: false
#| label: tbl-rwc-table-tidy
#| tbl-cap: Performance measures for the top 8 teams at the 2023 Rugby World Cup. This is a version of @tbl-rwc-table with some standard table design guidelines applied.
rwc8tidy <- rwc8
rwc8tidy$runs <- round(rwc8tidy$runs)
rwc8tidy <- rwc8tidy[order(rwc8tidy$points, decreasing=TRUE),]
kable(rwc8tidy, digits=1, row.names=FALSE)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-rwc-table
#| fig-cap:  A scatter plot of the performance measures for the top 8 teams at the 2023 Rugby World Cup.
ggplot(rwc8) +
    geom_point(aes(tries, points, size=breaks, colour=runs)) +
    scale_colour_gradient(low="grey80", high="black") +
    theme(aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-tries-hist
#| fig-cap: A histogram of the number of tries scored. The bars are horizontal for easy comparison with the stem-and-leaf plot.
#| fig.keep: last
ggplot(RWCperGame) +
    geom_histogram(aes(y=round(tries, 1), colour=sphere, fill=sphere), 
                   breaks=0:8, closed="left") +
    labs(title="Rugby World Cup 2023") +
    ylab("tries scored") +
    scale_x_continuous(expand=expansion(c(0, .05))) +
    scale_y_continuous(breaks=0:8) +
    scale_colour_manual(values=c("grey40", "grey40")) +
    scale_fill_manual(values=c("grey40", "grey40")) +
    theme(plot.background=element_rect(colour=NA, fill="grey95"),
          legend.background=element_rect(colour=NA, fill="grey95"),
          legend.title=element_text(colour=NA),
          legend.text=element_text(colour=NA),
          aspect.ratio=1,
          axis.title.x=element_blank(),
          panel.background=element_blank(),
          panel.border=element_rect(colour="black", fill=NA),
          panel.grid=element_blank())
grid.force()
keyRect <- grid.grep("key::rect", grep=TRUE, global=TRUE)
grid.edit(keyRect[[1]], gp=gpar(col=NA, fill=NA))
grid.edit(keyRect[[2]], gp=gpar(col=NA, fill=NA))
ggTriesHist <- grid.grab()
grid.newpage()
pushViewport(viewport(height=.8))
grid.draw(ggTriesHist)
popViewport()


## ----echo=FALSE---------------------------------------------------------------
stemAndLeaf <- 
    gsub("^  ", "",
         capture.output(stem(RWCperGame$tries, scale=2))[-c(1:3, 12)])


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-tries-stem
#| fig-cap: A stem-and-leaf plot of the number of tries scored.
## Try to get stem background same size as ggplot background
pushViewport(viewport(height=.8))
grid.rect(width=.912, gp=gpar(col=NA, fill="grey95"))
grid.text("Rugby World Cup 2023", x=.3, just="left",
          y=unit(1, "npc") - unit(1, "line"), gp=gpar(cex=1.5))
stemAndLeafTeX <- paste("\\begin{verbatim}",
                        paste(rev(stemAndLeaf), collapse="\n"),
                        "\\end{verbatim}", sep="\n")
grid.latex(stemAndLeafTeX,
           x=.3, hjust="left", gp=gpar(fontsize=16))
grid.text("tries scored", 
          x=.3, just="left", 
          y=unit(2, "lines"))
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
nc <- nchar(stemAndLeaf) - 4
lengths <- cumsum(nc)
stemAndLeafBold <- character(length(stemAndLeaf))
stemSphere <- RWCperGame$sphere[order(RWCperGame$tries)]
maxLen <- max(nc)
index <- 1
for (i in 1:length(stemAndLeaf)) {
    l <- lengths[i]
    index <- index:l
    hemi <- stemSphere[index]
    north <- hemi == "North"
    stem <- substring(stemAndLeaf[i], 1, 4)
    if (nchar(stem) < nchar(stemAndLeaf[i])) {
        char <- strsplit(substring(stemAndLeaf[i], 5), "")[[1]]
        char[north] <- paste0("\\textbf{", char[north], "}")
    } else {
        char <- ""
    }
    stemAndLeafBold[i] <- paste0(stem, paste(char, collapse=""))
    index <- l + 1
}


## -----------------------------------------------------------------------------
#| echo: false
stemBold <- function() {
    grid.rect(width=.912, gp=gpar(col=NA, fill="grey95"))
    grid.text("Rugby World Cup 2023", x=.3, just="left",
              y=unit(1, "npc") - unit(1, "line"), gp=gpar(cex=1.5))
    alltt <- LaTeXpackage("alltt", "\\usepackage{alltt}")
    stemAndLeafBoldTeX <- paste("\\setmonofont{Latin Modern Mono Light}",
                            "\\begin{alltt}",
                            paste(rev(stemAndLeafBold), collapse="\n"),
                            "\\end{alltt}", sep="\n")
    grid.latex(stemAndLeafBoldTeX, packages=alltt,
               x=.3, hjust="left", gp=gpar(fontsize=16))
    grid.text("tries scored", 
              x=.3, just="left", 
              y=unit(2, "lines"))
    grid.text(expression("South versus "*bold("North")), 
              x=.3, just="left",
              y=unit(1, "line"))
}


## -----------------------------------------------------------------------------
#| echo: false
nc <- nchar(stemAndLeaf) - 4
lengths <- cumsum(nc)
stemAndLeafColour <- character(length(stemAndLeaf))
maxLen <- max(nc)
index <- 1
for (i in 1:length(stemAndLeaf)) {
    l <- lengths[i]
    index <- index:l
    hemi <- stemSphere[index]
    north <- hemi == "North"
    stem <- substring(stemAndLeaf[i], 1, 4)
    if (nchar(stem) < nchar(stemAndLeaf[i])) {
        char <- strsplit(substring(stemAndLeaf[i], 5), "")[[1]]
        char[north] <- paste0("\\textcolor{salmon}{", char[north], "}")
        char[!north] <- paste0("\\textcolor{teal}{", char[!north], "}")
    } else {
        char <- ""
    }
    stemAndLeafColour[i] <- paste0(stem, paste(char, collapse=""))
    index <- l + 1
}


## -----------------------------------------------------------------------------
#| echo: false
library(gridtext)
NScols <- pal_npg()(2)
colsTeX <- paste0("\\definecolor{", c("salmon", "teal"), "}",
               apply(col2rgb(NScols), 2,
               function(x) {
                   paste0("{RGB}{", paste(x, collapse=", "), "}")
               }), collapse="\n")
stemColour <- function() {
    grid.rect(width=.912, gp=gpar(col=NA, fill="grey95"))
    grid.text("Rugby World Cup 2023", x=.3, just="left",
              y=unit(1, "npc") - unit(1, "line"), gp=gpar(cex=1.5))
    alltt <- LaTeXpackage("alltt", "\\usepackage{alltt}")
    stemAndLeafColourTeX <- paste(colsTeX,
                            "\\begin{alltt}",
                            paste(rev(stemAndLeafColour), collapse="\n"),
                            "\\end{alltt}", sep="\n")
    grid.latex(stemAndLeafColourTeX, packages=list(alltt, "xcolor"),
               x=.3, hjust="left", gp=gpar(fontsize=16))
    grid.text("tries scored", 
              x=.3, just="left", 
              y=unit(2, "lines"))
    grid.draw(richtext_grob(paste0('<span style="color: ', NScols[2], 
                                   '">South</span> versus ',
                                   '<span style="color: ', NScols[1], 
                                   '">North</span>'), 
                            x=.3, hjust=0,
                            y=unit(1, "line")))
}


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-tries-stem-colour
#| fig-cap: A stem-and-leaf plot of the number of tries scored with hemisphere encoded as colour.
grid.newpage()
pushViewport(viewport(height=.8))
stemColour()
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-tries-stem-weight
#| fig-cap: A stem-and-leaf plot of the number of tries scored with hemisphere encoded as font weight.
grid.newpage()
pushViewport(viewport(height=.8))
stemBold()
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| results: hide
deaths <- read.csv("Data/florida-deaths.csv")
bloodRed <- "#660000"
xaxis <- function(data, coords) {
    ticks <- coords$x[match(c(2000, 2010), data$x)]
    labels <- coords$x[match(c(1995, 2005, 2012), data$x)]
    grobTree(segmentsGrob(0, 1, 1, 1, gp=gpar(lwd=2)),
             segmentsGrob(0, 0, 1, 0, gp=gpar(lwd=2)),
             segmentsGrob(ticks, 0, ticks, unit(-5, "mm")),
             textGrob(c("1990s", "2000s", "2010s"),
                      labels, unit(-1, "mm"), just="top",
                      gp=gpar(fontsize=20)))
}
labelbg <- function(data, coords) {
    x <- coords$x[data$x == 2005]
    y <- coords$y[data$x == 2005]
    lab <- textGrob("2005\nFlorida enacted\nits 'Stand Your\nGround' law",
                    unit(x, "npc") - unit(1, "cm"),
                    unit(y, "npc") + unit(2, "cm"),
                    just=c("left", "bottom"),
                    gp=gpar(fontsize=20, fontface="bold", col="white",
                            lineheight=1))
    rectGrob(grobX(lab, "west") - unit(1, "cm"),
             grobY(lab, "south"),
             1,
             grobHeight(lab),
             just=c("left", "bottom"),
             gp=gpar(col=NA, fill="white"))
}
label <- function(data, coords) {
    x <- coords$x[data$x == 2005]
    y <- coords$y[data$x == 2005]
    lab <- textGrob("2005\nFlorida enacted\nits 'Stand Your\nGround' law",
                    unit(x, "npc") - unit(1, "cm"),
                    unit(y, "npc") + unit(2, "cm"),
                    just=c("left", "bottom"),
                    gp=gpar(fontsize=20, fontface="bold", col="white",
                            lineheight=1))
    grobTree(lab,
             segmentsGrob(x, y, x,
                          unit(y, "npc") + unit(18, "mm"),
                          gp=gpar(lwd=1, col="white")))
}
gg <- ggplot(deaths) +
    grid_panel(labelbg, aes(x=year, y=count)) +
    geom_polygon(data=rbind(data.frame(year=2012, count=0),
                            deaths,
                            data.frame(year=1990, count=0)),
                 aes(year, y=count, group=1),
                 fill=adjustcolor(bloodRed, alpha=.8)) +
    grid_panel(label, aes(x=year, y=count)) +
    geom_line(aes(year, count, group=1), linewidth=1.5) +
    geom_point(aes(year, count),
               colour="white", size=5) +
    geom_point(aes(year, count),
               colour="black", size=3) +
    scale_y_continuous(limits=c(1.05*max(deaths$count), 0), trans="reverse",
                       expand=expansion(0)) +
    coord_cartesian(clip="off") +
    labs(title="Alarming Rise In Florida Gun Deaths\nAfter 'Stand Your Ground' Was Enacted\n") +
    ## subtitle="All deaths involving guns\n") +
    theme(plot.title=element_text(size=30, face="bold"),
          plot.subtitle=element_text(size=20),
          plot.title.position="plot",
          panel.background=element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major.x=element_blank(),
          panel.grid.major.y=element_line(color="grey"),
          aspect.ratio=1,
          axis.ticks=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_text(size=20),
          axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          plot.margin=unit(rep(10, 4), "mm")) +
    grid_panel(xaxis, aes(x=year, y=count)) 
## Deliberately misleading title to show power of text labels
png("Images/deaths-florida-contra.png", height=750, width=680)
pushViewport(viewport(y=1, height=unit(1, "npc") - unit(1, "in"), just="top"))
ggcontra <- gg +
    labs(title="Suprising *Decrease* In Florida Gun Deaths<br>After 'Stand Your Ground' Was Enacted<br>") +
    theme(plot.title=element_markdown())
print(ggcontra, newpage=FALSE)
popViewport()
grid.text("Source: USAFacts", x=unit(10, "mm"), just="left",
          y=unit(2, "lines"),
          gp=gpar(fontsize=20))
dev.off()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-yji
youthCols <- c("#bf0000", "#2b4689", "#e1b728", "#8fbb22",
               "#0087c0", "#bdaa7c", "#f15a22")
youthCols <- pal_npg()(7)
gg1 <- ggplot(subset(crimeGroup, group != "Unknown")) +
    geom_line(aes(year, rate, colour=group)) +
    scale_colour_manual(values=youthCols[1:3]) +
    crimeAgeLineTitle +
    theme(axis.title=element_blank(),
          legend.justification="left",
          legend.margin=margin(),
          legend.title=element_blank(),
          legend.position="top")
typeTotal <- subset(crimeTypeTotal, prop > .04)
typeTotal$type <- reorder(typeTotal$type, typeTotal$prop,
                          decreasing=TRUE)
levels(typeTotal$type) <- c(levels(typeTotal$type), "Other")
typeTotal <- rbind(typeTotal, 
                   data.frame(type="Other", total=NA,
                              prop=sum(subset(crimeTypeTotal, 
                                              prop <= .04)$prop)))
gg2 <- ggplot(typeTotal) +
    geom_col(aes(x=prop, y="", fill=type), colour=figbg) +
    scale_fill_manual(values=youthCols, 
                      guide=guide_legend(position="top", ncol=1)) +
    coord_polar(direction=-1) +
    theme_void() +
    theme(plot.margin=margin(4, 0, 0, 0, "pt"),
          legend.justification="centre",
          legend.margin=margin(),
          legend.title=element_blank(),
          legend.key.size=unit(2, "mm"))
grid.newpage()
pushViewport(viewport(layout=grid.layout(1, 2, widths=2:1)))
pushViewport(viewport(layout.pos.col=1))
print(gg1, newpage=FALSE)
popViewport()
pushViewport(viewport(layout.pos.col=2))
print(gg2, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-rep-guide
#| fig-cap: A scatter plot of the number of times a team breaks through the opposition defence and the number of tries that a team scores (both are per-game averages) for teams at the 2023 Rugby World Cup.
repcols <- pal_npg()(4)
gg <- ggplot(RWCperGame) +
    geom_point(aes(breaks, tries, colour=sphere)) +
    scale_colour_manual(values=repcols[c(1, 3)], name="hemisphere") +
    scale_x_continuous(name="clean breaks") +
    scale_y_continuous(name="tries scored") +
    theme(aspect.ratio=1)
pushViewport(viewport(height=.8))
print(gg, newpage=FALSE)
popViewport()


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-align
#| fig-cap: A scatter plot of the number of times a team breaks through the opposition defence and the number of tries that a team scores (both are per-game averages) for teams at the 2023 Rugby World Cup.
#| fig-keep: last
repcols <- pal_npg()(10)[c(1, 2, 3, 9)]
gg <- ggplot(RWCperGame) +
    geom_point(aes(breaks, tries, colour=sphere)) +
    scale_colour_manual(values=repcols[c(1, 3)], name="hemisphere") +
    scale_x_continuous(name="clean breaks") +
    scale_y_continuous(name="tries scored") +
    theme(aspect.ratio=1,
          axis.title.x=element_text(hjust=1),
          axis.title.y=element_text(angle=0),
          legend.justification="top",
          legend.margin=margin(0, 10, 10, 40, "pt"),
          legend.key.size=unit(2, "mm"),
          legend.key.spacing.y=unit(2, "mm"))
pushViewport(viewport(height=.8))
print(gg, newpage=FALSE)
popViewport()
grid.force()
tickLabel <- grid.get("axis.1-2-1-2::titleGrob::text", grep=TRUE)
grid.edit("ylab::title::text", grep=TRUE,
          x=unit(1, "npc") + grobWidth(tickLabel), hjust=1)


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-rep-none
#| fig-cap: A scatter plot of the number of times a team breaks through the opposition defence and the number of tries that a team scores (both are per-game averages) for teams at the 2023 Rugby World Cup.
#| fig-keep: last
gg <- ggplot(RWCperGame) +
    geom_point(aes(breaks, tries, colour=sphere)) +
    scale_colour_manual(values=repcols[c(1, 3)], name="hemisphere") +
    scale_x_continuous(name="clean breaks") +
    scale_y_continuous(name="tries scored") +
    theme(aspect.ratio=1)
pushViewport(viewport(height=.8))
print(gg, newpage=FALSE)
popViewport()
grid.force()
keyPoint <- grid.grep("key::points", grep=TRUE, global=TRUE)
grid.edit(keyPoint[[1]], gp=gpar(col=repcols[2]))
grid.edit(keyPoint[[2]], gp=gpar(col=repcols[4]))


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-rep-double
#| fig-cap: A scatter plot of the number of times a team breaks through the opposition defence and the number of tries that a team scores (both are per-game averages) for teams at the 2023 Rugby World Cup.
#| fig-keep: last
gg <- ggplot(RWCperGame) +
    geom_point(aes(breaks, tries, colour=sphere)) +
    scale_colour_manual(values=repcols[c(1, 3)], name="hemisphere") +
    scale_x_continuous(name="clean breaks") +
    scale_y_continuous(name="tries scored") +
    theme(aspect.ratio=1)
pushViewport(viewport(height=.8))
print(gg, newpage=FALSE)
popViewport()
grid.force()
keyText <- grid.grep("label::text", grep=TRUE, global=TRUE)
grid.edit(keyText[[1]], gp=gpar(col=repcols[1], fontface="bold"))
grid.edit(keyText[[2]], gp=gpar(col=repcols[3], fontface="bold"))


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-trump-bill
#| fig-cap: A bar plot of the distributional effects of Trump's "Big, Beautiful Bill".[^trump-bill]
bill <- read.csv("Data/trump-bill.csv")
bill$Income.group <- factor(bill$Income.group,
                            levels=bill$Income.group)
bill$amountLabel <- c("-1K", "-705", "845", "3.2K", "6.1K",
                      "8.8K", "20K", "44.4K", "389.3K")
bill$groupLabel <- gsub(" [(]", "\n(", 
                        gsub("K to ", "K-", bill$Income.group))

ggplot(bill) +
    geom_col(aes(Income.group,
                 Average.change.in.after.tax.and.transfer.income),
             fill=c(rep("lightblue", 8), 4)) +
    geom_text(aes(Income.group,
                  Average.change.in.after.tax.and.transfer.income,
                  label=amountLabel),
              vjust=c(rep(2, 2), rep(-.5, 5), rep(1.5, 2)),
              colour=c(rep("black", 8), "white"),
              fontface=c(rep("bold", 2), rep("plain", 6), "bold")) +
    geom_text(aes(Income.group,
                  0,
                  label=groupLabel),
              lineheight=1,
              size=3,
              vjust=c(rep(-.5, 2), rep(1.5, 7)),
              fontface=c(rep("bold", 2), rep("plain", 6), "bold")) +
    scale_y_continuous(expand=expansion(c(0, .05)),
                       breaks=1000*seq(100, 300, 100), 
                       labels=paste0(seq(100, 300, 100), "K")) +
    scale_x_discrete(label=bill$groupLabel) +
    coord_cartesian(clip="off") +
    ylab(NULL) +
    xlab(NULL) +
    ggtitle("Dollar change in after-tax income in 2026, by income level") +
    theme(panel.border=element_blank(),
          panel.grid.major.y=element_line(colour="grey"),
          axis.ticks=element_blank(),
          axis.text.x=element_text(colour=NA))


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-contrast
#| fig-cap: A scatter plot of the number of times a team breaks through the opposition defence and the number of tries that a team scores (both are per-game averages) for teams at the 2023 Rugby World Cup.
#| fig-keep: last
subtle <- "grey"
gg <- ggplot(RWCperGame) +
    geom_point(aes(breaks, tries, colour=sphere)) +
    scale_colour_manual(values=repcols[c(1, 3)], name="hemisphere") +
    scale_x_continuous(name="clean breaks") +
    scale_y_continuous(name="tries scored") +
    theme(aspect.ratio=1,
          panel.border=element_rect(colour=subtle, fill=NA),
          axis.title=element_text(colour=subtle),
          axis.text=element_text(colour=subtle),
          axis.ticks=element_line(colour=subtle),
          legend.title=element_text(colour=subtle))
pushViewport(viewport(height=.8))
print(gg, newpage=FALSE)
popViewport()
grid.force()
keyText <- grid.grep("label::text", grep=TRUE, global=TRUE)
grid.edit(keyText[[1]], gp=gpar(col=repcols[1], fontface="bold"))
grid.edit(keyText[[2]], gp=gpar(col=repcols[3], fontface="bold"))


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-highlight
#| fig-cap: A scatter plot of the number of times a team breaks through the opposition defence and the number of tries that a team scores (both are per-game averages) for teams at the 2023 Rugby World Cup.
#| fig-keep: last
dimcols <- lighten(repcols[c(1, 3)], .7)
gg <- ggplot(RWCperGame) +
    geom_point(aes(breaks, tries, colour=sphere),
               data=subset(RWCperGame, 
                           !country %in% c("New Zealand", "South Africa"))) +
    geom_point(aes(breaks, tries), colour=repcols[1], size=3,
               data=subset(RWCperGame, 
                           country %in% c("New Zealand", "South Africa"))) +
    geom_text(aes(breaks, tries, label=country), hjust=1.15,
              fontface="bold", colour=repcols[1], size=3,
              data=subset(RWCperGame, 
                          country %in% c("New Zealand", "South Africa"))) +
    scale_colour_manual(values=dimcols, name="hemisphere") +
    scale_x_continuous(name="clean breaks") +
    scale_y_continuous(name="tries scored") +
    theme(aspect.ratio=1,
          panel.border=element_rect(colour=subtle, fill=NA),
          axis.title=element_text(colour=subtle),
          axis.text=element_text(colour=subtle),
          axis.ticks=element_line(colour=subtle),
          legend.title=element_text(colour=subtle))
pushViewport(viewport(height=.8))
print(gg, newpage=FALSE)
popViewport()
grid.force()
keyText <- grid.grep("label::text", grep=TRUE, global=TRUE)
grid.edit(keyText[[1]], gp=gpar(col=dimcols[1], fontface="bold"))
grid.edit(keyText[[2]], gp=gpar(col=dimcols[2], fontface="bold"))


## -----------------------------------------------------------------------------
#| echo: false
#| label: fig-sina
#| fig-cap: A SinaPlot of the points scored per game by Tier One nations at Rugby World Cups.
gg <- ggplot(rwcAll, aes(scored, y="")) +
    geom_violin() +
    geom_sina() +
    scale_x_continuous(name="points scored")
pushViewport(viewport(height=.8, width=.8))
print(gg, newpage=FALSE)
popViewport()

dev.off()
