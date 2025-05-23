
library(dplyr)
library(ggplot2)
library(ggsci)
library(gggrid)
library(gridExtra)
library(grid)
library(gridSVG)

setwd("..")
source("youth-crime.R")
setwd("Animation")

crime <- subset(crimeAge,
                age %in% c("14", "15", "16"),
                c("year", "rate", "age"))
crime$rate <- round(crime$rate)

figbg <- "#F2F2F2"
highlight <- "#7D12BA" 

scale_fill_continuous <- function(...) {
    scale_fill_distiller(palette="Purples", ...)
}

theme_set(theme_bw())
theme_update(plot.background=element_rect(colour=NA, 
                                          fill=figbg),
             panel.background=element_blank(),
             panel.grid.minor=element_blank(),
             panel.grid.major=element_blank(),
             legend.background=element_blank())

yearShow <- function(data, coords) {
    x <- coords$x[data$x == 2011][1]
    tg <- textGrob("",
                   x=x,
                   y=unit(0, "npc") - unit(.5, "lines"),
                   gp=gpar(col=NA),
                   name="year.end")
    sg <- segmentsGrob(x, unit(0, "mm"), x, unit(1, "npc") + unit(0, "mm"),
                       gp=gpar(col=adjustcolor(highlight, alpha=0),
                               lty="22", lwd=2),
                       name="year.line")
    grobTree(tg, sg)
}
ageShow <- function(data, coords) {
    y <- coords$y[data$y == 16][1]
    tg <- textGrob("",
                   y=y,
                   x=unit(0, "npc") - unit(1, "lines"),
                   gp=gpar(col=NA),
                   name="age.end")
    sg <- segmentsGrob(unit(0, "mm"), y, unit(1, "npc") + unit(0, "mm"), y,
                       gp=gpar(col=adjustcolor(highlight, alpha=0),
                               lty="22", lwd=2),
                       name="age.line")
    grobTree(tg, sg)
}

gg <- ggplot(crime) +
    geom_tile(aes(year, age, fill=rate), colour=NA) +
    scale_x_continuous(expand=expansion(0)) +
    scale_y_continuous(expand=expansion(0), breaks=14:16) +
    theme(aspect.ratio=1) +
    coord_cartesian(clip="off") +
    grid_panel(yearShow, aes(x=year)) +
    grid_panel(ageShow, aes(y=age))

nrow <- 9
rowcols <- rep("black", nrow)
rowcols[3] <- highlight
rowface <- rep("plain", nrow)
rowface[3] <- "bold"
gt <-
    tableGrob(head(crime, nrow), rows=NULL,
              theme=ttheme_default(core=list(fg_params=list(col=rowcols,
                                                            fontface=rowface))))

svg("mappings-2-static.svg", width=8, height=4)

grid.newpage()
grid.rect(gp=gpar(col=NA, fill=figbg))
pushViewport(viewport(x=1/3, width=2/3, just="left"))
print(gg, newpage=FALSE)
grid.rect(gp=gpar(col=NA, fill=rgb(1,1,1,.7)))
upViewport()
pushViewport(viewport(x=0, width=1/3, just="left"))
grid.draw(gt)
upViewport()

grid.force()
grid.edit("axis-l::absoluteGrob::axis::axis::titleGrob::text", grep=TRUE,
          label=c(14, 15, ""))
grid.edit("guides::layout::label::titleGrob::text", grep=TRUE,
          label=c(400, 600, ""))

reveal <- function(name, ..., global=FALSE, index=NULL, remove=FALSE,
                   segment=FALSE) {
    grobName <- grid.grep(name,
                          grep=TRUE, global=global, viewports=TRUE)
    if (global) 
        grobName <- grobName[[index]]
    grob <- grid.get(grobName)
    if (remove)
        grid.remove(grobName)
    downViewport(attr(grobName, "vpPath"))
    if (segment) {
        grobLoc1 <- deviceLoc(grob$x0, grob$y0)
        grobLoc2 <- deviceLoc(grob$x1, grob$y1)
    } else {
        grobLoc <- deviceLoc(grob$x, grob$y)
    }
    upViewport(0)
    if (segment) {
        grid.draw(editGrob(grob, x0=grobLoc1$x, y0=grobLoc1$y,
                           x1=grobLoc2$x, y1=grobLoc2$y, ...))
    } else {
        grid.draw(editGrob(grob, x=grobLoc$x, y=grobLoc$y, ...))
    }
}

reveal("age.end", label="16", gp=gpar(col=highlight, fontface="bold"))
reveal("age.line", gp=gpar(col=highlight), segment=TRUE)
reveal("year.end", label="2011", gp=gpar(col=highlight, fontface="bold"))
reveal("year.line", gp=gpar(col=highlight), segment=TRUE)

reveal("guides::layout::label::titleGrob::text",
       label=c("", "", "810"),
       gp=gpar(col=highlight, cex=1.5, fontface="bold"))
barName <- grid.grep("guides::layout::bar::rastergrob",
                     grep=TRUE, viewports=TRUE)
downViewport(attr(barName, "vpPath"))
barLoc <- deviceLoc(unit(0, "npc"), unit(1, "npc"))
barDim <- deviceDim(unit(1, "npc"), unit(1, "npc"))
upViewport(0)
grid.rect(barLoc$x, barLoc$y, just=c("left", "top"),
          barDim$w, unit(1, "mm"),
          gp=gpar(col=highlight, fill=NA))

dev.off()

