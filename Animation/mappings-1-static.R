
library(dplyr)
library(ggplot2)
library(ggsci)
library(gggrid)
library(gridExtra)
library(grid)

setwd("..")
source("youth-crime.R")
setwd("Animation")

crime <- subset(crimeAge,
                age %in% c("14", "15", "16"),
                c("year", "rate", "ageFactor"))
names(crime) <- c("year", "rate", "age")
crime$rate <- round(crime$rate)

figbg <- "#F2F2F2"
highlight <- "#7D12BA" 

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
    sg <- linesGrob(x, unit.c(unit(0, "mm"), unit(1, "npc") + unit(0, "mm")),
                    gp=gpar(col=adjustcolor(highlight, alpha=0),
                            lty="22", lwd=2),
                    name="year.line")
    grobTree(tg, sg)
}
rateShow <- function(data, coords) {
    y <- coords$y[data$y == 810][1]
    tg <- textGrob("",
                   y=y,
                   x=unit(0, "npc") - unit(1, "lines"),
                   gp=gpar(col=NA),
                   name="rate.end")
    sg <- linesGrob(unit.c(unit(0, "mm"), unit(1, "npc") + unit(0, "mm")), y,
                    gp=gpar(col=adjustcolor(highlight, alpha=0),
                            lty="22", lwd=2),
                    name="rate.line")
    grobTree(tg, sg)
}

gg <- ggplot(crime) +
    geom_line(aes(year, rate, colour=age), linewidth=.5) +
    scale_colour_npg() +
    theme(panel.grid.major.y=element_line(colour="black", linewidth=.1),
          aspect.ratio=1) +
    coord_cartesian(clip="off") +
    grid_panel(yearShow, aes(x=year)) +
    grid_panel(rateShow, aes(y=rate))

nrow <- 9
rowcols <- rep("black", nrow)
rowcols[3] <- highlight
rowface <- rep("plain", nrow)
rowface[3] <- "bold"
gt <-
    tableGrob(head(crime, nrow), rows=NULL,
              theme=ttheme_default(core=list(fg_params=list(col=rowcols,
                                                            fontface=rowface))))

svg("mappings-1-static.svg", width=8, height=4)

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
          label=c(200, 400, 600, ""))

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

reveal("rate.end", label="810", gp=gpar(col=highlight, fontface="bold"))
reveal("rate.line", gp=gpar(col=highlight))
reveal("year.end", label="2011", gp=gpar(col=highlight, fontface="bold"))
reveal("year.line", gp=gpar(col=highlight))

reveal("guide.label.titleGrob::text",
       gp=gpar(col=highlight, cex=1.5, fontface="bold"),
       global=TRUE, index=3, remove=TRUE)
reveal("key::segments",
       gp=gpar(lwd=2),
       global=TRUE, index=3, remove=TRUE, segment=TRUE)

dev.off()

