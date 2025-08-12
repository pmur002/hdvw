
setwd("..")
source("common.R")
setwd("Cover")

## Path from question mark drawn with dots, real dot a different colour

## Generated using "split complementary" on 
## https://color.adobe.com/create/color-wheel
col1 <- "#5DE6F5" ## "#71F549"
col2 <- "#5DF59E" ## "#F5F249"
col3 <- "#4B3D38" ## "#483E4B"

scale <- 1
    
k <- 0.5522847498
x <- c(.4, .4, .5 - k*.1, .5,
       .5 + k*.1, .6, .6,
       .6, .5, .5,
       .5, .5, .5)
y <- c(.8, .8 + k*.1, .9, .9,
       .9, .8 + k*.1, .8,
       .8 - k*.2, .6 + k*.2, .6,
       .5, .5, .5) - .2
    
library(gridBezier)

svg("cover.svg", width=4, height=4)

grid.newpage()
grid.rect(gp=gpar(col=NA, fill=col3))

## Fixed size for drawing (regardless of device size)
pushViewport(viewport(width=unit(6, "in"), height=unit(6, "in")))

## grid.Bezier(x, y)

nspots <- 15
spots <- seq(0, 1, length.out=nspots)

library(gridGeometry)
segments <- forceGrob(trimGrob(BezierGrob(x, y, default.units="npc"),
                               from=spots[-nspots], to=spots[-1],
                               gp=gpar(col=1:2, lwd=3)))
## grid.draw(segments)
coords <- grobCoords(segments$children[[1]])
startX <- sapply(coords, function(x) x$x[1])
startY <- sapply(coords, function(x) x$y[1])
endX <- sapply(coords, function(x) x$x[length(x$x)])
endY <- sapply(coords, function(x) x$y[length(x$y)])

grid.circle(startX, startY, r=unit(2, "mm"), default.units="in",
            gp=gpar(col=col1, fill=col1, lwd=2))
grid.circle(endX[nspots - 1], endY[nspots - 1], r=unit(2, "mm"),
            default.units="in", gp=gpar(col=col2, fill=NA, lwd=2))

dev.off()

notrun <- function() {
    library(vwline)
    pts <- BezierPoints(BezierGrob(x, y))
    grid.circle(x[1], y[1], r=unit(2, "mm"), gp=gpar(fill="black"))
    grid.brushXspline(circleBrush(),
                      pts$x, pts$y, default.units="in",
                      shape=0,
                      w=unit(4, "mm"),
                      spacing=unit(8, "mm"),
                      gp=gpar(fill="black"))
    grid.circle(x[length(x)], y[length(y)], r=unit(2, "mm"),
                gp=gpar(col=2, fill=2))
}

