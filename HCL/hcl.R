
## Diagram of HCL (based on CIE Luv)

library(grid)
library(colorspace)
library(gMOIP)

u <- -200:200
v <- -200:200
L <- 100:0
C <- 0:200

r <- .475
d <- .5 - r

angle <- 50

costheta <- cos(angle/180*pi)
sintheta <- sin(angle/180*pi)

dotalpha <- .5
dotsize <- unit(1, "mm")

## Optimal colour solid?
z <- read.csv("cie_xyz_spectral_boundary.csv")
library(colorspace)
luvSolid <- coords(as(XYZ(z[,1], z[,2], z[,3]), "LUV"))
luvHull <- geometry::convhulln(luvSolid[!duplicated(luvSolid), ])

colorFromUV <- function(v, u, L=50, sRGB=TRUE) {
    if (sRGB) {
        srgb <- coords(as(LUV(L, u, v), "sRGB"))
        unreal <- apply(srgb, 1, function(row) any(row < 0 | row > 1))
        cols <- character(nrow(srgb))
        if (any(!unreal))
            cols[!unreal] <- rgb(srgb[!unreal, , drop=FALSE])
        if (any(unreal))
            cols[unreal] <- NA
    } else {
        luv <- LUV(L, u, v)
        lch <- coords(as(luv, "polarLUV"))
        rgb <- hcl(lch[,3], lch[,2], lch[,1], fixup=TRUE)
        ## Cannot call inHull() on too many points at once
        real <- logical(length(rgb))
        start <- 1
        repeat {
            end <- min(length(rgb), start + 4999)
            cat(sprintf("%06d:%06d\n", start, end))
            real[start:end] <-
                inHull(coords(luv)[start:end, ],
                       luvSolid[!duplicated(luvSolid), ],
                       luvHull) >= 0
            if (end == length(rgb))
                break
            start <- end + 1
        }
        if (any(!real)) {
            rgb[!real] <- NA
        }
        cols <- rgb   
    }
    cols
}

colorFromHue <- function(L, C, H=0, sRGB=TRUE) {
    if (sRGB) {
        srgb <- coords(as(polarLUV(L, C, H), "sRGB"))
        unreal <- apply(srgb, 1, function(row) any(row < 0 | row > 1))
        cols <- character(nrow(srgb))
        if (any(!unreal))
            cols[!unreal] <- rgb(srgb[!unreal, , drop=FALSE])
        if (any(unreal))
            cols[unreal] <- NA
    } else {
        rgb <- hcl(H, C, L, fixup=TRUE)
        real <- inHull(coords(as(polarLUV(L, C, H), "LUV")),
                       luvSolid[!duplicated(luvSolid), ],
                       luvHull) >= 0
        if (any(!real)) {
            rgb[!real] <- NA
        }
        cols <- rgb   
    }
    cols
}

findEdge <- function(cols, left=TRUE, up=TRUE) {
    if (left) {
        x <- median(1:length(v)):1
    } else {
        x <- median(1:length(v)):length(v)
    }
    if (up) {
        y <- median(1:length(v)) + round((1:length(x) - 1)*tan(angle/180*pi))
    } else {
        y <- median(1:length(v)) - round((1:length(x) - 1)*tan(angle/180*pi))
    }    
    while (!is.na(cols[x[1], y[1]])) {
        x <- x[-1]
        y <- y[-1]
    }
    c(x[1], y[1])
}

circ <- function() {
    grid.circle(r=r, gp=gpar(col=rgb(1,1,1,.2)))
}

semi <- function(left=TRUE) {
    if (left) {
        x <- 1
    } else {
        x <- 0
    }
    grid.segments(x, d, x, 1 - d, gp=gpar(col=rgb(1,1,1,.2)))
    pushViewport(viewport(clip=TRUE))
    grid.circle(x, .5, 2*r, gp=gpar(col=rgb(1,1,1,.2)))
    popViewport()
}

slice <- function(cols, up=TRUE, angled=TRUE) {
    ras <- rasterGrob(cols)
    if (up) {
        dy <- 1
    } else {
        dy <- -1
    }
    if (!angled) {
        costheta <- 1
        sintheta <- 0
    }
    circleLeft <- circleGrob(.5 - r*costheta, .5 + dy*r*sintheta,
                             dotsize,
                             gp=gpar(col=NA, fill=rgb(0, 0, 0)))
    circleRight <- circleGrob(.5 + r*costheta, .5 + dy*r*sintheta,
                              dotsize,
                              gp=gpar(col=NA, fill=rgb(0, 0, 0)))
    dotleft <<- deviceLoc(unit(.5 - r*costheta, "npc"),
                          unit(.5 + dy*r*sintheta, "npc"))
    dotright <<- deviceLoc(unit(.5 + r*costheta, "npc"),
                           unit(.5 + dy*r*sintheta, "npc"))
    circleMask <- fillGrob(grobTree(rectGrob(width=2),
                                    circleLeft, circleRight),
                           gp=gpar(fill="black"))
    grid.draw(editGrob(circleLeft, gp=gpar(fill=rgb(1, 1, 1, dotalpha))))
    grid.draw(editGrob(circleRight, gp=gpar(fill=rgb(1, 1, 1, dotalpha))))
    pushViewport(viewport(mask=circleMask))
    grid.segments(.5, .5, .5 - r*costheta, .5 + dy*r*sintheta,
                  gp=gpar(col=rgb(1, 1, 1, dotalpha), lty="dotted"))
    grid.segments(.5, .5, .5 + r*costheta, .5 + dy*r*sintheta,
                  gp=gpar(col=rgb(1, 1, 1, dotalpha), lty="dotted"))
    popViewport()
    grid.draw(ras)
    grid.circle(.5, .5, dotsize,
                gp=gpar(col=NA, fill=rgb(1, 1, 1, dotalpha)))
    pushViewport(viewport(mask=ras))
    grid.segments(.5, .5, .5  - r*costheta, .5 + dy*r*sintheta,
                  gp=gpar(col=rgb(1, 1, 1, .2), lwd=2))
    grid.segments(.5, .5, .5  + r*costheta, .5 + dy*r*sintheta,
                  gp=gpar(col=rgb(1, 1, 1, .2), lwd=2))
    popViewport()
}

connector <- function(start, end) {
    dots <- circleGrob(unit.c(start$x, end$x),
                       unit.c(start$y, end$y),
                       dotsize)
    mask <- fillGrob(grobTree(rectGrob(), dots),
                     gp=gpar(fill="black"))
    pushViewport(viewport(mask=mask))
    grid.segments(start$x, start$y, end$x, end$y,
                  gp=gpar(col=rgb(1, 1, 1, dotalpha), lty="dotted", lwd=2))
    popViewport()
}

hclDiagram <- function(sRGB=TRUE) {
    grid.newpage()
    grid.rect(gp=gpar(fill="black"))
    topvp <- viewport(width=.9, height=.9,
                      layout=grid.layout(5, 5,
                                         widths=c(1, .5, 2, .5, 1),
                                         heights=c(2, .5, 2, .5, 2),
                                         respect=TRUE),
                      gp=gpar(fill=NA, lwd=2))

    pushViewport(topvp)
    ## 1, 1
    pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
    semi()
    pushViewport(viewport(height=.5))
    cols <- outer(L, rev(C),
                  function(x, y) colorFromHue(x, y, 180 - angle, sRGB))
    grid.raster(cols, width=1, height=1)
    nonNA <- which(!is.na(cols[quantile(L, .2),]))
    grid.segments(min(nonNA)/ncol(cols), .8, 1, .8,
                  gp=gpar(col=rgb(1, 1, 1, .2), lwd=2))
    grid.circle(1, .8, dotsize, gp=gpar(col=NA, fill=rgb(1, 1, 1, dotalpha)))
    dot11 <- deviceLoc(unit(1, "npc"), unit(.8, "npc"))
    popViewport(2)
    ## 1, 2
    pushViewport(viewport(layout.pos.col=3, layout.pos.row=1),
                 viewport(y=.5 - (.5 + r*sintheta - .65), angle=0))
    circ()
    cols <- outer(rev(u), v, function(x, y) colorFromUV(x, y, 80, sRGB))
    slice(cols)
    popViewport(2)
    ## 1, 3
    pushViewport(viewport(layout.pos.col=5, layout.pos.row=1))
    semi(FALSE)
    pushViewport(viewport(height=.5))
    cols <- outer(L, C, function(x, y) colorFromHue(x, y, angle, sRGB))
    grid.raster(cols, width=1, height=1)
    nonNA <- which(!is.na(cols[quantile(L, .2),]))
    grid.segments(max(nonNA)/ncol(cols), .8, 0, .8,
                  gp=gpar(col=rgb(1, 1, 1, .2), lwd=2))
    grid.circle(0, .8, dotsize, gp=gpar(col=NA, fill=rgb(1, 1, 1, dotalpha)))
    dot13 <- deviceLoc(unit(0, "npc"), unit(.8, "npc"))
    popViewport(2)
    ## hlines
    popViewport()
    connector(dot11, dotleft)
    connector(dot13, dotright)

    pushViewport(topvp)
    ## 2, 1
    pushViewport(viewport(layout.pos.col=1, layout.pos.row=3))
    semi()
    cols <- outer(L, rev(C), function(x, y) colorFromHue(x, y, 180, sRGB))
    grid.raster(cols, width=1, height=.5)
    nonNA <- which(!is.na(cols[median(L),]))
    grid.segments(min(nonNA)/ncol(cols), .5, 1, .5,
                  gp=gpar(col=rgb(1, 1, 1, .2), lwd=2))
    grid.circle(1, .5, dotsize, gp=gpar(col=NA, fill=rgb(1, 1, 1, dotalpha)))
    dot21 <- deviceLoc(unit(1, "npc"), unit(.5, "npc"))
    popViewport()
    ## 2, 2
    pushViewport(viewport(layout.pos.col=3, layout.pos.row=3),
                 viewport(angle=0))
    circ()
    cols <- outer(rev(u), v, function(x, y) colorFromUV(x, y, 50, sRGB))
    slice(cols, angled=FALSE)
    popViewport(2)
    ## 2, 3
    pushViewport(viewport(layout.pos.col=5, layout.pos.row=3))
    semi(FALSE)
    cols <- outer(L, C, function(x, y) colorFromHue(x, y, 0, sRGB))
    grid.raster(cols, width=1, height=.5)
    nonNA <- which(!is.na(cols[median(L),]))
    grid.segments(max(nonNA)/ncol(cols), .5, 0, .5,
                  gp=gpar(col=rgb(1, 1, 1, .2), lwd=2))
    grid.circle(0, .5, dotsize, gp=gpar(col=NA, fill=rgb(1, 1, 1, dotalpha)))
    dot23 <- deviceLoc(unit(0, "npc"), unit(.5, "npc"))
    popViewport()
    ## hlines
    popViewport()
    connector(dot21, dotleft)
    connector(dot23, dotright)

    pushViewport(topvp)
    ## 3, 1
    pushViewport(viewport(layout.pos.col=1, layout.pos.row=5))
    semi()
    pushViewport(viewport(height=.5))
    cols <- outer(L, rev(C),
                  function(x, y) colorFromHue(x, y, 180 + angle, sRGB))
    grid.raster(cols, width=1, height=1)
    nonNA <- which(!is.na(cols[quantile(L, .8),]))
    grid.segments(min(nonNA)/ncol(cols), .2, 1, .2,
                  gp=gpar(col=rgb(1, 1, 1, .2), lwd=2))
    grid.circle(1, .2, dotsize, gp=gpar(col=NA, fill=rgb(1, 1, 1, dotalpha)))
    dot31 <- deviceLoc(unit(1, "npc"), unit(.2, "npc"))
    popViewport(2)
    ## 3, 2
    pushViewport(viewport(layout.pos.col=3, layout.pos.row=5),
                 viewport(y=.5 + (.5 + r*sintheta - .65), angle=0))
    circ()
    cols <- outer(rev(u), v, function(x, y) colorFromUV(x, y, 20, sRGB))
    slice(cols, up=FALSE)
    popViewport(2)
    ## 3, 3 
    pushViewport(viewport(layout.pos.col=5, layout.pos.row=5))
    semi(FALSE)
    pushViewport(viewport(height=.5))
    cols <- outer(L, C, function(x, y) colorFromHue(x, y, 360 - angle, sRGB))
    grid.raster(cols, width=1, height=1)
    nonNA <- which(!is.na(cols[quantile(L, .8),]))
    grid.segments(max(nonNA)/ncol(cols), .2, 0, .2,
                  gp=gpar(col=rgb(1, 1, 1, .2), lwd=2))
    grid.circle(0, .2, dotsize, gp=gpar(col=NA, fill=rgb(1, 1, 1, dotalpha)))
    dot33 <- deviceLoc(unit(0, "npc"), unit(.2, "npc"))
    popViewport(2)
    ## hlines
    popViewport()
    connector(dot31, dotleft)
    connector(dot33, dotright)
}

png("hcl-srgb.png", width=600, height=800)
hclDiagram()
dev.off()

png("hcl.png", width=600, height=800)
hclDiagram(sRGB=FALSE)
dev.off()
