
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
            end <- min(length(rgb), start + 9999)
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

slice <- function(cols, up=TRUE) {
    ras <- rasterGrob(cols)
    if (up) {
        dy <- 1
    } else {
        dy <- -1
    }
    circles <- circleGrob(c(.5 - r*costheta, .5 + r*costheta),
                          c(.5 + dy*r*sintheta, .5 + dy*r*sintheta),
                          unit(1, "mm"),
                          gp=gpar(col=NA, fill=rgb(0, 0, 0)))
    circleMask <- fillGrob(grobTree(rectGrob(width=2),
                                    circles),
                           gp=gpar(fill="black"))
    grid.draw(editGrob(circles, gp=gpar(fill=rgb(1, 1, 1, dotalpha))))
    pushViewport(viewport(mask=circleMask))
    grid.line.to(.5 - r*costheta, .5 + dy*r*sintheta,
                 gp=gpar(col=rgb(1, 1, 1, dotalpha), lty="dotted"))
    grid.segments(.5, .5, .5 - r*costheta, .5 + dy*r*sintheta,
                  gp=gpar(col=rgb(1, 1, 1, dotalpha), lty="dotted"))
    grid.segments(.5, .5, .5 + r*costheta, .5 + dy*r*sintheta,
                  gp=gpar(col=rgb(1, 1, 1, dotalpha), lty="dotted"))
    popViewport()
    grid.draw(ras)
    grid.circle(.5, .5, unit(1, "mm"),
                gp=gpar(col=NA, fill=rgb(1, 1, 1, dotalpha)))
    pushViewport(viewport(mask=ras))
    grid.segments(.5, .5, .5  - r*costheta, .5 + dy*r*sintheta,
                  gp=gpar(col=rgb(1, 1, 1, .2), lwd=2))
    grid.segments(.5, .5, .5  + r*costheta, .5 + dy*r*sintheta,
                  gp=gpar(col=rgb(1, 1, 1, .2), lwd=2))
    popViewport()
    grid.move.to(.5 + r*costheta, .5 + dy*r*sintheta)
}

hclDiagram <- function(sRGB=TRUE) {
    grid.newpage()
    grid.rect(gp=gpar(fill="black"))
    pushViewport(viewport(width=.9, height=.9,
                          layout=grid.layout(5, 5,
                                             widths=c(1, .5, 2, .5, 1),
                                             heights=c(2, .5, 2, .5, 2),
                                             respect=TRUE),
                          gp=gpar(fill=NA, lwd=2)))

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
    grid.move.to(1, .8)
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
    grid.line.to(0, .8, gp=gpar(col=rgb(1,1,1,dotalpha), lty="dotted"))
    cols <- outer(L, C, function(x, y) colorFromHue(x, y, angle, sRGB))
    grid.raster(cols, width=1, height=1)
    nonNA <- which(!is.na(cols[quantile(L, .2),]))
    grid.segments(max(nonNA)/ncol(cols), .8, 0, .8,
                  gp=gpar(col=rgb(1, 1, 1, .2), lwd=2))
    popViewport(2)

    ## 2, 1
    pushViewport(viewport(layout.pos.col=1, layout.pos.row=3))
    semi()
    cols <- outer(L, rev(C), function(x, y) colorFromHue(x, y, 180, sRGB))
    grid.raster(cols, width=1, height=.5)
    nonNA <- which(!is.na(cols[median(L),]))
    grid.segments(min(nonNA)/ncol(cols), .5, 1, .5,
                  gp=gpar(col=rgb(1, 1, 1, .2), lwd=2))
    grid.move.to(1, .5)
    popViewport()
    ## 2, 2
    pushViewport(viewport(layout.pos.col=3, layout.pos.row=3),
                 viewport(angle=0))
    circ()
    cols <- outer(rev(u), v, function(x, y) colorFromUV(x, y, 50, sRGB))
    grid.raster(cols)
    nonNA <- which(!is.na(cols[median(1:length(v)),]))
    grid.line.to(min(nonNA)/ncol(cols), .5, 
                 gp=gpar(col=rgb(1, 1, 1, dotalpha), lty="dotted"))
    grid.segments(min(nonNA)/ncol(cols), .5, .5, .5,
                  gp=gpar(col=rgb(1, 1, 1, .2), lwd=2))
    grid.circle(.5, .5, unit(.5, "mm"),
                gp=gpar(col=rgb(1, 1, 1, .2)))
    grid.segments(max(nonNA)/ncol(cols), .5, .5, .5,
                  gp=gpar(col=rgb(1, 1, 1, .2), lwd=2))
    grid.move.to(max(nonNA)/ncol(cols), .5)
    popViewport(2)
    ## 2, 3
    pushViewport(viewport(layout.pos.col=5, layout.pos.row=3))
    semi(FALSE)
    cols <- outer(L, C, function(x, y) colorFromHue(x, y, 0, sRGB))
    grid.raster(cols, width=1, height=.5)
    nonNA <- which(!is.na(cols[median(L),]))
    grid.line.to(0, .5, 
                 gp=gpar(col=rgb(1, 1, 1, dotalpha), lty="dotted"))
    grid.segments(max(nonNA)/ncol(cols), .5, 0, .5,
                  gp=gpar(col=rgb(1, 1, 1, .2), lwd=2))
    popViewport()

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
    grid.move.to(1, .2)
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
    grid.line.to(0, .2, gp=gpar(col=rgb(1,1,1,dotalpha), lty="dotted"))
    cols <- outer(L, C, function(x, y) colorFromHue(x, y, 360 - angle, sRGB))
    grid.raster(cols, width=1, height=1)
    nonNA <- which(!is.na(cols[quantile(L, .8),]))
    grid.segments(max(nonNA)/ncol(cols), .2, 0, .2,
                  gp=gpar(col=rgb(1, 1, 1, .2), lwd=2))
    popViewport(2)
}

png("hcl-srgb.png", width=600, height=800)
hclDiagram()
dev.off()

pause <- function() {
    png("hcl.png", width=600, height=800)
    hclDiagram(sRGB=FALSE)
    dev.off()
}
