grid.newpage()
features <- c("position", "length", "angle", "pattern", "area", "hue", 
              "chroma", "luminance")
hues <- c(0, 80, 160, 240)
pch <- c(0, 1, 3, 4)
pushViewport(viewport(x=1, width=unit(1, "npc") - max(stringWidth(features)),
                      y=0, height=unit(1, "npc") - unit(1, "lines"),
                      just=c("right", "bottom"),
                      layout=grid.layout(8, 8, respect=TRUE)))
for (i in 1:8) {
    pushViewport(viewport(layout.pos.col=i))
    grid.text(features[i], y=unit(1, "npc"), just="bottom")
    popViewport()
    pushViewport(viewport(layout.pos.row=i, layout.pos.col=i))
    grid.text(features[i], x=unit(0, "npc"), just="right")
    popViewport()
}
## Position
pushViewport(viewport(layout.pos.col=1, layout.pos.row=1),
             viewport(width=.8, height=.8, gp=gpar(linejoin="mitre")))
grid.rect(gp=gpar(fill="white", lwd=3))
pushViewport(viewport(width=.8, height=.8))
x <- 1:4/5
y <- 1:4/5
grid.circle(x, y, unit(1, "mm"), gp=gpar(fill="black"))
popViewport(3)
pushViewport(viewport(layout.pos.col=2, layout.pos.row=1),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", col=NA))
pushViewport(viewport(width=.8, height=.8))
l <- seq(.05, .15, length.out=4)
grid.rect(1:4/5, .425, .05, l, just="bottom",
          gp=gpar(fill="black"))
popViewport(3)
pushViewport(viewport(layout.pos.col=3, layout.pos.row=1),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", col=NA))
pushViewport(viewport(width=.8, height=.8))
for (i in 1:4) {
    pushViewport(viewport(i/5, .5, .2, .2, angle=90 + (i-1)*20))
    grid.segments(0, .5, 1, .5, gp=gpar(lwd=2, lineend="butt"))
    popViewport()
}
popViewport(3)
pushViewport(viewport(layout.pos.col=4, layout.pos.row=1),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", col=NA))
pushViewport(viewport(width=.9, height=.9))
cols <- hcl(0, 50, 4:1/5*100)
grid.points(1:4/5, rep(.5, 4), size=unit(2, "mm"), pch=pch,
            gp=gpar(lwd=1))
popViewport(3)
pushViewport(viewport(layout.pos.col=5, layout.pos.row=1),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", col=NA))
pushViewport(viewport(width=.9, height=.9))
w <- seq(.05, .15, length.out=4)
grid.rect(1:4/5, .5, w, w,
          gp=gpar(fill="black"))
popViewport(3)
pushViewport(viewport(layout.pos.col=6, layout.pos.row=1),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", col=NA))
pushViewport(viewport(width=.9, height=.9))
cols <- hcl(hues, 50, 70)
grid.circle(1:4/5, .5, unit(1, "mm"),
            gp=gpar(col=cols, fill=cols))
popViewport(3)
pushViewport(viewport(layout.pos.col=7, layout.pos.row=1),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", col=NA))
pushViewport(viewport(width=.9, height=.9))
cols <- hcl(0, 4:1/5*100, 70)
grid.circle(1:4/5, .5, unit(1, "mm"),
            gp=gpar(col=cols, fill=cols))
popViewport(3)
pushViewport(viewport(layout.pos.col=8, layout.pos.row=1),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", col=NA))
pushViewport(viewport(width=.9, height=.9))
cols <- hcl(0, 50, 4:1/5*100)
grid.circle(1:4/5, .5, unit(1, "mm"),
            gp=gpar(col=cols, fill=cols))
popViewport(3)
## Length
pushViewport(viewport(layout.pos.col=2, layout.pos.row=2),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", lwd=3))
pushViewport(viewport(width=.8, height=.8))
l <- seq(.05, .15, length.out=4)
h <- rep(l, 4)
w <- rep(l, each=4)
grid.rect(rep(1:4/5, 4), rep(4:1/5, each=4) - max(l)/2, w, h,
          just=c("left", "bottom"),
          gp=gpar(fill="black"))
popViewport(3)
pushViewport(viewport(layout.pos.col=3, layout.pos.row=2),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", lty="dotted"))
pushViewport(viewport(width=.8, height=.8))
l <- seq(.05, .15, length.out=4)
for (j in 1:4) {
    for (i in 4:1) {
        pushViewport(viewport(j/5, i/5, angle=(j - 1)*20))
        grid.rect(.5, .5, l[5 - i], .05, 
                  gp=gpar(fill="black"))
        popViewport()
    }
}
popViewport(3)
pushViewport(viewport(layout.pos.col=4, layout.pos.row=2),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", lty="dotted"))
pushViewport(viewport(width=.9, height=.9))
l <- seq(.05, .15, length.out=4)
for (j in 1:4) {
    for (i in 4:1) {
        name <- paste0("length-pattern-", i, "-", j)
        grid.define(pointsGrob(.5, .5, size=unit(2, "mm"), pch=pch[j],
                               gp=gpar(lwd=1)), name=name)
        pushViewport(viewport(j/5, i/5, width=l[5 - i]/max(l), height=1))
        grid.use(name)
        popViewport()
    }
}
popViewport(3)
pushViewport(viewport(layout.pos.col=5, layout.pos.row=2),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", lwd=3))
pushViewport(viewport(width=.8, height=.8))
w <- seq(.05, .15, length.out=4)
l <- seq(.05, .15, length.out=4)
for (j in 1:4) {
    for (i in 4:1) {
        pushViewport(viewport(j/5, i/5 - max(l)/2))
        grid.rect(.5, .5, l[5 - i], w[j], 
                  just=c("left", "bottom"),
                  gp=gpar(fill="black"))
        popViewport()
    }
}
popViewport(3)
pushViewport(viewport(layout.pos.col=6, layout.pos.row=2),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", col=NA))
pushViewport(viewport(width=.8, height=.8))
cols <- rep(hcl(hues, 50, 70), 4)
l <- seq(.05, .15, length.out=4)
grid.rect(rep(1:4/5, 4), rep(4:1/5 - l/2, each=4), rep(l, each=4), .05, 
          just="left",
          gp=gpar(col=cols, fill=cols))
popViewport(3)
pushViewport(viewport(layout.pos.col=7, layout.pos.row=2),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", col=NA))
pushViewport(viewport(width=.8, height=.8))
cols <- rep(hcl(0, 4:1/5*100, 70), 4)
l <- seq(.05, .15, length.out=4)
grid.rect(rep(1:4/5, 4), rep(4:1/5 - l/2, each=4), rep(l, each=4), .05, 
          just="left",
          gp=gpar(col=cols, fill=cols))
popViewport(3)
pushViewport(viewport(layout.pos.col=8, layout.pos.row=2),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", col=NA))
pushViewport(viewport(width=.8, height=.8))
cols <- rep(hcl(0, 50, 4:1/5*100), 4)
l <- seq(.05, .15, length.out=4)
grid.rect(rep(1:4/5, 4), rep(4:1/5 - l/2, each=4), rep(l, each=4), .05, 
          just="left",
          gp=gpar(col=cols, fill=cols))
popViewport(3)
## Angle
pushViewport(viewport(layout.pos.col=3, layout.pos.row=3),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", lwd=3))
pushViewport(viewport(width=.8, height=.8))
for (j in 1:4) {
    for (i in 4:1) {
        pushViewport(viewport(j/5, i/5, .2, .2,
                              angle=90 + (4 - i)*20 + (j - 1)*20))
        grid.segments(0, .5, 1, .5, gp=gpar(lwd=2, lineend="butt"))
        popViewport()
    }
}
popViewport(3)
pushViewport(viewport(layout.pos.col=4, layout.pos.row=3),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", lwd=3))
pushViewport(viewport(width=.9, height=.9))
for (j in 1:4) {
    for (i in 4:1) {
        name <- paste0("angle-pattern-", i, "-", j)
        pushViewport(viewport(j/5, i/5))
        grid.define(pointsGrob(.5, .5, size=unit(2, "mm"), pch=pch[j],
                               gp=gpar(lwd=1)), name=name)
        popViewport()
        pushViewport(viewport(j/5, i/5, angle=(4 - i)*20))
        grid.use(name)
        popViewport()
    }
}
popViewport(3)
pushViewport(viewport(layout.pos.col=5, layout.pos.row=3),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", lwd=3))
pushViewport(viewport(width=.9, height=.9))
w <- seq(.05, .15, length.out=4)
for (j in 1:4) {
    for (i in 4:1) {
        pushViewport(viewport(j/5, i/5, angle=90 + (4 - i)*20))
        grid.rect(.5, .5, w[j], w[j],
                  gp=gpar(fill="black"))
        popViewport()
    }
}
popViewport(3)
pushViewport(viewport(layout.pos.col=6, layout.pos.row=3),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", col=NA))
pushViewport(viewport(width=.8, height=.8))
cols <- hcl(hues, 50, 70)
for (j in 1:4) {
    for (i in 4:1) {
        pushViewport(viewport(j/5, i/5, .2, .2, angle=90 + (4-i)*20))
        grid.segments(0, .5, 1, .5, gp=gpar(lwd=2, lineend="butt", col=cols[j]))
        popViewport()
    }
}
grid.segments(.1, 1:4/5 - .1, .3, 1:4/5 - .1, gp=gpar(col="white"))
popViewport(3)
pushViewport(viewport(layout.pos.col=7, layout.pos.row=3),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", col=NA))
pushViewport(viewport(width=.8, height=.8))
cols <- hcl(0, 4:1/5*100, 70)
for (j in 1:4) {
    for (i in 4:1) {
        pushViewport(viewport(j/5, i/5, .2, .2, angle=90 + (4-i)*20))
        grid.segments(0, .5, 1, .5, gp=gpar(lwd=2, lineend="butt", col=cols[j]))
        popViewport()
    }
}
grid.segments(.1, 1:4/5 - .1, .3, 1:4/5 - .1, gp=gpar(col="white"))
popViewport(3)
pushViewport(viewport(layout.pos.col=8, layout.pos.row=3),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", col=NA))
pushViewport(viewport(width=.8, height=.8))
cols <- hcl(0, 50, 4:1/5*100)
for (j in 1:4) {
    for (i in 4:1) {
        pushViewport(viewport(j/5, i/5, .2, .2, angle=90 + (4-i)*20))
        grid.segments(0, .5, 1, .5, gp=gpar(lwd=2, lineend="butt", col=cols[j]))
        popViewport()
    }
}
grid.segments(.1, 1:4/5 - .1, .3, 1:4/5 - .1, gp=gpar(col="white"))
popViewport(3)
## Pattern
pushViewport(viewport(layout.pos.col=4, layout.pos.row=4),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", lwd=3))
pushViewport(viewport(width=.9, height=.9))
pch1 <- matrix(ncol=4, nrow=4)
pch1[1, ] <- pch
pch1[, 1] <- pch
pch1[2:4, 2:4] <- pch[2:4]
pch2 <- matrix(ncol=4, nrow=4)
pch2[1, ] <- pch
pch2[, 1] <- pch
pch2[2:4, 2:4] <- rep(pch[2:4], each=3)
for (j in 1:4) {
    for (i in 4:1) {
        grid.points(j/5, i/5, size=unit(2, "mm"), pch=pch1[j, 5 - i],
                    gp=gpar(lwd=1))
        if (j > 1 && i < 4) {
            grid.points(j/5, i/5, size=unit(2, "mm"), pch=pch2[j, 5 - i],
                        gp=gpar(lwd=1))
        }
    }
}
popViewport(3)
pushViewport(viewport(layout.pos.col=5, layout.pos.row=4),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", lty="dotted"))
pushViewport(viewport(width=.9, height=.9))
w <- seq(1, 2, length.out=4)
grid.points(rep(1:4/5, 4), rep(4:1/5, each=4), size=rep(unit(w, "mm"), 4),
            pch=rep(pch, each=4), gp=gpar(lwd=1))
popViewport(3)
pushViewport(viewport(layout.pos.col=6, layout.pos.row=4),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", col=NA))
pushViewport(viewport(width=.9, height=.9))
cols <- hcl(hues, 50, 70)
grid.points(rep(1:4/5, 4), rep(4:1/5, each=4), size=unit(2, "mm"),
            pch=rep(pch, each=4),
            gp=gpar(lwd=1.5, col=rep(cols, 4)))
popViewport(3)
pushViewport(viewport(layout.pos.col=7, layout.pos.row=4),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", col=NA))
pushViewport(viewport(width=.9, height=.9))
cols <- hcl(0, 4:1/5*100, 70)
grid.points(rep(1:4/5, 4), rep(4:1/5, each=4), size=unit(2, "mm"),
            pch=rep(pch, each=4),
            gp=gpar(lwd=1.5, col=rep(cols, 4), fill=rep(cols, 4)))
popViewport(3)
pushViewport(viewport(layout.pos.col=8, layout.pos.row=4),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", col=NA))
pushViewport(viewport(width=.9, height=.9))
cols <- hcl(0, 50, 4:1/5*100)
grid.points(rep(1:4/5, 4), rep(4:1/5, each=4), size=unit(2, "mm"),
            pch=rep(pch, each=4),
            gp=gpar(lwd=1.5, col=rep(cols, 4)))
popViewport(3)
## Area
pushViewport(viewport(layout.pos.col=5, layout.pos.row=5),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", lwd=3))
pushViewport(viewport(width=.9, height=.9))
w <- seq(.05, .15, length.out=4)
w2 <- outer(w, w)
w2 <- min(w) + (w2 - min(w2))/diff(range(w2)) * diff(range(w))
grid.rect(rep(1:4/5, 4), rep(4:1/5, each=4), w2, w2,
          gp=gpar(fill="black"))
popViewport(3)
pushViewport(viewport(layout.pos.col=6, layout.pos.row=5),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", lty="dotted"))
pushViewport(viewport(width=.9, height=.9))
cols <- rep(hcl(hues, 50, 70), 4)
w <- rep(seq(.05, .15, length.out=4), each=4)
grid.rect(rep(1:4/5, 4), rep(4:1/5, each=4), w, w,
          gp=gpar(col=cols, fill=cols))
popViewport(3)
pushViewport(viewport(layout.pos.col=7, layout.pos.row=5),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", lty="dotted"))
pushViewport(viewport(width=.9, height=.9))
cols <- rep(hcl(0, 4:1/5*100, 70), 4)
w <- rep(seq(.05, .15, length.out=4), each=4)
grid.rect(rep(1:4/5, 4), rep(4:1/5, each=4), w, w,
          gp=gpar(col=cols, fill=cols))
popViewport(3)
pushViewport(viewport(layout.pos.col=8, layout.pos.row=5),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", lty="dotted"))
pushViewport(viewport(width=.9, height=.9))
cols <- hcl(0, 50, rep(4:1/5*100, 4))
w <- rep(seq(.05, .15, length.out=4), each=4)
grid.rect(rep(1:4/5, 4), rep(4:1/5, each=4), w, w,
          gp=gpar(col=cols, fill=cols))
popViewport(3)
## Hue
pushViewport(viewport(layout.pos.col=6, layout.pos.row=6),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", lwd=3))
pushViewport(viewport(width=.9, height=.9))
cols <- matrix(ncol=4, nrow=4)
cols[1, ] <- hcl(hues, 50, 70)
cols[, 1] <- hcl(hues, 50, 70)
cols[2:4, 2:4] <- hcl(outer(hues[-1], hues[-1],
                            function(x, y) apply(cbind(x, y), 1, mean)),
                      50, 70)
notrun <- function() {
    rgb <- col2rgb(hcl(hues[-1], 50, 70))/255
    cols1 <- as(sRGB(rep(rgb[1,], 3), rep(rgb[2,], 3), rep(rgb[3,], 3)), "LUV")
    cols2 <- as(sRGB(rep(rgb[1,], each=3), rep(rgb[2,], each=3),
                     rep(rgb[3,], each=3)), "LUV")
    cols[2:4, 2:4] <- hex(mixcolor(.5, cols1, cols2))
}
grid.circle(rep(1:4/5, 4), rep(4:1/5, each=4), unit(1, "mm"),
            gp=gpar(col=cols, fill=cols))
popViewport(3)
pushViewport(viewport(layout.pos.col=7, layout.pos.row=6),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", lty="dotted"))
pushViewport(viewport(width=.9, height=.9))
cols <- hcl(rep(hues, each=4), rep(4:1/5*100, 4), 70)
grid.circle(rep(1:4/5, 4), rep(4:1/5, each=4), unit(1, "mm"),
            gp=gpar(col=cols, fill=cols))
popViewport(3)
pushViewport(viewport(layout.pos.col=8, layout.pos.row=6),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", lty="dotted"))
pushViewport(viewport(width=.9, height=.9))
cols <- hcl(rep(hues, each=4), 50, rep(4:1/5*100, 4))
grid.circle(rep(1:4/5, 4), rep(4:1/5, each=4), unit(1, "mm"),
            gp=gpar(col=cols, fill=cols))
popViewport(3)
## Chroma
pushViewport(viewport(layout.pos.col=7, layout.pos.row=7),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", lwd=3))
pushViewport(viewport(width=.9, height=.9))
cols <- matrix(ncol=4, nrow=4)
cols[1, ] <- hcl(0, 4:1/5*100, 70)
cols[, 1] <- hcl(0, 4:1/5*100, 70)
cols[2:4, 2:4] <- hcl(0, t(outer(3:1/5, 3:1/5))*100, 70)
grid.circle(rep(1:4/5, 4), rep(4:1/5, each=4), unit(1, "mm"),
            gp=gpar(col=cols, fill=cols))
popViewport(3)
pushViewport(viewport(layout.pos.col=8, layout.pos.row=7),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", lwd=3))
pushViewport(viewport(width=.9, height=.9))
cols <- hcl(0, rep(4:1/5*100, each=4), rep(4:1/5*100, 4))
grid.circle(rep(1:4/5, 4), rep(4:1/5, each=4), unit(1, "mm"),
            gp=gpar(col=cols, fill=cols))
popViewport(3)
## Luminance
pushViewport(viewport(layout.pos.col=8, layout.pos.row=8),
             viewport(width=.8, height=.8))
grid.rect(gp=gpar(fill="white", lwd=3))
pushViewport(viewport(width=.9, height=.9))
cols <- matrix(ncol=4, nrow=4)
cols[1, ] <- hcl(0, 50, 4:1/5*100)
cols[, 1] <- hcl(0, 50, 4:1/5*100)
cols[2:4, 2:4] <- hcl(0, 50, t(outer(3:1/5, 3:1/5))*100)
grid.circle(rep(1:4/5, 4), rep(4:1/5, each=4), unit(1, "mm"),
            gp=gpar(col=cols, fill=cols))
popViewport(3)
