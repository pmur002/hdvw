

source("common.R")

NOMADS <- function(which) {
    plot.new()
    plot.window(xlim=c(xmin, xmax),
                ylim=c(ymin, ymax),
                asp=1)
    mult <- sqrt(2)
    switch(which,
    {
        pd <- .2
        for (i in seq_along(x0)) {
            ## cat(i, "\n")
            dx <- fine.step*(windStr[i]/max(windStr))*cos(windDir[i])
            dy <- fine.step*(windStr[i]/max(windStr))*sin(windDir[i])
            polygon(c(x0[i], 
                      x0[i] - mult*dx - pd*dy,
                      x0[i] - mult*dx + pd*dy),
                    c(y0[i],
                      y0[i] - mult*dy + pd*dx,
                      y0[i] - mult*dy - pd*dx),
                    col=ifelse(onland[i], 4, 3),
                    border=ifelse(onland[i], 4, 3),
                    lwd=.5,
                    ljoin="mitre")
        }
    },
    segments(x0 - mult*fine.step*(windStr/max(windStr))*cos(windDir),
             y0 - mult*fine.step*(windStr/max(windStr))*sin(windDir),
             x0,
             y0,
             col=ifelse(onland, 4, 5),
             lwd=1),
    points(x0, y0,
           col=ifelse(onland, 4, 5),
           cex=.1*(windStr/max(windStr))),
    {
        col <- ifelse(windDir < 3/4*pi & windDir > 1/4*pi, 1, 
                      ifelse(windDir > -1/4*pi & windDir < 1/4*pi, 2,
                             ifelse(windDir < -3/4*pi | windDir > 3/4*pi, 3,
                                    4)))
        points(x0, y0, pch=21,
               col=col, bg=col,
               cex=.25*(windStr/max(windStr)))
    })
}

png("NOMADS.png", width=800, height=800, res=200)
par(mar=rep(0, 4), xaxs="i", yaxs="i")
NOMADS(1)
dev.off()

png("NOMADS-dots.png", width=800, height=800, res=200)
par(mar=c(0, 0, 2, 0), xaxs="i", yaxs="i")
NOMADS(4)
legend(mean(x0), -33.5, xjust=.5, yjust=0,
       c("North", "East", "West", "South"),
       col=1:4, pt.bg=1:4, pch=21, pt.cex=.2, cex=.3, bty="n",
       horiz=TRUE, xpd=NA)
dev.off()

png("NOMADS-barb.png", width=800, height=800, res=200)
par(mar=c(1, 1, 3, 1), xaxs="i", yaxs="i", xpd=NA)
subset <- row(u.grid.nz.fine) > 80 & row(u.grid.nz.fine) < 120 &
    col(u.grid.nz.fine) > 30 & col(u.grid.nz.fine) < 70
xx <- x0[subset] ## rep(x, length(x))
yy <- y0[subset] ## rep(y, each=length(x))
direction <-
    -(180*(atan2(v.grid.nz.fine, u.grid.nz.fine) + pi/2)/pi)[subset]
speed <- sqrt(u.grid.nz.fine^2 + v.grid.nz.fine^2)[subset]
library(thunder)
plot(xx, yy, type="n", axes=FALSE, asp=1)
for (i in seq_along(xx)) {
    ## 1 metre per sec is approximately 2 knots
    windbarbs(xx[i], yy[i], direction[i], 2*speed[i], cex=.3)
}
library(gridGraphics)
key <- function(x, speed) {
    pushViewport(viewport(unit(.5, "npc") + unit(x, "lines"),
                          unit(1, "npc") - unit(1, "lines")))
    grid.echo(function() {
        par(mar=rep(0, 4)); plot.new(); windbarbs(.5, .5, 45, speed, cex=.5)
    },
    newpage=FALSE)
    grid.text(speed, just=c("right", "bottom"), gp=gpar(cex=.7))
    popViewport()
}
key(-6, 1)
key(-4, 5)
key(-2, 10)
key(0, 15)
key(2, 20)
key(4, 25)
key(6, 30)
dev.off()

notrun <- function() {
    map("nz", add=TRUE)
}
