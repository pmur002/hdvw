
source("common.R")

png("NOMADS-barb.png", width=800, height=800, res=200)
dev.control("enable")

par(mar=c(1, 1, 2, 1), xaxs="i", yaxs="i", xpd=NA)
subset <- row(u.grid.nz.fine) > 91 & row(u.grid.nz.fine) < 131 &
    col(u.grid.nz.fine) > 78 & col(u.grid.nz.fine) < 118
xx <- x0[subset] ## rep(x, length(x))
yy <- y0[subset] ## rep(y, each=length(x))
direction <-
    -(180*(atan2(v.grid.nz.fine, u.grid.nz.fine) + pi/2)/pi)[subset]
speed <- sqrt(u.grid.nz.fine^2 + v.grid.nz.fine^2)[subset]
speedMax <- which.max(speed)
speedMin <- which.min(speed)
library(thunder)
plot(xx, yy, type="n", axes=FALSE, asp=1)
map("nz", c("North.Island"), fill=TRUE, col=NA, add=TRUE)
## library(mapdata)
## map("nzHires", c("North Island"), fill=TRUE, col="grey90", add=TRUE)
for (i in seq_along(xx)) {
    ## 1 metre per sec is approximately 2 knots
    windbarbs(xx[i], yy[i], direction[i], 2*speed[i], cex=.3)
}
library(gridGraphics)
grid.echo()
barbs <- grid.grep("segments", grep=TRUE, global=TRUE)
## grid.edit(barbs[[speedMax]], gp=gpar(col="red"))
## grid.edit(barbs[[speedMin]], gp=gpar(col="green"))
grid.rect(y=1, height=unit(1, "lines"), just="top",
          gp=gpar(col=NA, fill="white"))
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

