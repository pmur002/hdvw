

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
            col <- ifelse(onland[i], 4, 3)
            if (i == which.max(windStr))
                col <- 2
            polygon(c(x0[i], 
                      x0[i] - mult*dx - pd*dy,
                      x0[i] - mult*dx + pd*dy),
                    c(y0[i],
                      y0[i] - mult*dy + pd*dx,
                      y0[i] - mult*dy - pd*dx),
                    col=col,
                    border=col,
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
par(mar=c(0, 0, 1, 0), xaxs="i", yaxs="i")
NOMADS(4)
legend(mean(x0), -34, xjust=.5, yjust=0,
       c("Southerly", "Westerly", "Easterly", "Northerly"),
       col=1:4, pt.bg=1:4, pch=21, pt.cex=.25, cex=.4, bty="n",
       horiz=TRUE, xpd=NA)
dev.off()

