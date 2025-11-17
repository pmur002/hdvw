

source("common.R")

png("NOMADS.png", width=800, height=800, res=200)

par(mar=rep(0, 4), xaxs="i", yaxs="i")
plot.new()
plot.window(xlim=c(xmin, xmax),
            ylim=c(ymin, ymax),
            asp=1)
mult <- sqrt(2)
switch(1,
       {
           pd <- .2
           for (i in seq_along(x0)) {
               cat(i, "\n")
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
              cex=.1*(windStr/max(windStr))))

dev.off()

notrun <- function() {
    map("nz", add=TRUE)
}
