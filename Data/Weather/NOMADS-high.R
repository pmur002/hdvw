
source("common.R")

# Downsampled (original res) wind strength
windStrLo <- sqrt(u.grid.nz^2 + v.grid.nz^2)

plot <- function() {
    filled.contour(x, y, windStrLo,
                   levels=quantile(windStrLo, seq(0, 1, .25)), 
                   color.palette=function(n)
                       rev(hcl.colors(n + 2, "YlOrRd")[-(1:2)]),
                   plot.axes={
                       map("nz",
                           border=NA,
                           fill=TRUE,
                           col=rgb(0,0,0,.5),
                           add=TRUE)
                   },
                   asp=1)
}

library(gridGraphics)

png("NOMADS-high.png", width=800, height=800, res=200)

grid.newpage()
pushViewport(viewport(x=-.15, height=2, width=1.5, just="left"))
grid.echo(plot, newpage=FALSE)
grid.remove("rect|right-axis|box", grep=TRUE, global=TRUE)

dev.off()
