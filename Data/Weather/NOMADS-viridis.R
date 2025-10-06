

source("common.R")

library(viridis)
cols <- viridis(ceiling(max(windStr)) + 1, direction=-1)
cols <- cols[round(windStr) + 1]
dim(cols) <- dim(windStr)
    
png("NOMADS-viridis.png", width=2*ncol(windStr), height=2*nrow(windStr))

pushViewport(viewport(angle=90))
grid.raster(cols, interpolate=FALSE)
popViewport()

dev.off()

