

source("common.R")

library(grid)

cols <- rainbow(ceiling(max(windStr)) + 1)
cols <- cols[round(windStr) + 1]
dim(cols) <- dim(windStr)

scale <- 2

png("NOMADS-rainbow.png", width=2*ncol(windStr), height=2*nrow(windStr))

pushViewport(viewport(angle=90))
grid.raster(cols, interpolate=FALSE)
popViewport()

dev.off()

