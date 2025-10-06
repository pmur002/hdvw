

source("common.R")

library(grid)

cols <- grey(1 - round(windStr)/round(max(windStr)))
dim(cols) <- dim(windStr)

scale <- 2

png("NOMADS-grey.png", width=2*ncol(windStr), height=2*nrow(windStr))

pushViewport(viewport(angle=90))
grid.raster(cols, interpolate=FALSE)
popViewport()

dev.off()

