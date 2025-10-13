

source("common.R")

library(grid)

cols <- rev(hcl.colors(ceiling(max(windStr)) + 1, palette="Purples"))
cols <- cols[round(windStr) + 1]
dim(cols) <- dim(windStr)

scale <- 2

png("NOMADS-purple.png", width=2*ncol(windStr), height=2*nrow(windStr))

pushViewport(viewport(angle=90))
grid.raster(cols, interpolate=FALSE)
popViewport()

dev.off()

