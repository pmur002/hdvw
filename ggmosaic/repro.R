
source("../colours.R")

library(grid)

gtree <- readRDS("spine.rds")
svg("fig-spine.svg", width=8, height=4, bg=figbg)
grid.draw(gtree)
dev.off()
pdf("fig-spine.pdf", width=8, height=4, bg=figbg)
grid.draw(gtree)
dev.off()

gtree <- readRDS("spine-weak.rds")
svg("fig-spine-weak.svg", width=8, height=4, bg=figbg)
grid.draw(gtree)
dev.off()
pdf("fig-spine-weak.pdf", width=8, height=4, bg=figbg)
grid.draw(gtree)
dev.off()

gtree <- readRDS("mosaic.rds")
svg("fig-mosaic.svg", width=8, height=4, bg=figbg)
grid.draw(gtree)
dev.off()
pdf("fig-mosaic.pdf", width=8, height=4, bg=figbg)
grid.draw(gtree)
dev.off()

