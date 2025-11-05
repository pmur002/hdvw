
gg <- readRDS("ggplot.rds")

source("../colours.R")

svg("fig-ternary.svg", width=8, height=4, bg=figbg)
print(gg)
dev.off()

pdf("fig-ternary.pdf", width=8, height=4, bg=figbg)
print(gg)
dev.off()

