
library(grid)

model <- function(verbal=FALSE) {
    grid.rect(gp=gpar(col=NA, fill=linearGradient(c("white", "grey80"),
                                                  x1=.5, x2=.5)))
    pushViewport(viewport(layout=grid.layout(5, 6)))

    label <- function(text, col, row, gp=gpar(fontface="plain"),
                      x=.5, hjust=.5) {
        pushViewport(viewport(layout.pos.col=col, layout.pos.row=row))
        grid.text(text, gp=gp, x=x, hjust=hjust)
        popViewport()
    } 
    pointer <- function(col, rows) {
        mask <- rectGrob(gp=gpar(col=NA,
                                 fill=linearGradient(c(NA,
                                                       "white"),
                                                     x1=.5,
                                                     x2=.5)))
        pushViewport(viewport(layout.pos.col=col, layout.pos.row=rows,
                              mask=mask))
        grid.segments(.5, 0, .5, 1,
                      arrow=arrow(angle=20, length=unit(3, "mm"),
                                  type="closed"),
                      gp=gpar(fill="black", linejoin="mitre", lwd=2))
        popViewport()
    }

    label("visual\nfeatures", 1, 5, gp=gpar(fontface="bold"))
    pointer(1, 2)
    label("visual\nshapes", 1, 3, gp=gpar(fontface="bold"))
    pointer(1, 4)
    if (verbal) {
        label("visual\nobjects", 1, 1, gp=gpar(fontface="bold"),
              hjust=1)
        label("+", 1, 1, gp=gpar(fontface="bold"),
              x=.6)
        label("text", 1, 1, gp=gpar(fontface="bold"),
              x=.7, hjust=0)
    } else {
        label("visual\nobjects", 1, 1, gp=gpar(fontface="bold"))
    }

    col <- 2
    label("faster\n", col, 5)
    pointer(col, 2:4)
    label("\nslower", col, 1)

    col <- 3
    label("many\n", col, 5)
    pointer(col, 2:4)
    label("\nfew", col, 1)

    col <- 6
    label("simple\n", col, 5)
    pointer(col, 2:4)
    label("\ncomplex", col, 1)

    col <- 4
    label("subconscious\n", col, 5)
    pointer(col, 2:4)
    label("\nconscious", col, 1)

    col <- 5
    label("fleeting\n", col, 5)
    pointer(col, 2:4)
    label("\npersistent", col, 1)

    popViewport()
}

png("model.png", width=800, height=400, res=100)
## grid.newpage()
model()
dev.off()

png("model-text.png", width=800, height=400, res=100)
## grid.newpage()
model(verbal=TRUE)
dev.off()
