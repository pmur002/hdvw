
library(grid)

## Taken from top of how-data-vis-works.qmd
highlight <- "#7D12BA"
## Matching Diagrams/generate.R
library(colorspace)
highrgb <- col2rgb(highlight)/255
highhcl <- coords(as(sRGB(highrgb[1], highrgb[2], highrgb[3]), "polarLUV"))
col1 <- 120
col2 <- 240
cols <- hcl(c(col1, col2), highhcl[2], highhcl[1])

size <- 7
usize <- unit(size, "mm")
cx <- cy <- unit(.5, "npc")
gap <- unit(2, "mm")

dataframe <- function(nr=4, nc=3, 
                      rownames=1:nr, colnames=rev(letters)[nc:1],
                      hl=NULL, hlcol=NULL,
                      colrot=rep(0, nc),
                      coljust=rep("centre", nc),
                      basecol="grey") {
    pushViewport(viewport(layout=grid.layout(nr, nc, respect=TRUE,
                                             widths=usize, heights=usize)))
    for (i in 1:nr) {
        pushViewport(viewport(layout.pos.row=i))
        grid.text(rownames[i], unit(-.5, "lines"))
        popViewport()
        for (j in 1:nc) {
            pushViewport(viewport(layout.pos.row=i, layout.pos.col=j))
            grid.rect(gp=gpar(col=basecol))
            if (i == 1) {
                grid.text(colnames[j], y=unit(1, "npc") + unit(.5, "lines"),
                          rot=colrot[j], just=coljust[j], gp=gpar(col=basecol))
            }
            popViewport()
        }
    }

    highlight <- function(hl, k) {
        hr <- hl$hr
        hc <- hl$hc
        if (!is.null(hr)) {
            if (is.null(hc)) {
                hc <- 1:nc
            }
        } 
        if (!is.null(hc)) {
            if (is.null(hr)) {
                hr <- 1:nr
            }
            for (i in 1:nr) {
                for (j in hc) {
                     pushViewport(viewport(layout.pos.row=i, layout.pos.col=j))
                     if (i == 1) {
                         grid.text(colnames[j], 
                                   y=unit(1, "npc") + unit(.5, "lines"))
                     }
                     grid.rect(gp=gpar(fill=adjustcolor(cols[1], alpha=.5)))
                     popViewport()
                }                
            }
        }
        if (!is.null(hr)) {
            pushViewport(viewport(layout.pos.col=hc, layout.pos.row=hr))
            grid.rect(gp=gpar(lwd=3, fill=adjustcolor(cols[1], alpha=.5)))
            popViewport()
        }
    }

    if (!is.null(hl)) {
        if (is.null(hlcol)) {
            hlcol <- seq_along(hl)
        }
        mapply(highlight, hl, hlcol)
    }
    popViewport()
}

symbols <- function(nr=4, nc=3, 
                    rownames=1:nr, colnames=rev(letters)[nc:1],
                    hl=NULL, hlcol=NULL,
                    colrot=rep(0, nc),
                    coljust=rep("centre", nc),
                    basecol="grey") {
    if (!is.null(hl)) {
        ## ASSUME hl only length 1
        ## (or if not, the components are identical in structure)
        nhr <- length(hl[[1]]$hr)
        if (nhr == 1) {
            heights <- rep(unit.c(usize, gap), length.out=nr + (nr - 1))
        } else {
            heights <- rep(unit.c(rep(unit.c(usize, unit(0, "mm")),
                                      length.out=nhr + (nhr - 1)),
                                  gap), length.out=nr + (nr - 1))
        }
    } else {
        heights <- rep(unit.c(usize, unit(0, "mm")), length.out=nr + (nr - 1))
    }
    pushViewport(viewport(layout=grid.layout(nr + (nr - 1), nc, respect=TRUE,
                                             widths=usize,
                                             heights=heights)))
    for (i in 1:nr) {
        pushViewport(viewport(layout.pos.row=(i - 1)*2 + 1))
        grid.text(rownames[i], unit(-.5, "lines"))
        popViewport()
        for (j in 1:nc) {
            pushViewport(viewport(layout.pos.row=(i - 1)*2 + 1,
                                  layout.pos.col=j))
            grid.rect(gp=gpar(col=basecol))
            if (i == 1) {
                grid.text(colnames[j], y=unit(1, "npc") + unit(.5, "lines"),
                          rot=colrot[j], just=coljust[j], gp=gpar(col=basecol))
            }
            popViewport()
        }
    }

    highlight <- function(hl, k) {
        hr <- hl$hr
        hc <- hl$hc
        if (!is.null(hr)) {
            if (is.null(hc)) {
                hc <- 1:nc
            }
        } 
        if (!is.null(hc)) {
            if (is.null(hr)) {
                hr <- 1:nr
            }
            for (i in 1:nr) {
                for (j in hc) {
                     pushViewport(viewport(layout.pos.row=(i - 1)*2 + 1,
                                           layout.pos.col=j))
                     if (i == 1) {
                         grid.text(colnames[j], 
                                   y=unit(1, "npc") + unit(.5, "lines"))
                     }
                     grid.rect(gp=gpar(fill=adjustcolor(cols[2], alpha=.5^k)))
                     popViewport()
                }                
            }
        }
        if (!is.null(hr)) {
            pushViewport(viewport(layout.pos.col=hc,
                                  layout.pos.row=(hr - 1)*2 + 1))
            grid.rect(gp=gpar(lwd=3, fill=adjustcolor(cols[2], alpha=.5^k)))
            popViewport()
        }
    }

    if (!is.null(hl)) {
        if (is.null(hlcol)) {
            hlcol <- seq_along(hl)
        }
        mapply(highlight, hl, hlcol)
    }
    popViewport()
}

frame <- function(label) {
    ## grid.roundrect(gp=gpar(col=NA, fill="grey95"))
    ## grid.rect(gp=gpar(col=NA, fill="grey95"))
    pushViewport(viewport(clip=rectGrob(y=1, height=unit(1, "lines"), 
                                        just="top")))
    ## grid.roundrect(gp=gpar(col=NA, fill="#666"))
    ## grid.rect(gp=gpar(col=NA, fill="#666"))
    grid.text(label, x=unit(2, "mm"), just="left",
              y=unit(1, "npc") - unit(.5, "lines"),
              gp=gpar(col="black", fontface="bold"))
    popViewport()
}

mapping <- function(hlData, hlSymbol,
                    nrData=4, ncData=3, 
                    nrSymbol=nrData, ncSymbol=ncData,
                    rnData=1:nrData, cnData=letters[1:ncData],
                    rnSymbol=rnData, cnSymbol=rev(letters)[ncData:1]) {
    grid.rect(gp=gpar(col=NA, fill="grey95"))
    pushViewport(viewport(layout=grid.layout(1, 3)))
    pushViewport(viewport(layout.pos.col=1))
    pushViewport(viewport(x=.55, y=.45))
    frame("data")
    dataframe(nrData, ncData, rnData, cnData, hl=hlData)
    popViewport()
    popViewport()
    
    pushViewport(viewport(layout.pos.col=2))
    grid.segments(.2, .5, .8, .5, 
                  arrow=arrow(angle=20, length=unit(3, "mm"), type="closed"),
                  gp=gpar(fill="black"))
    popViewport()
    
    pushViewport(viewport(layout.pos.col=3))
    pushViewport(viewport(x=.55, y=.45))
    frame("symbols")
    symbols(nrSymbol, ncSymbol, rnSymbol, cnSymbol, hl=hlSymbol)
    popViewport()
    popViewport()
}

draw <- function(file, ..., width=800, height=400) {
    png(paste0(file, ".png"), width=width, height=height, res=200)
    mapping(...)
    dev.off()
}

draw("mapping-1-1-1-1",
     list(list(hr=1, hc=1)), ## , list(hr=2, hc=1)),
     list(list(hr=1, hc=1))) ## , list(hr=2, hc=1)))

draw("mapping-1-1-1-2",
     list(list(hr=1, hc=1)), ## , list(hr=2, hc=1)),
     list(list(hr=1, hc=1:2))) ## , list(hr=2, hc=1:2)))

draw("mapping-1-2-1-2",
     list(list(hr=1, hc=1:2)), ## , list(hr=2, hc=1:2)),
     list(list(hr=1, hc=1:2))) ## , list(hr=2, hc=1:2)))

draw("mapping-2-1-1-1",
     list(list(hr=1:2, hc=1)), ## , list(hr=3:4, hc=1)),
     list(list(hr=1:2, hc=1)), ## , list(hr=3:4, hc=1)),
     rnSymbol=c(1, "", 2, ""))

draw("mapping-2-2-1-2",
     list(list(hr=1:2, hc=1:2)), ## , list(hr=3:4, hc=1:2)),
     list(list(hr=1:2, hc=1:2)), ## , list(hr=3:4, hc=1:2)),
     rnSymbol=c(1, "", 2, ""))

draw("mapping-n-2-1-2",
     list(list(hr=1:4, hc=1:2)), 
     list(list(hr=1:4, hc=1:2)),
     rnSymbol=c(1, "", "", ""))

draw("mapping-1-3-1-3",
     list(list(hr=1, hc=1:3)), ## , list(hr=2, hc=1:3)),
     list(list(hr=1, hc=1:3))) ## , list(hr=2, hc=1:3)))

draw("mapping-1-2-1-2-reuse",
     list(list(hr=1, hc=1:2)), ## , list(hr=2, hc=1:3)),
     list(list(hr=1, hc=1:2)), ## , list(hr=2, hc=1:3)),
     cnSymbol=c(rep("x", 2), "y"))

draw("mapping-1-n-1-n",
     list(list(hr=1, hc=1:3)), ## , list(hr=2, hc=1:3)),
     list(list(hr=1, hc=1:3)), ## , list(hr=2, hc=1:3)),
     cnSymbol=rep("x", 3))


png("mappings-1.png", width=800, height=200, res=200)
grid.rect(gp=gpar(col=NA, fill="grey95"))
pushViewport(viewport(layout=grid.layout(1, 3, widths=c(5, 1, 5))))
pushViewport(viewport(layout.pos.col=1))
pushViewport(viewport(x=.5, y=.45))
frame("data")
w <- stringWidth("year") + unit(4, "mm")
pushViewport(viewport(layout=grid.layout(1, 3, widths=w,
                                         height=unit(1, "lines"))))
pushViewport(viewport(layout.pos.col=1))
grid.text("year", y=unit(1, "npc") + unit(.5, "lines"),
          x=unit(1, "npc") - unit(1, "mm"), just="right")
grid.rect(gp=gpar(fill="white"))
grid.text(2011,
          x=unit(1, "npc") - unit(1, "mm"), just="right")
popViewport()
pushViewport(viewport(layout.pos.col=2))
grid.text("rate", y=unit(1, "npc") + unit(.5, "lines"),
          x=unit(1, "npc") - unit(1, "mm"), just="right")
grid.rect(gp=gpar(fill="white"))
grid.text(810,
          x=unit(1, "npc") - unit(1, "mm"), just="right")
popViewport()
pushViewport(viewport(layout.pos.col=3))
grid.text("age", y=unit(1, "npc") + unit(.5, "lines"),
          x=unit(1, "npc") - unit(1, "mm"), just="right")
grid.rect(gp=gpar(fill="white"))
grid.text(16,
          x=unit(1, "npc") - unit(1, "mm"), just="right")
popViewport()
popViewport()
popViewport()
popViewport()

pushViewport(viewport(layout.pos.col=2))
grid.segments(.2, .5, .8, .5, 
              arrow=arrow(angle=20, length=unit(3, "mm"), type="closed"),
              gp=gpar(fill="black"))
popViewport()

pushViewport(viewport(layout.pos.col=3))
pushViewport(viewport(x=.5, y=.45))
frame("symbols")
w <- unit.c(rep(stringWidth("2011") + unit(4, "mm"), 2),
            stringWidth("colour") + unit(4, "mm"))
pushViewport(viewport(layout=grid.layout(1, 3, widths=w,
                                         height=unit(1, "lines"))))
pushViewport(viewport(layout.pos.col=1))
grid.text("x", y=unit(1, "npc") + unit(.5, "lines"),
          x=unit(1, "npc") - unit(1, "mm"), just="right")
grid.rect(gp=gpar(fill="white"))
grid.text(2011,
          x=unit(1, "npc") - unit(1, "mm"), just="right")
popViewport()
pushViewport(viewport(layout.pos.col=2))
grid.text("y", y=unit(1, "npc") + unit(.5, "lines"),
          x=unit(1, "npc") - unit(1, "mm"), just="right")
grid.rect(gp=gpar(fill="white"))
grid.text(810,
          x=unit(1, "npc") - unit(1, "mm"), just="right")
popViewport()
pushViewport(viewport(layout.pos.col=3))
grid.text("colour", y=unit(1, "npc") + unit(.5, "lines"),
          x=unit(1, "npc") - unit(1, "mm"), just="right")
grid.rect(gp=gpar(fill="white"))
grid.text("green",
          x=unit(1, "npc") - unit(1, "mm"), just="right")
popViewport()
popViewport()
popViewport()
popViewport()
dev.off()

png("mappings-2.png", width=800, height=200, res=200)
grid.rect(gp=gpar(col=NA, fill="grey95"))
pushViewport(viewport(layout=grid.layout(1, 3, widths=c(5, 1, 5))))
pushViewport(viewport(layout.pos.col=1))
pushViewport(viewport(x=.5, y=.45))
frame("data")
w <- stringWidth("year") + unit(4, "mm")
pushViewport(viewport(layout=grid.layout(1, 3, widths=w,
                                         height=unit(1, "lines"))))
pushViewport(viewport(layout.pos.col=1))
grid.text("year", y=unit(1, "npc") + unit(.5, "lines"),
          x=unit(1, "npc") - unit(1, "mm"), just="right")
grid.rect(gp=gpar(fill="white"))
grid.text(2011,
          x=unit(1, "npc") - unit(1, "mm"), just="right")
popViewport()
pushViewport(viewport(layout.pos.col=2))
grid.text("rate", y=unit(1, "npc") + unit(.5, "lines"),
          x=unit(1, "npc") - unit(1, "mm"), just="right")
grid.rect(gp=gpar(fill="white"))
grid.text(810,
          x=unit(1, "npc") - unit(1, "mm"), just="right")
popViewport()
pushViewport(viewport(layout.pos.col=3))
grid.text("age", y=unit(1, "npc") + unit(.5, "lines"),
          x=unit(1, "npc") - unit(1, "mm"), just="right")
grid.rect(gp=gpar(fill="white"))
grid.text(16,
          x=unit(1, "npc") - unit(1, "mm"), just="right")
popViewport()
popViewport()
popViewport()
popViewport()

pushViewport(viewport(layout.pos.col=2))
grid.segments(.2, .5, .8, .5, 
              arrow=arrow(angle=20, length=unit(3, "mm"), type="closed"),
              gp=gpar(fill="black"))
popViewport()

pushViewport(viewport(layout.pos.col=3))
pushViewport(viewport(x=.5, y=.45))
frame("symbols")
w <- unit.c(stringWidth("2011") + unit(4, "mm"),
            stringWidth("colour") + unit(4, "mm"),
            stringWidth("2011") + unit(4, "mm"))
pushViewport(viewport(layout=grid.layout(1, 3, widths=w,
                                         height=unit(1, "lines"))))
pushViewport(viewport(layout.pos.col=1))
grid.text("x", y=unit(1, "npc") + unit(.5, "lines"),
          x=unit(1, "npc") - unit(1, "mm"), just="right")
grid.rect(gp=gpar(fill="white"))
grid.text(2011,
          x=unit(1, "npc") - unit(1, "mm"), just="right")
popViewport()
pushViewport(viewport(layout.pos.col=2))
grid.text("colour", y=unit(1, "npc") + unit(.5, "lines"),
          x=unit(1, "npc") - unit(1, "mm"), just="right")
grid.rect(gp=gpar(fill="white"))
grid.text("white",
          x=unit(1, "npc") - unit(1, "mm"), just="right")
popViewport()
pushViewport(viewport(layout.pos.col=3))
grid.text("y", y=unit(1, "npc") + unit(.5, "lines"),
          x=unit(1, "npc") - unit(1, "mm"), just="right")
grid.rect(gp=gpar(fill="white"))
grid.text("16",
          x=unit(1, "npc") - unit(1, "mm"), just="right")
popViewport()
popViewport()
popViewport()
popViewport()
dev.off()

png("mapping-nested.png", width=1200, height=400, res=200)
## grid.newpage()
grid.rect(gp=gpar(col=NA, fill="grey95"))
pushViewport(viewport(layout=grid.layout(1, 5)))
pushViewport(viewport(layout.pos.col=1))
pushViewport(viewport(x=.55, y=.45))
frame("data")
dataframe(4, 3, 1:4, letters[1:3], list(list(hr=1:2, hc=1)))
popViewport()
popViewport()

pushViewport(viewport(layout.pos.col=2))
grid.segments(.2, .5, .8, .5, 
              arrow=arrow(angle=20, length=unit(3, "mm"), type="closed"),
              gp=gpar(fill="black"))
popViewport()

pushViewport(viewport(layout.pos.col=3))
pushViewport(viewport(x=.55, y=.45))
frame("symbols")
symbols(4, 3, c(1, "", 2, ""), c("z", "b", "c"), 
        list(list(hr=1:2, hc=1),
             list(hr=1, hc=2:3)))
popViewport()
popViewport()

pushViewport(viewport(layout.pos.col=4))
grid.segments(.2, .5, .8, .5, 
              arrow=arrow(angle=20, length=unit(3, "mm"), type="closed"),
              gp=gpar(fill="black"))
popViewport()

pushViewport(viewport(layout.pos.col=5))
pushViewport(viewport(x=.55, y=.45))
frame("symbols")
symbols(4, 3, 1:4, rev(letters)[3:1],
        list(list(hr=1, hc=1:2)), hlcol=2)
popViewport()
popViewport()
dev.off()
