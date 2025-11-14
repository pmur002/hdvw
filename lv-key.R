
lvkeycols <- palette.colors(8, "Okabe-Ito")

lvkey <- function(data, coords) {
    p <- c(2^-(13:1), cumsum(2^-(1:13))[-1])
    x <- qnorm(p)
    y <- dnorm(x)
    y[13] <- y[12]
    vp <- viewport(y=1.07, height=.8*1/14, width=.9,
                   xscale=c(-4, 4), yscale=2*c(-max(y), max(y)),
                   gp=gpar(lwd=2)) 
    xlow <- x[1:12]
    ylow <- y[1:12]
    cols <- lvkeycols
    lowcols <- cols[rev(c(1, rep(2:12, each=2, length.out=11)))]
    xhi <- x[13:24]
    yhi <- y[13:24]
    hhi <- y[14:25]
    hicols <- cols[c(1, rep(2:12, each=2, length.out=11))]
    grey <- adjustcolor(palette.colors(9, "okabe-ito")[9], alpha=.15)
    grobTree(rectGrob(unit(xlow, "native"), 0,
                      unit(diff(x[1:13]), "native"), 1,
                      just=c("left", "bottom"),
                      gp=gpar(col="white", 
                              fill=adjustcolor(lowcols, alpha=.15))),
             rectGrob(unit(xhi, "native"), 0,
                      unit(diff(x[13:25]), "native"), 1,
                      just=c("left", "bottom"),
                      gp=gpar(col="white", 
                              fill=adjustcolor(hicols, alpha=.15))),
             rectGrob(unit(-4, "native"), 0,
                      unit(min(x) - -4, "native"), 1,
                      just=c("left", "bottom"),
                      gp=gpar(col="white", fill=grey)),
             rectGrob(unit(4, "native"), 0,
                      unit(max(x) - 4, "native"), 1,
                      just=c("left", "bottom"),
                      gp=gpar(col="white", fill=grey)),
             linesGrob(rep(x, each=2),
                       c(0, rep(ylow, each=2), rep(yhi[-1], each=2), 0),
                       default.units="native",
                       gp=gpar(lwd=1.5)),
             linesGrob(rep(x, each=2),
                       -c(0, rep(ylow, each=2), rep(yhi[-1], each=2), 0),
                       default.units="native",
                       gp=gpar(lwd=1.5)),
             segmentsGrob(unit(x[13], "native"), unit(y[13], "native"),
                 unit(x[13], "native"), unit(-y[13], "native")),
             textGrob(c("M", LETTERS[6:1], LETTERS[26:23]),
                      unit(x[13:23], "native"), unit(-.7, "lines"),
                      gp=gpar(fontsize=8)),
             latexGrob(paste0("$\\sfrac{1}{", 2^(8:2), "}$"),
                       unit(x[7:13] - diff(x[6:13])/2, "native"),
                       unit(0:1, "npc") +
                       unit(c(-.7, .7), "lines"),
                       packages=LaTeXpackage("xfrac", 
                                             preamble="\\usepackage{xfrac}"),
                       engine="xetex",
                       gp=gpar(fontsize=8)),
             pointsGrob(unit(c(-3.8, 3.8, -4, 4), "native"),
                        unit(rep(0, 4), "native"),
                        size=unit(2, "mm"),
                        pch=21, gp=gpar(col="white", fill="black", lwd=.5)),
             vp=vp)
}
