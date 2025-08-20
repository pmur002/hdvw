
## Generate two images to go on separate pages in PDF output

library(grid)

luminance <- 60
chroma <- 60

circles <- function(n, change=FALSE, seed=123) {
    set.seed(seed)
    grid.rect(width=.8, height=.8)
    m <- matrix(1:360, ncol=n)
    row <- sample(1:nrow(m), 1)
    rowOrder <- sample(1:ncol(m))
    cols <- hcl(m[row, rowOrder], chroma, luminance)
    if (change) {
        old <- sample(1:n, n %/% 2)
        new <- sample((1:ncol(m))[-old], n %/% 2)
        if (row < 21) {
            cols[old] <- hcl(m[row + 20, rowOrder][new], chroma, luminance)
        } else {
            cols[old] <- hcl(m[row - 20, rowOrder][new], chroma, luminance)
        }
    }
    if (n == 1) {
        x <- .5
        y <- .5
    } else {
        t <- pi/2 + seq(0, 2*pi, length.out=n + 1)[-(n + 1)]
        r <- .2
        x <- .5 + r*cos(t)
        y <- .5 + r*sin(t)
    }
    grid.circle(x, y, r=.05, gp=gpar(fill=cols))
}

pdf("static-1.pdf", width=6, height=2)
pushViewport(viewport(layout=grid.layout(1, 3, respect=TRUE)))

pushViewport(viewport(layout.pos.col=1))
circles(3)
popViewport()

pushViewport(viewport(layout.pos.col=2))
circles(5)
popViewport()

pushViewport(viewport(layout.pos.col=3))
circles(8)
popViewport()

popViewport()
dev.off()

pdf("static-2.pdf", width=6, height=2)
pushViewport(viewport(layout=grid.layout(1, 3, respect=TRUE)))

pushViewport(viewport(layout.pos.col=1))
circles(3, change=TRUE)
popViewport()

pushViewport(viewport(layout.pos.col=2))
circles(5, change=TRUE)
popViewport()

pushViewport(viewport(layout.pos.col=3))
circles(8, change=TRUE)
popViewport()

popViewport()
dev.off()
