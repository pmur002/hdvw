
library(gridSVG)
library(grid)

luminance <- 60
chroma <- 60
hues <- floor(runif(10, 1, 360))

cols <- hcl(hues, chroma, luminance)

t <- seq(0, 2*pi, length.out=361)[-361]
x <- unit(.5, "npc") + unit(cos(t), "in")
y <- unit(.5, "npc") + unit(sin(t), "in")

pdf(width=3, height=3)

grid.newpage()

## Stimuli
elementName <- function(stem, col) {
    paste0(stem, col)
}
for (i in seq_along(cols)) {
    grid.circle(r=unit(1, "cm"), gp=gpar(col=cols[i], fill=cols[i]),
                name=elementName("stimulus-", hues[i]))
}

## Match
## Draw segments from centre ...
grid.segments(.5, .5, x, y,
              gp=gpar(col=hcl(t/pi*180, chroma, luminance), lwd=3),
              name="match")
## ... but obscure centre with circle
## (which also will intercept click events (when visible)
grid.circle(r=unit(1, "in") - unit(5, "mm"),
            gp=gpar(col="white", fill="white"),
            name="mask")
## Circles to show match
for (i in seq_along(x)) {
    grid.circle(x[i], y[i], r=unit(1, "mm"),
                gp=gpar(fill=NA),
                name=elementName("matchMark-", i))
}
## Circles to show stimuli
for (i in seq_along(cols)) {
    grid.circle(x[hues[i]], y[hues[i]], r=unit(1, "mm"),
                gp=gpar(fill="black"),
                name=elementName("stimMark-", hues[i]))
}

## Result message
grid.text("Error = 00.0", y=.5, name="error")

## Play again
paText <- textGrob("play again", gp=gpar(col="white"), name="play-again-text")
paRect <- roundrectGrob(width=grobWidth(paText) + unit(4, "mm"),
                        height=grobHeight(paText) + unit(4, "mm"),
                        gp=gpar(col="#333", fill="#333"))
paTree <- grobTree(paRect, paText, 
                   vp=viewport(y=unit(1, "lines")),
                   name="play-again")
grid.draw(paTree)

## Starting visibility and onclicks
for (i in seq_along(cols)) {
    grid.garnish(elementName("stimulus-", hues[i]),
                 onclick="state.phaseTwo(this)", class="stimulus")
}
grid.garnish("match", onclick=rep("state.phaseThree(this)", 360), group=FALSE)
grid.garnish("match", visibility="hidden")
grid.garnish("mask", visibility="hidden")
for (i in seq_along(x)) {
    grid.garnish(elementName("matchMark-", i),
                 visibility="hidden", class="matchMark")
}
for (i in seq_along(cols)) {
    grid.garnish(elementName("stimMark-", hues[i]),
                 visibility="hidden", class="stimMark")
}
grid.garnish("error", visibility="hidden")
grid.garnish("play-again", visibility="hidden", onclick="state.phaseOne()")
grid.garnish("play-again-text", "pointer-events"="none")

## For debugging
grid.export("memory.svg")
writeLines(c(readLines("memory.svg"),
             "<script>",
             readLines("memory.js"),
             "</script>"),
           "memory.html")

## Just SVG file
grid.script(paste(readLines("memory.js"), collapse="\n"))
grid.export("memory.svg")
