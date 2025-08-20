
library(gridSVG)
library(grid)

luminance <- 60
chroma <- 60

t <- seq(0, 2*pi, length.out=361)[-361]
x <- unit(.5, "npc") + unit(cos(t), "in")
y <- unit(.5, "npc") + unit(sin(t), "in")

elementName <- function(stem, col) {
    paste0(stem, col)
}

generate <- function(nstim) {
    ## Sample from separate regions then reorder columns
    hues <- t(apply(apply(matrix(1:360, ncol=nstim), 2,
                          function(col) sample(col, 10)), 1,
                    function(row) sample(row, length(row))))

    cols <- hcl(hues, chroma, luminance)

    pdf(width=3, height=3)

    grid.newpage()

    grid.rect(width=.9, height=.9)
    
    ## Stimuli
    if (nstim == 1) {
        stimX <- rep(.5, 10)
        stimY <- rep(.5, 10)
    } else {
        stimX <- rep(1:nstim/(nstim + 1), each=10)
        stimY <- rep(.5, nstim*10)
        ## stimT <- seq(0, 2*pi, length.out=(nstim + 1))[-(nstim + 1)] + pi/2
        ## stimX <- rep(.5 + .2*cos(stimT), each=10)
        ## stimY <- rep(.5 + .2*sin(stimT), each=10)
    }
    for (i in seq_along(cols)) {
        grid.circle(x=stimX[i], y=stimY[i], r=unit(5, "mm"),
                    gp=gpar(col="black", fill=cols[i]),
                    name=elementName("stimulus-", hues[i]))
    }

    ## Match
    ## Draw segments from centre ...
    grid.segments(.5, .5, x, y,
                  gp=gpar(col=hcl(t/pi*180, chroma, luminance), lwd=3),
                  name="match")
    ## ... but obscure centre with circle
    ## (which also will intercept click events (when visible)
    grid.circle(r=unit(1, "in") - unit(10, "mm"),
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
                       vp=viewport(y=.05),
                       name="play-again")
    grid.draw(paTree)

    ## Starting visibility and onclicks
    grid.garnish("nstim", visibility="hidden")
    for (i in seq_along(cols)) {
        grid.garnish(elementName("stimulus-", hues[i]),
                     onclick=paste0("memory", nstim,
                                    "state.phaseTwo(this)"),
                     class="stimulus")
    }
    grid.garnish("match",
                 onclick=rep(paste0("memory", nstim, "state.phaseThree(this)"),
                             360),
                 group=FALSE)
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
    grid.garnish("play-again", visibility="hidden",
                 onclick=paste0("memory", nstim, "state.phaseOne()"))
    grid.garnish("play-again-text", "pointer-events"="none")

    ## For debugging
    grid.export("memory.svg", prefix=paste0("memory", nstim), xmldecl=NULL)
    writeLines(c(readLines("memory.svg", warn=FALSE),
                 "<script>",
                 paste0("memory", nstim, "state = ("),
                        readLines("memory.js"),
                        paste0(")(", nstim, ");"),
                 "</script>"),
               paste0("memory-", nstim, ".html"))

    ## Just SVG file
    grid.script(paste(c(paste0("memory", nstim, "state = ("),
                        readLines("memory.js"),
                        paste0(")(", nstim, ");")),
                      collapse="\n"))
    grid.export(paste0("memory-", nstim, ".svg"),
                prefix=paste0("memory", nstim),
                xmldecl=NULL)
}

cat("memory-1 ...\n")
generate(1)
cat("memory-3 ...\n")
generate(3)
cat("memory-5 ...\n")
generate(5)
