
library(gridSVG)
library(grid)

luminance <- 60
chroma <- 60
hues <- runif(10, 0, 360)

cols <- hcl(hues, chroma, luminance)

t <- seq(0, 2*pi, length.out=360)
x <- unit(.5, "npc") + unit(cos(t), "in")
y <- unit(.5, "npc") + unit(sin(t), "in")

grid.newpage()
## Stimuli
grid.circle(r=unit(1, "cm"), gp=gpar(col=cols, fill=cols),
            name="stimulus")

## gridSVG (luminance) mask
pushMask(mask(circleGrob(r=unit(1, "in"), gp=gpar(lwd=40, col="white"))))
grid.segments(.5, .5, x, y,
              gp=gpar(col=hcl(t/pi*180, chroma, luminance), lwd=3),
              name="match")
popMask()

grid.garnish("stimulus", onclick="phaseTwo(this)")
grid.garnish("stimulus", class="stimulus")

grid.garnish("match", onclick=rep("phaseOne(this)", 360), group=FALSE)
grid.garnish("match", visibility="hidden")

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

