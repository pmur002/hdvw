
figbg <- "#F2F2F2"
highlight <- "#7D12BA" ## Match text code colour (more precise than "purple")

library(colorspace)

highrgb <- col2rgb(highlight)/255
highhcl <- coords(as(sRGB(highrgb[1], highrgb[2], highrgb[3]), "polarLUV"))

col1 <- 40
col2 <- 180
col3 <- 0
cols <- hcl(c(col1, col2, col3), highhcl[2], c(70, 60, 60))

colsDarker <- darken(cols, .3)
colsDarkest <- darken(colsDarker, .3)

## Try ColorBrewer palettes

oranges <- scales::pal_brewer(palette="Oranges")(5)[3:5]
greens <- scales::pal_brewer(palette="Greens")(5)[3:5]
reds <- scales::pal_brewer(palette="Reds")(5)[3:5]

cols <- c(oranges[1], greens[1], reds[1])
colsDarker <- c(oranges[2], greens[2], reds[2])
colsDarkest <- c(oranges[3], greens[3], reds[3])
