
figbg <- "#F2F2F2"
highlight <- "#7D12BA" ## Match text code colour (more precise than "purple")

library(colorspace)
highrgb <- col2rgb(highlight)/255
highhcl <- coords(as(sRGB(highrgb[1], highrgb[2], highrgb[3]), "polarLUV"))
col1 <- 40
col2 <- 180
col3 <- 0
cols <- hcl(c(col1, col2, col3), highhcl[2], c(70, 60, 60))
