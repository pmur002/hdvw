
library(png)
library(grid)
library(grImport)

figbg <- "#F2F2F2"
highlight <- "#7D12BA"

path <- "/media/pmur002/ExternalStorage/scratch/Saliency/MASSVIS"

eyetrack <- function(name, threshold=50, area=400) {
    file <- paste0(file.path(path, "targets", name), ".png")
    ## Read the image in
    img <- readPNG(file)
    ## Create thumbnail
    dim <- dim(img)
    if (dim[1] > dim[2]) {
        h <- 150
        w <- h*dim[2]/dim[1]
    } else {
        w <- 150
        h <- w*dim[1]/dim[2]
    }
    notrun <- function() {
        png(paste0(name, "-thumb.png"), width=w, height=h)
        grid.raster(img)
        dev.off()
    }
    system(paste0("convert ", file, " -thumbnail ", w, "x", h,
                  " -unsharp 0x.5 -bordercolor \"black\" -border 1 ",
                  name, "-thumb.png"))
    thumb <- readPNG(paste0(name, "-thumb.png"))
    ## Create feature map
    system(paste0("convert ", file, " -threshold ", threshold, "% ",
                  name, "-bw.png"))
    system(paste0("convert ", name, "-bw.png ",
                  "-define connected-components:area-threshold=", area, " ",
                  "-define connected-components:mean-color=true ",
                  "-connected-components 4 -auto-level ",
                  name, "-objects.png"))
    ## border to retain image size
    system(paste0("convert -bordercolor black -border 1 ",
                   name, "-objects.png ", name, "-border.png"))
    system(paste0("convert ", name, "-border.png ", name, "-border.pbm"))
    system(paste0("potrace ", name, "-border.pbm -o ", name, "-trace.eps"))
    ## Read eye tracking
    fixationDir <- file.path(path, "fixationsByVis", name, "enc")
    fixationFiles <- list.files(fixationDir, full.names=TRUE)
    fixations <- read.csv(fixationFiles[1])
    ## Overlay eye tracking on feature map alongside thumbnail
    PostScriptTrace(paste0(name, "-trace.eps"), paste0(name, "-trace.xml"))
    objects <- readPicture(paste0(name, "-trace.xml"))
    ## Track plot
    if (dim[1] > dim[2]) {
        height <- 4
        width <- height*dim[2]/dim[1]
    } else {
        width <- 4
        height <- width*dim[1]/dim[2]
    }
    svg(paste0(name, "-track.svg"), width=width, height=height)
    pushViewport(viewport(layout=grid.layout(1, 1, widths=w, heights=h,
                                             respect=TRUE)))
    pushViewport(viewport(layout.pos.col=1))
    grid.rect(gp=gpar(col=NA, fill="white"))
    ## grid.raster(img)
    grid.picture(objects, exp=0, use.gc=FALSE,
                 gp=gpar(col="black", fill=NA))
    grid.circle(fixations[[2]]/dim[2], 1 - fixations[[3]]/dim[1], unit(1.5, "mm"),
                gp=gpar(col=highlight, fill=adjustcolor(highlight, alpha=.5)))
    grid.lines(fixations[[2]]/dim[2], 1 - fixations[[3]]/dim[1],
               gp=gpar(col=highlight, lwd=2))
    popViewport(2)
    dev.off()
}

eyetrack("visMost505", 90, 2400)

eyetrack("economist_daily_chart_75", 50, 2000)

