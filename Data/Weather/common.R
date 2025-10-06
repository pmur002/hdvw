
## Gather wind data
library(rNOMADS)

if (file.exists("u.grid.rds")) {
    u.grid <- readRDS("u.grid.rds")
    v.grid <- readRDS("v.grid.rds")
} else {
    model.urls <- GetDODSDates("gfs_0p50")
    latest.model <- tail(model.urls$url, 1)
    model.runs <- GetDODSModelRuns(latest.model)
    latest.model.run <- tail(model.runs$model.run, 1)

    time <- c(0, 0) #Analysis run, index starts at 0
    lon <- c(0, 719) #All 720 longitude points
    lat <- c(0, 360) #All 361 latitude points

    ## Find out info about variables available
    runonce <- function() {
        model.info <- GetDODSModelRunInfo(latest.model,
                                          tail(model.runs$model.run, 1))
        model.info[grep("wind", model.info)]
    }

    ## u-component wind NZ
    variable <- "ugrd10m"
    model.data <- DODSGrab(latest.model, latest.model.run,
                           variable, time, lon, lat)
    u.grid <- ModelGrid(model.data, c(0.5, 0.5))
    ## v-component wind NZ
    variable <- "vgrd10m"
    model.data <- DODSGrab(latest.model, latest.model.run,
                           variable, time, lon, lat)
    v.grid <- ModelGrid(model.data, c(0.5, 0.5))
    
    saveRDS(u.grid, "u.grid.rds")
    saveRDS(v.grid, "v.grid.rds")
}

## Square to make it easy to interpolate
xmin <- 166
xmax <- 179
ymin <- -47
ymax <- -34
u.grid.nz <- u.grid$z[1,1,
                      u.grid$x >= xmin & u.grid$x <= xmax,
                      u.grid$y >= ymin & u.grid$y <= ymax]
v.grid.nz <- v.grid$z[1,1,
                      v.grid$x >= xmin & v.grid$x <= xmax,
                      v.grid$y >= ymin & v.grid$y <= ymax]

## Interpolate u- and v-
library(akima)
x <- seq(xmin, xmax, .5)
y <- seq(ymin, ymax, .5)
fine.step <- .1
x.fine <- seq(xmin, xmax, fine.step)
y.fine <- seq(ymin, ymax, fine.step)
n.fine <- length(x.fine)
x0 <- rep(x.fine, n.fine)
y0 <- rep(y.fine, each=n.fine)
u.grid.nz.fine <- bilinear(x, y, u.grid.nz, x0, y0)$z
dim(u.grid.nz.fine) <- c(n.fine, n.fine)
v.grid.nz.fine <- bilinear(x, y, v.grid.nz, x0, y0)$z
dim(v.grid.nz.fine) <- c(n.fine, n.fine)
    
## wind strength
windStr <- sqrt(u.grid.nz.fine^2 + v.grid.nz.fine^2)
## wind direction
windDir <- atan2(v.grid.nz.fine, u.grid.nz.fine)

## which grid points are within NZ?
library(maps)
nz2 <- map("nz", c("North", "South"), fill=TRUE, plot=FALSE)
onland <- is.na(map.where(nz2, x0, y0))

