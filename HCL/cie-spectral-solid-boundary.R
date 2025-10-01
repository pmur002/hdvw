
xyz <- read.csv("cie_xyz_spectral_boundary.csv")

viewXYZ <- function() {
    library(rgl)
    plot3d(xyz)   
    points3d(0, 0, 0, col="red", size=10)
    points3d(xyzMax, col="red", size=10)
    points3d(seq(0, xyzMax[1], length.out=50),
             seq(0, xyzMax[2], length.out=50),
             seq(0, xyzMax[3], length.out=50),
             col="red")            
}

xyzMax <- c(xyz[,1][which.max(xyz[,1])],
            xyz[,2][which.max(xyz[,2])],
            xyz[,3][which.max(xyz[,3])])
lineOfGreys <- cbind(seq(0, xyzMax[1], length.out=50),
                     seq(0, xyzMax[2], length.out=50),
                     seq(0, xyzMax[3], length.out=50))

library(colorspace)
luv <- coords(as(XYZ(xyz[,1], xyz[,2], xyz[,3]), "LUV"))

viewLUV <- function() {
    library(rgl)
    plot3d(luv)
    points3d(coords(as(XYZ(lineOfGreys[,1],
                           lineOfGreys[,2],
                           lineOfGreys[,3]), "LUV")),
             col="red")
}

library(gMOIP)
hull <- convexHull(as.matrix(luv[!duplicated(luv), ]))
plotHull3D(as.matrix(luv[!duplicated(luv), ]))

## plotPoints3D(c(50, 0, 0), addText=TRUE)
inHull(coords(as(polarLUV(50, 0, 0), "LUV")), luv[!duplicated(luv), ])
inHull(coords(as(polarLUV(50, 0:10, 0), "LUV")), luv[!duplicated(luv), ])
inHull(coords(as(polarLUV(0, 50, 0), "LUV")), luv[!duplicated(luv), ])

## Generate convex hull once
convhull <- geometry::convhulln(luv[!duplicated(luv), ])
inHull(coords(as(polarLUV(0, 50, 0), "LUV")), luv[!duplicated(luv), ],
       hull=convhull)

notrun <- function() {
    z <- read.csv("cie_luv_spectral_boundary.csv")

    library(rgl)
    plot3d(z)

    library(gMOIP)
    hull <- convexHull(as.matrix(z[!duplicated(z), ]))
    plotHull3D(as.matrix(z[!duplicated(z), ]))
}

###############################################################################

doesnotrun <- function() {
    library(cxhull)

    ## Just stops with message "Killed" !?
    hull <- cxhull(as.matrix(z[!duplicated(z), ]), triangulate=TRUE)

    plotConvexHull3d(hull)

    ## Fails to even install
    library(cgalMeshes)
}


