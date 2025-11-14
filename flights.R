
## https://www.bts.gov/topics/airlines-and-airports/airline-codes
airlineCodes <- read.table("Data/airline-codes.txt", sep="\t", header=TRUE)
## Two codes missing!
airlineCodes <- rbind(airlineCodes,
                      data.frame(CARRIER=c("US", "VX"),
                                 CARRIERNAME=c("US Airways", "Virgin America")))
flights <- merge(ontime, airlineCodes, by.x="UniqueCarrier", by.y="CARRIER")
flights$airline <- gsub(" (Inc[.]|Co[.]|LLC).*$", "", flights$CARRIERNAME)
flights$taxi <- flights$TaxiOut + flights$TaxiIn
flights <- flights[!is.na(flights$taxi), ]
## Order by median
flights$airline <- reorder(flights$airline, flights$taxi, median)

## source("lvcalc.R")
## df <- subset(flights, UniqueCarrier == "AA")
## letterValues(df$taxi)

## Generate various data frames for drawing different shapes
taxiList <- split(flights$taxi, flights$airline)
letterValues <- function(x) {
    k = determineDepth(length(x), alpha=.95)
    lvtable(x, k)
}
LV <- lapply(taxiList, letterValues)
LVnamed <- mapply(function(x, y) 
                      cbind(as.data.frame(x), 
                            airline=factor(y, levels=levels(flights$airline))), 
                  LV, names(LV), SIMPLIFY=FALSE)
LVdf <- do.call(function(...) rbind(..., make.row.names=FALSE), LVnamed)

outliers <- function(x, lv) {
    lower <- x < lv[, "LV"][1] 
    upper <- x > lv[, "LV"][nrow(lv)]
    side <- ifelse(lower, "lower",
                   ifelse(upper, "upper", NA))
    data.frame(taxi=x[lower | upper], side=side[lower | upper])
}
out <- mapply(outliers, taxiList, LV, SIMPLIFY=FALSE)
outNamed <- mapply(function(x, y) 
                       data.frame(x,
                                  airline=factor(y, 
                                                 levels=levels(flights$airline)), 
                                                 row.names=NULL),
                   out, names(out), SIMPLIFY=FALSE)
outdf <- do.call(function(...) rbind(..., make.row.names=FALSE), outNamed)

calcSpike <- function(x) {
    x <- as.data.frame(x)
    n <- nrow(x)
    maxDepth <- max(x$depth)
    middle <- which(rownames(x) == "M")
    lv1 <- x$LV[1:(middle - 1)]
    dx1 <- diff(x$LV[1:middle])
    spikes1 <- diff(x$depth[1:middle]) / maxDepth / dx1
    spikes1[!is.finite(spikes1)] <- NA
    lv2 <- x$LV[(middle + 1):n]
    dx2 <- diff(x$LV[middle:n])
    spikes2 <- -diff(x$depth[middle:n]) / maxDepth / dx2
    spikes2[!is.finite(spikes2)] <- NA
    level <- c(1, rep(2:(middle-1), each=2, length.out=middle - 2))
    medspike <- max(spikes1[middle - 1], spikes2[1])
    data.frame(lv=c(lv1, x$LV[middle], lv2), 
               level=factor(c(rev(level), 1, level)),
               median=x$LV[middle], medlevel=factor(1),
               medspike=medspike,
               spike=c(spikes1, medspike, spikes2))
}
spike <- lapply(LV, calcSpike)
spikeNamed <- mapply(cbind,
                     spike, 
                     airline=factor(names(spike), 
                                    levels=levels(flights$airline)), 
                     SIMPLIFY=FALSE)
spikedf <- do.call(function(...) rbind(..., make.row.names=FALSE), spikeNamed)

calcRect <- function(x) {
    x <- as.data.frame(x)
    n <- nrow(x)
    maxDepth <- max(x$depth)
    middle <- which(rownames(x) == "M")
    left1 <- x$LV[1:(middle - 1)]
    right1 <- x$LV[2:middle]
    height1 <- diff(x$depth[1:middle]) / maxDepth / (right1 - left1)
    height1[!is.finite(height1)] <- NA
    left2 <- x$LV[middle:(n - 1)]
    right2 <- x$LV[(middle + 1):n]
    height2 <- -diff(x$depth[middle:n]) / maxDepth / (right2 - left2)
    height2[!is.finite(height2)] <- NA
    level <- c(1, rep(2:(middle-1), each=2, length.out=middle - 2))
    data.frame(left=c(left1, left2), 
               right=c(right1, right2),
               level=factor(c(rev(level), level)),
               height=c(height1, height2))
}
rect <- lapply(LV, calcRect)
rectNamed <- mapply(cbind,
                    rect, 
                    airline=factor(names(rect),
                                   levels=levels(flights$airline)), 
                    SIMPLIFY=FALSE)
rectdf <- do.call(function(...) rbind(..., make.row.names=FALSE), rectNamed)

calcBlock <- function(x) {
    x <- as.data.frame(x)
    n <- nrow(x)
    height <- max(rectdf$height, na.rm=TRUE)
    middle <- which(rownames(x) == "M")
    left1 <- x$LV[1:(middle - 1)]
    right1 <- x$LV[2:middle]
    left2 <- x$LV[middle:(n - 1)]
    right2 <- x$LV[(middle + 1):n]
    level <- c(1, rep(2:(middle-1), each=2, length.out=middle - 2))
    data.frame(left=c(left1, left2), 
               right=c(right1, right2),
               side=rep(c("lower", "upper"), c(length(left1), length(left2))),
               level=factor(c(rev(level), level)),
               height=height)
}
block <- lapply(LV, calcBlock)
blockNamed <- mapply(cbind,
                     block, 
                     airline=factor(names(block),
                                    levels=levels(flights$airline)), 
                     SIMPLIFY=FALSE)
blockdf <- do.call(function(...) rbind(..., make.row.names=FALSE), blockNamed)

calcLine <- function(x) {
    x <- as.data.frame(x)
    n <- nrow(x)
    maxDepth <- max(x$depth)
    middle <- which(rownames(x) == "M")
    x1 <- rep(x$LV[1:middle], each=2)[-(middle*2)]
    y1 <- c(0,
            rep(diff(x$depth[1:middle]) / maxDepth / diff(x$LV[1:middle]),
                each=2))
    x1 <- x1[is.finite(y1)]
    y1 <- y1[is.finite(y1)]
    x2 <- rep(x$LV[middle:n], each=2)[-1]
    y2 <- c(rep(-diff(x$depth[middle:n]) / maxDepth / diff(x$LV[middle:n]),
                each=2),
            0)
    x2 <- x2[is.finite(y2)]
    y2 <- y2[is.finite(y2)]
    data.frame(x=c(x1, x2), 
               y=c(y1, y2),
               side=rep(c("lower", "upper"), c(length(x1), length(x2))))
}
line <- lapply(LV, calcLine)
lineNamed <- mapply(cbind,
                    line, 
                    airline=factor(names(line),
                                   levels=levels(flights$airline)), 
                    SIMPLIFY=FALSE)
linedf <- do.call(function(...) rbind(..., make.row.names=FALSE), lineNamed)

calcOutBlock <- function(x, lv) {
    lv <- as.data.frame(lv)
    height <- max(rectdf$height, na.rm=TRUE)
    left <- c(min(x), max(lv$LV))
    right <- c(min(lv$LV), max(x))
    data.frame(left=left, right=right, height=height)
}
outBlock <- mapply(calcOutBlock, taxiList, LV, SIMPLIFY=FALSE)
outBlockNamed <-
    mapply(function(x, y) 
               cbind(x, 
                     airline=factor(y, 
                                    levels=levels(flights$airline)),
                     row.names=NULL),
           outBlock, names(outBlock), SIMPLIFY=FALSE)
outBlockdf <- do.call(function(...) rbind(..., make.row.names=FALSE),
                      outBlockNamed)
