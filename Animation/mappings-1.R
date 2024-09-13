
library(dplyr)
library(ggplot2)
library(ggsci)
library(gggrid)
library(gridExtra)
library(grid)
library(gridSVG)

setwd("..")
source("youth-crime.R")
setwd("Animation")

crime <- subset(crimeAge,
                age %in% c("14", "15", "16"),
                c("year", "rate", "ageFactor"))
names(crime) <- c("year", "rate", "age")
crime$rate <- round(crime$rate)

figbg <- "#F2F2F2"
highlight <- "#7D12BA" 

theme_set(theme_bw())
theme_update(plot.background=element_rect(colour=NA, 
                                          fill=figbg),
             panel.background=element_blank(),
             panel.grid.minor=element_blank(),
             panel.grid.major=element_blank(),
             legend.background=element_blank())

yearShow <- function(data, coords) {
    x <- coords$x[data$x == 2011][1]
    tg <- textGrob("",
                   x=x,
                   y=unit(0, "npc") - unit(.5, "lines"),
                   gp=gpar(col=NA),
                   name="year.end")
    sg <- segmentsGrob(x, unit(-2, "mm"), x, unit(1, "npc") + unit(2, "mm"),
                       gp=gpar(col=adjustcolor(highlight, alpha=0),
                               lty="22", lwd=2),
                       name="year.line")
    grobTree(tg, sg)
}
rateShow <- function(data, coords) {
    y <- coords$y[data$y == 810][1]
    tg <- textGrob("",
                   y=y,
                   x=unit(0, "npc") - unit(1, "lines"),
                   gp=gpar(col=NA),
                   name="rate.end")
    sg <- segmentsGrob(unit(-2, "mm"), y, unit(1, "npc") + unit(2, "mm"), y,
                       gp=gpar(col=adjustcolor(highlight, alpha=0),
                               lty="22", lwd=2),
                       name="rate.line")
    grobTree(tg, sg)
}

gg <- ggplot(crime) +
    geom_line(aes(year, rate, colour=age), linewidth=.5) +
    scale_colour_npg() +
    theme(panel.grid.major.y=element_line(colour="black", linewidth=.1),
          aspect.ratio=1) +
    coord_cartesian(clip="off") +
    grid_panel(yearShow, aes(x=year)) +
    grid_panel(rateShow, aes(y=rate))

nrow <- 9
rowcols <- rep("black", nrow)
rowcols[3] <- highlight
rowface <- rep("plain", nrow)
rowface[3] <- "bold"
gt <-
    tableGrob(head(crime, nrow), rows=NULL,
              theme=ttheme_default(core=list(fg_params=list(col=rowcols,
                                                            fontface=rowface))))

gridsvg("mappings-1.svg", width=8, height=4, res=96)

grid.newpage()
grid.rect(gp=gpar(col=NA, fill=figbg))
pushViewport(viewport(x=1/3, width=2/3, just="left"))
print(gg, newpage=FALSE)
grid.rect(gp=gpar(col=NA, fill=rgb(1,1,1,.7)))
popViewport()
pushViewport(viewport(x=0, width=1/3, just="left"))
grid.draw(gt)
popViewport()

grid.force()
grid.add("key-5-1-1.7-2-7-2", grep=TRUE,
         textGrob("", unit(-.5, "lines"), name="age.end"))
yearCopy <- editGrob(grid.get("core-fg.4-1-4-1::text", grep=TRUE),
                     gp=gpar(col="grey", fontface="plain"), name="year.copy")
grid.add("core-fg.4-1-4-1", grep=TRUE, yearCopy)
grid.reorder("core-fg.4-1-4-1", "year.copy")
rateCopy <- editGrob(grid.get("core-fg.4-2-4-2::text", grep=TRUE),
                     gp=gpar(col="grey", fontface="plain"), name="rate.copy")
grid.add("core-fg.4-2-4-2", grep=TRUE, rateCopy)
grid.reorder("core-fg.4-2-4-2", "rate.copy")
ageCopy <- editGrob(grid.get("core-fg.4-3-4-3::text", grep=TRUE),
                     gp=gpar(col="grey", fontface="plain"), name="age.copy")
grid.add("core-fg.4-3-4-3", grep=TRUE, ageCopy)
grid.reorder("core-fg.4-3-4-3", "age.copy")

dev.off()

library(animaker)

moveYear <- atomic(start=1, dur=1.5, label="moveYear")
showYear <- atomic(dur=0, label="showYear")
year <- vec(moveYear, showYear, label="year")
moveRate <- atomic(dur=1, label="moveRate")
showRate <- atomic(dur=0, label="showRate")
rate <- vec(moveRate, showRate, label="rate")
moveAge <- atomic(dur=2, label="moveAge")
showAge <- atomic(dur=0, label="showAge")
age <- vec(moveAge, showAge, label="age")
anim <- vec(year, rate, age, durn=20, label="anim")
timings <- timing(anim)
names(timings) <- sapply(timings, function(x) x$label)
totalDurn <- sum(sapply(timings, function(x) x$durn))

library(xml2)

svg <- read_xml("mappings-1.svg")
ns <- xml_ns_rename(xml_ns(svg), d1="svg")
yearStartGroup <-
    xml_find_first(svg, '//svg:g[@id = "core-fg.4-1-4-1.1"]/svg:g[2]/svg:g', ns)
yearEndGroup <- xml_find_first(svg, '//svg:g[@id = "year.end.1.1"]', ns)
yearLine <- xml_find_first(svg, '//svg:polyline[@id = "year.line.1.1"]', ns)
xml_set_attr(yearStartGroup, "style",
             "filter: drop-shadow(3px -3px 2px rgba(0,0,0,.6)")
xml_set_attr(yearLine, "style",
             "filter: drop-shadow(3px -3px 2px rgba(0,0,0,.8)")
yearStart <- as.numeric(strsplit(gsub("[^0-9. ]", "",
                                      xml_attr(yearStartGroup, "transform")),
                                 " ")[[1]])
yearEnd <- as.numeric(strsplit(gsub("[^0-9. ]", "",
                                    xml_attr(yearEndGroup, "transform")),
                               " ")[[1]])
yearDiff <- yearEnd - yearStart
xml_add_child(yearStartGroup,
              "animateMotion",
              path=paste0("M0,0",
                          " C0,", yearDiff[2]/2,
                          " ", yearDiff[1]/2, ",", yearDiff[2],
                          " ", yearDiff[1], ",", yearDiff[2]),
              begin=paste0(timings$moveYear$start, "s"),
              dur=paste0(timings$moveYear$durn, "s"),
              fill="freeze",
              restart="always")
xml_add_child(yearLine,
              "set",
              attributeName="stroke-opacity",
              to="1",
              begin=paste0(timings$showYear$start, "s"),
              dur="indefinite",
              restart="always")
xml_remove(yearLine)
xml_add_child(xml_find_first(svg, "svg:g", ns), yearLine)
rateStartGroup <-
    xml_find_first(svg, '//svg:g[@id = "core-fg.4-2-4-2.1"]/svg:g[2]/svg:g', ns)
rateEndGroup <- xml_find_first(svg, '//svg:g[@id = "rate.end.1.1"]', ns)
rateLine <- xml_find_first(svg, '//svg:polyline[@id = "rate.line.1.1"]', ns)
xml_set_attr(rateStartGroup, "style",
             "filter: drop-shadow(3px -3px 2px rgba(0,0,0,.6)")
xml_set_attr(rateLine, "style",
             "filter: drop-shadow(3px -3px 2px rgba(0,0,0,.8)")
rateStart <- as.numeric(strsplit(gsub("[^0-9. ]", "",
                                      xml_attr(rateStartGroup, "transform")),
                                 " ")[[1]])
rateEnd <- as.numeric(strsplit(gsub("[^0-9. ]", "",
                                    xml_attr(rateEndGroup, "transform")),
                               " ")[[1]])
rateDiff <- rateEnd - rateStart
xml_add_child(rateStartGroup,
              "animateMotion",
              path=paste0("M0,0",
                          " C0,", rateDiff[2]/2,
                          " ", rateDiff[1]/2, ",", rateDiff[2],
                          " ", rateDiff[1], ",", rateDiff[2]),
              begin=paste0(timings$moveRate$start, "s"),
              dur=paste0(timings$moveRate$durn, "s"),
              fill="freeze",
              restart="always")
xml_add_child(rateLine,
              "set",
              attributeName="stroke-opacity",
              to="1",
              begin=paste0(timings$showRate$start, "s"),
              dur="indefinite",
              restart="always")              
xml_remove(rateLine)
xml_add_child(xml_find_first(svg, "svg:g", ns), rateLine)
ageStartGroup <-
    xml_find_first(svg, '//svg:g[@id = "core-fg.4-3-4-3.1"]/svg:g[2]/svg:g', ns)
ageEndGroup <- xml_find_first(svg, '//svg:g[@id = "age.end.1.1"]', ns)
ageLine <-
    xml_find_first(svg,
                   '//svg:g[@id = "key-5-1-1.7-2-7-2.1"]/svg:g/svg:polyline',
                   ns)
xml_set_attr(ageStartGroup, "style",
             "filter: drop-shadow(3px -3px 2px rgba(0,0,0,.6)")
ageStart <- as.numeric(strsplit(gsub("[^0-9. ]", "",
                                      xml_attr(ageStartGroup, "transform")),
                                 " ")[[1]])
ageEnd <- as.numeric(strsplit(gsub("[^0-9. ]", "",
                                    xml_attr(ageEndGroup, "transform")),
                               " ")[[1]])
ageDiff <- ageEnd - ageStart
xml_add_child(ageStartGroup,
              "animateMotion",
              path=paste0("M0,0",
                          " C", ageDiff[1]/2, ",0", 
                          " ", ageDiff[1]/2, ",0",
                          " ", ageDiff[1], ",", ageDiff[2]),
              begin=paste0(timings$moveAge$start, "s"),
              dur=paste0(timings$moveAge$durn, "s"),
              fill="freeze",
              restart="always")
xml_add_child(ageLine,
              "set",
              attributeName="stroke",
              to=pal_npg()(3)[3],
              begin=paste0(timings$showAge$start, "s"),
              dur="indefinite",
              restart="always")              
xml_add_child(ageLine,
              "set",
              attributeName="stroke-width",
              to="3",
              begin=paste0(timings$showAge$start, "s"),
              dur="indefinite",
              restart="always")
xml_add_child(ageLine,
              "set",
              attributeName="filter",
              to="drop-shadow(3px -3px 2px rgba(0,0,0,.8)",
              begin=paste0(timings$showAge$start, "s"),
              dur="indefinite",
              restart="always")
xml_remove(ageLine)
xml_add_child(xml_find_first(svg, "svg:g", ns), ageLine)
## Remove clipping regions (otherwise translations hide elements)              
clipRegions <- xml_find_all(svg, '//svg:g[@clip-path]', ns)
xml_set_attr(clipRegions, "clip-path", "none")

write_xml(svg, "mappings-1.svg")
