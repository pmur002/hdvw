
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
                   y=unit(0, "npc") - unit(1, "lines"),
                   gp=gpar(col=NA),
                   name="year.end")
    sg <- segmentsGrob(x, 0, x, 1,
                       gp=gpar(col=adjustcolor(highlight, alpha=0),
                               lty="dashed", lwd=2),
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
    sg <- segmentsGrob(0, y, 1, y,
                       gp=gpar(col=adjustcolor(highlight, alpha=0),
                               lty="dashed", lwd=2),
                       name="rate.line")
    grobTree(tg, sg)
}

gg <- ggplot(crime) +
    geom_line(aes(year, rate, colour=age), linewidth=.5) +
    scale_colour_manual(values=rep("grey", 3)) +
    theme(panel.border=element_rect(colour="grey"),
          panel.grid.major.y=element_line(colour="grey"),
          plot.title=element_text(colour="grey"),
          plot.subtitle=element_text(colour="grey"),
          axis.ticks=element_line(colour="grey"),
          axis.text=element_text(colour="grey"),
          axis.title=element_text(colour="grey"),
          legend.text=element_text(colour="grey"),
          legend.title=element_text(colour="grey"),
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
popViewport()
pushViewport(viewport(x=0, width=1/3, just="left"))
grid.draw(gt)
popViewport()

grid.force()
grid.add("key-5-1-1.7-2-7-2", grep=TRUE,
         textGrob("", unit(-.5, "lines"), name="age.end"))

dev.off()

library(animaker)

moveYear <- atomic(dur=2, label="moveYear")
showYear <- atomic(dur=0, label="showYear")
year <- vec(moveYear, showYear, label="year")
moveRate <- atomic(dur=2, label="moveRate")
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
    xml_find_first(svg, '//svg:g[@id = "core-fg.4-1-4-1.1"]/svg:g/svg:g', ns)
yearEndGroup <- xml_find_first(svg, '//svg:g[@id = "year.end.1.1"]', ns)
yearLine <- xml_find_first(svg, '//svg:polyline[@id = "year.line.1.1"]', ns)
xml_set_attr(yearStartGroup, "style",
             "filter: drop-shadow(3px -3px 2px rgba(0,0,0,.4)")
xml_set_attr(yearLine, "style",
             "filter: drop-shadow(3px -3px 2px rgba(0,0,0,.4)")
xml_add_child(yearStartGroup,
              "animateTransform",
              attributeName="transform",
              attributeType="XML",
              type="translate",
              from=gsub("[^0-9. ]", "", xml_attr(yearStartGroup, "transform")),
              to=gsub("[^0-9. ]", "", xml_attr(yearEndGroup, "transform")),
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
rateStartGroup <-
    xml_find_first(svg, '//svg:g[@id = "core-fg.4-2-4-2.1"]/svg:g/svg:g', ns)
rateEndGroup <- xml_find_first(svg, '//svg:g[@id = "rate.end.1.1"]', ns)
rateLine <- xml_find_first(svg, '//svg:polyline[@id = "rate.line.1.1"]', ns)
xml_set_attr(rateStartGroup, "style",
             "filter: drop-shadow(3px -3px 2px rgba(0,0,0,.4)")
xml_set_attr(rateLine, "style",
             "filter: drop-shadow(3px -3px 2px rgba(0,0,0,.4)")
xml_add_child(rateStartGroup,
              "animateTransform",
              attributeName="transform",
              attributeType="XML",
              type="translate",
              from=gsub("[^0-9. ]", "", xml_attr(rateStartGroup, "transform")),
              to=gsub("[^0-9. ]", "", xml_attr(rateEndGroup, "transform")),
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
ageStartGroup <-
    xml_find_first(svg, '//svg:g[@id = "core-fg.4-3-4-3.1"]/svg:g/svg:g', ns)
ageEndGroup <- xml_find_first(svg, '//svg:g[@id = "age.end.1.1"]', ns)
ageLine <-
    xml_find_first(svg,
                   '//svg:g[@id = "key-5-1-1.7-2-7-2.1"]/svg:g/svg:polyline',
                   ns)
xml_set_attr(ageStartGroup, "style",
             "filter: drop-shadow(3px -3px 2px rgba(0,0,0,.4)")
xml_add_child(ageStartGroup,
              "animateTransform",
              attributeName="transform",
              attributeType="XML",
              type="translate",
              from=gsub("[^0-9. ]", "", xml_attr(ageStartGroup, "transform")),
              to=gsub("[^0-9. ]", "", xml_attr(ageEndGroup, "transform")),
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
              to="drop-shadow(3px -3px 2px rgba(0,0,0,.4)",
              begin=paste0(timings$showAge$start, "s"),
              dur="indefinite",
              restart="always")
## Remove clipping regions (otherwise translations hide elements)              
clipRegions <- xml_find_all(svg, '//svg:g[@clip-path]', ns)
xml_set_attr(clipRegions, "clip-path", "none")

write_xml(svg, "mappings-1.svg")
