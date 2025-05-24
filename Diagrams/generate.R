
## Taken from top of how-data-vis-works.qmd
highlight <- "#7D12BA"
library(colorspace)
highrgb <- col2rgb(highlight)/255
highhcl <- coords(as(sRGB(highrgb[1], highrgb[2], highrgb[3]), "polarLUV"))
col1 <- 120
col2 <- 240
col3 <- 0
cols <- hcl(c(col1, col2, col3), highhcl[2], highhcl[1])
n <- 10
grad1 <- paste0(apply(colorRamp(c(cols[1],
                                  cols[2]))(seq(0, 1, length=n)),
                      1,
                      function(x) {
                          do.call(rgb, as.list(x/255))
                      }),
                ";", round(1/n, 2), collapse=":")
grad2 <- paste0(apply(colorRamp(c(cols[2],
                                  cols[1]))(seq(0, 1, length=n)),
                      1,
                      function(x) {
                          do.call(rgb, as.list(x/255))
                      }),
                ";", round(1/n, 2), collapse=":")
grad3 <- paste0(apply(colorRamp(c(cols[1],
                                  cols[3]))(seq(0, 1, length=n)),
                      1,
                      function(x) {
                          do.call(rgb, as.list(x/255))
                      }),
                ";", round(1/n, 2), collapse=":")
grad4 <- paste0(apply(colorRamp(c(cols[3],
                                  cols[2]))(seq(0, 1, length=n)),
                      1,
                      function(x) {
                          do.call(rgb, as.list(x/255))
                      }),
                ";", round(1/n, 2), collapse=":")
grad5 <- paste0(apply(colorRamp(c(cols[2],
                                  cols[3]))(seq(0, 1, length=n)),
                      1,
                      function(x) {
                          do.call(rgb, as.list(x/255))
                      }),
                ";", round(1/n, 2), collapse=":")
grad6 <- paste0(apply(colorRamp(c("black",
                                  cols[2]))(seq(0, 1, length=n)),
                      1,
                      function(x) {
                          do.call(rgb, as.list(x/255))
                      }),
                ";", round(1/n, 2), collapse=":")

graphDefault <- "  graph [ rankdir=LR; margin=.2; nodesep=.5 ];"
nodeDefault <- '  node [ fontsize=20; fontname="sans bold"; margin=.2 ];'

dataNode <- function(name, label) {
    paste0("  ", name,
           ' [ label="', label, '"; ',
           'shape=box; style="filled, rounded"; penwidth=0; fontcolor=white; ',
           'fillcolor="', cols[1], '" ]')
}

visualNode <- function(name, label) {
    paste0("  ", name,
           ' [ label="', label, '"; shape=box;\n',
           "  ", paste(rep(" ", nchar(name)), collapse=""), 
           'style="filled, rounded"; penwidth=0; fontcolor=white; ',
           'fillcolor="', cols[2], '" ]')
}

lieNode <- function(name, label) {
    paste0("  ", name,
           ' [ label="', label, '"; ',
           'shape=box; style="filled, rounded"; penwidth=0; fontcolor=white; ',
           'fillcolor="', cols[3], '" ]')
}

ggplotNode <- function(name, label=name, data=FALSE) {
    paste0("  ", name,
           ' [ label="', label, '"; shape=box; style=rounded; ',
           'color="', if (data) cols[1] else cols[2], '"; ',
           'fontcolor="', if (data) cols[1] else cols[2], '" ]')
}

modelNode <- visualNode

textNode <- function(name, label=name) {
    paste0("  ", name,
           ' [ label="', label, '"; shape=none; ]')
}

## data to visual
mapEdge <- function(from, to, grad=grad1) {
    paste0("  ", from, ' -> ', to,
           ' [ color="', grad, '" ]')
}

## computational processing
compEdge <- function(from, to) {
    paste0("  ", from, ' -> ', to,
           ' [ color="', cols[1], '" ]')
}

lieEdge <- function(from, to) {
    paste0("  ", from, ' -> ', to,
           ' [ color="', grad3, '" ]')
}

## visual processing
procEdge <- function(from, to) {
    paste0("  ", from, ' -> ', to,
           ' [ color="', cols[2], '" ]')
}

## visual to data
backEdge <- function(from, to, grad=grad2) {
    paste0("  ", from, ' -> ', to,
           ' [ style="dashed", color="', grad, '" ]')
}

implicitEdge <- function(from, to) {
    paste0("  ", from, ' -> ', to,
           ' [ style="solid", color="', grad2, '" ]')
}

## Edges between model nodes
modelEdge <- function(from, to) {
    paste0("  ", from, ' -> ', to,
           ' [ color="', grad6, '" ]')
}
    
invis <- function(edge) {
    paste0(edge, ' [ style="invis" ]')
}
    
sameRank <- function(...) {
    paste0("{ rank=same; ", paste(..., sep=", "), " }")
}

graph <- function(..., file) {
    g <- c("digraph {",
           graphDefault,
           nodeDefault,
           ...,
           "}")
    writeLines(g, file)
}

## Data nodes
data <- dataNode("data", "data\\nvalues")
data2 <- dataNode("data2", "data\\nvalues")
stat <- dataNode("stat", "data\\nsummaries")
stat2 <- dataNode("stat2", "summary\\nsummaries")
lie <- lieNode("lie", "garbage\\nand lies")
meta <- dataNode("meta", "metadata\\n")
dataData2Same <- sameRank("data", "data2")
dataStatSame <- sameRank("data", "stat")
dataStat2Same <- sameRank("data", "stat2")
dataLieSame <- sameRank("data", "lie")

## Visual nodes
sym <- visualNode("sym", "data\\nsymbols")
vis <- visualNode("vis", "visual\\nfeatures")
add <- visualNode("add", "emergent\\nfeatures")
sum <- visualNode("sum", "visual\\nsummaries")
shape <- visualNode("shape", "visual\\nshapes")
obj <- visualNode("obj", "visual\\nobjects")
label <- visualNode("label", "text\\nlabel")
visSumSame <- sameRank("vis", "sum")
visVisSame <- sameRank("vis", "add")
visShapeSame <- sameRank("vis", "shape")
visShapeObjSame <- sameRank("vis", "shape", "obj")
visObjSame <- sameRank("vis", "obj")

## ggplot node
aes <- ggplotNode("aes", "aesthetics")
geom <- ggplotNode("geom", "geoms")
scale <- ggplotNode("scale", "scales")
ggstat <- ggplotNode("ggstat", "stats", data=TRUE)

## Model nodes
eye <- textNode("eye")
basic <- modelNode("basic", "visual\\nfeatures")
shapes <- modelNode("shapes", "visual\\nshapes")
objects <- modelNode("objects", "visual\\nobjects")

## Edges from data
dataSymEdge <- mapEdge("data", "sym")
dataVisEdge <- mapEdge("data", "vis")
data2VisEdge <- mapEdge("data2", "vis")
dataStatEdge <- compEdge("data", "stat")
dataLieEdge <- lieEdge("data", "lie")
lieVisEdge <- mapEdge("lie", "vis", grad=grad4)
statSymEdge <- mapEdge("stat", "sym")
statVisEdge <- mapEdge("stat", "vis")
dataObjEdge <- mapEdge("data", "obj")
statLabelEdge <- mapEdge("stat", "label")
metaLabelEdge <- mapEdge("meta", "label")

## Edges from visual
symDataEdge <- backEdge("sym:s", "data:se")
symStatEdge <- backEdge("sym:s", "stat:se")
visDataEdge <- backEdge("vis:s", "data:se")
visDataEdge2 <- implicitEdge("vis:n", "data:ne")
visData2Edge <- backEdge("vis:s", "data2:se")
visData2Edge2 <- implicitEdge("vis:n", "data2:ne")
visDataEdge3 <- backEdge("vis:sw", "data:se")
visDataEdge4 <- backEdge("vis:s", "data:s")
visStatEdge <- backEdge("vis:s", "stat:se")
visLieEdge <- backEdge("vis:s", "lie:se", grad=grad5)
visSumEdge <- procEdge("vis", "sum")
visVisEdge <- procEdge("vis", "add")
addDataEdge <- backEdge("add:sw", "data:s")
addStatEdge <- backEdge("add:s", "stat:se")
addLieEdge <- backEdge("add:s", "lie:se", grad=grad5)
visShapeEdge <- procEdge("vis", "shape")
shapeObjEdge <- procEdge("shape", "obj")
visObjEdge <- procEdge("vis", "obj")
sumStatEdge <- backEdge("sum:s", "stat:se")
shapeStatEdge <- backEdge("shape:s", "stat:se")
shapeLieEdge <- backEdge("shape:s", "lie:se", grad=grad5)
shapeStat2Edge <- backEdge("shape:s", "stat2:se")
objDataEdge <- backEdge("obj:s", "data:se")
objStatEdge <- backEdge("obj:s", "stat:se")
labelMetaEdge <- backEdge("label:s", "meta:se")
labelStatEdge <- backEdge("label:s", "stat:se")

## Edges from ggplot
dataAesEdge <- mapEdge("data", "aes")
dataGGstatEdge <- compEdge("data", "ggstat")
ggstatStatEdge <- compEdge("ggstat", "stat")
dataScaleEdge <- mapEdge("data", "scale")
statScaleEdge <- mapEdge("stat", "scale")
scaleAesEdge <- procEdge("scale", "aes")
aesGeomEdge <- procEdge("aes", "geom")
geomSymEdge <- procEdge("geom", "sym")
geomVisEdge <- procEdge("geom", "vis")

## Edges for models
eyeBasicEdge <- modelEdge("eye", "basic")
basicShapeEdge <- procEdge("basic", "shapes")
shapeObjectEdge <- procEdge("shapes", "objects")

graph(data,
      sym,
      dataSymEdge,
      file="data-sym.dot")

graph(data,
      sym,
      dataSymEdge,
      symDataEdge,
      file="data-sym-decode.dot")

graph(data,
      vis,
      dataVisEdge,
      file="data-vis.dot")

graph(data,
      vis,
      dataVisEdge,
      visDataEdge,
      file="data-vis-decode.dot")

graph(data,
      vis,
      invis(dataVisEdge),
      visDataEdge2,
      file="implicit-decode.dot")

graph(data,
      vis,
      dataVisEdge,
      visDataEdge,
      visDataEdge2,
      file="congruent-decode.dot")

graph(data,
      data2,
      vis,
      data2VisEdge,
      visDataEdge2,
      visData2Edge,
      dataData2Same,
      file="dissonant-decode.dot")

graph(data,
      data2,
      vis,
      invis(data2VisEdge),
      visDataEdge2,
      visData2Edge2,
      dataData2Same,
      file="ambiguous-decode.dot")

graph(data,
      vis,
      dataVisEdge,
      visDataEdge,
      visDataEdge2,
      file="data-vis-redundant.dot")

graph(data,
      stat,
      sym,
      dataStatEdge,
      statSymEdge,
      symStatEdge,
      dataStatSame,
      file="stat-sym-decode.dot")

graph(data,
      stat,
      vis,
      dataStatEdge,
      statVisEdge,
      dataStatSame,
      file="stat-vis.dot")

graph(data,
      stat,
      vis,
      dataStatEdge,
      statVisEdge,
      visStatEdge,
      dataStatSame,
      file="stat-vis-decode.dot")

graph(data,
      stat,
      vis,
      dataVisEdge,
      visDataEdge3,
      visStatEdge,
      dataStatSame,
      file="data-vis-stat-decode.dot")

graph(data,
      lie,
      vis,
      dataLieEdge,
      lieVisEdge,
      visLieEdge,
      dataLieSame,
      file="lie-vis-decode.dot")

graph(data,
      vis,
      add,
      dataVisEdge,
      visVisEdge,
      visVisSame,
      file="data-vis-vis.dot")

graph(data,
      vis,
      add,
      dataVisEdge,
      visVisEdge,
      visDataEdge3,
      addDataEdge,
      visVisSame,
      file="data-vis-vis-decode.dot")

graph(data,
      stat,
      vis,
      add,
      dataVisEdge,
      visVisEdge,
      visDataEdge3,
      addStatEdge,
      dataStatSame,
      visVisSame,
      file="data-vis-vis-stat.dot")

graph(data,
      lie,
      vis,
      add,
      dataVisEdge,
      visVisEdge,
      addLieEdge,
      dataLieSame,
      visVisSame,
      file="data-vis-vis-lie.dot")

graph(data,
      stat,
      vis,
      add,
      dataVisEdge,
      visVisEdge,
      addStatEdge,
      dataStatSame,
      visVisSame,
      file="data-vis-vis-stat-only.dot")

graph(data,
      vis,
      shape,
      dataVisEdge,
      visShapeEdge,
      visShapeSame,
      file="data-vis-shape.dot")

graph(data,
      stat,
      vis,
      shape,
      dataVisEdge,
      visShapeEdge,
      shapeStatEdge,
      dataStatSame,
      visShapeSame,
      file="data-vis-shape-decode.dot")

graph(data,
      lie,
      vis,
      shape,
      dataVisEdge,
      visShapeEdge,
      shapeLieEdge,
      dataLieSame,
      visShapeSame,
      file="data-vis-shape-lie.dot")

graph(data,
      obj,
      dataObjEdge,
      objDataEdge,
      file="data-obj-decode.dot")

graph(data,
      stat,
      vis,
      shape,
      obj,
      dataVisEdge,
      visShapeEdge,
      shapeObjEdge,
      objStatEdge,
      dataStatSame,
      visShapeObjSame,
      file="data-vis-shape-obj.dot")

graph(data,
      stat,
      vis,
      obj,
      dataVisEdge,
      visObjEdge,
      objStatEdge,
      dataStatSame,
      visObjSame,
      file="data-vis-obj.dot")

graph(data,
      vis,
      invis(dataVisEdge),
      file="vis-clutter.dot")

graph(data,
      aes,
      geom,
      sym,
      dataAesEdge,
      aesGeomEdge,
      geomSymEdge,
      file="data-aes-geom-sym.dot")

graph(data,
      scale,
      aes,
      geom,
      sym,
      dataScaleEdge,
      scaleAesEdge,
      aesGeomEdge,
      geomSymEdge,
      file="data-scale-aes-geom-sym.dot")

graph(data,
      ggstat,
      stat,
      scale,
      aes,
      geom,
      sym,
      dataGGstatEdge,
      ggstatStatEdge,
      statScaleEdge,
      scaleAesEdge,
      aesGeomEdge,
      geomSymEdge,
      file="data-stat-scale-aes-geom-sym.dot")

graph(data,
      aes,
      geom,
      vis,
      dataAesEdge,
      aesGeomEdge,
      geomVisEdge,
      file="data-aes-geom-vis.dot")

graph(meta,
      label,
      metaLabelEdge,
      labelMetaEdge,
      file="meta-label.dot")

graph(data,
      stat,
      label,
      dataStatEdge,
      statLabelEdge,
      labelStatEdge,
      dataStatSame,
      file="data-stat-label.dot")

graph(data,
      stat,
      stat2,
      vis,
      shape,
      dataStatEdge,
      statVisEdge,
      visShapeEdge,
      shapeStat2Edge,
      dataStatSame,
      dataStat2Same,
      visShapeSame,
      file="data-stat-vis-shape-stat.dot")

graph(eye,
      basic,
      shapes,
      objects,
      eyeBasicEdge,
      basicShapeEdge,
      shapeObjectEdge,
      file="visual-processing.dot")

graph(data,
      vis,
      invis(dataVisEdge),
      visDataEdge2,
      file="learned-decode.dot")

