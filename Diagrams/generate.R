
graphDefault <- "  graph [ rankdir=LR; margin=.2; nodesep=.5 ];"
nodeDefault <- '  node [ fontsize=20; fontname="sans bold"; margin=.2 ];'

dataNode <- function(name, label) {
    paste0("  ", name,
           ' [ label="', label, '"; shape=box; style=filled; fillcolor=gray ]')
}

visualNode <- function(name, label) {
    paste0("  ", name,
           ' [ label="', label, '"; shape=box;\n',
           "  ", paste(rep(" ", nchar(name)), collapse=""), 
           '   style=filled; fillcolor="#444444"; fontcolor=white ]')
}

ggplotNode <- function(name, label=name) {
    paste0("  ", name,
           ' [ label="', label, '"; shape=ellipse; ]')
}

modelNode <- visualNode

textNode <- function(name, label=name) {
    paste0("  ", name,
           ' [ label="', label, '"; shape=none; ]')
}

## data to visual
mapEdge <- function(from, to) {
    paste0("  ", from, ' -> ', to)
}

## computational processing
compEdge <- function(from, to) {
    paste0("  ", from, ' -> ', to)
}

## visual processing
procEdge <- function(from, to) {
    paste0("  ", from, ' -> ', to)
}

## visual to data
backEdge <- function(from, to) {
    paste0("  ", from, ' -> ', to, ' [ style="dashed" ]')
}

## Edges between model nodes
modelEdge <- function(from, to) {
    paste0("  ", from, ' -> ', to)
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
stat <- dataNode("stat", "data\\nsummaries")
meta <- dataNode("meta", "metadata\\n")
dataStatSame <- sameRank("data", "stat")

## Visual nodes
sym <- visualNode("sym", "data\\nsymbols")
vis <- visualNode("vis", "visual\\nfeatures")
add <- visualNode("add", "additional\\nfeatures")
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
ggstat <- ggplotNode("ggstat", "stats")

## Model nodes
eye <- textNode("eye")
basic <- modelNode("basic", "visual\\nfeatures")
shapes <- modelNode("shapes", "visual\\nshapes")
objects <- modelNode("objects", "visual\\nobjects")

## Edges from data
dataSymEdge <- mapEdge("data", "sym")
dataVisEdge <- mapEdge("data", "vis")
dataStatEdge <- compEdge("data", "stat")
statSymEdge <- mapEdge("stat", "sym")
statVisEdge <- mapEdge("stat", "vis")
dataObjEdge <- mapEdge("data", "obj")
statLabelEdge <- mapEdge("stat", "label")
metaLabelEdge <- mapEdge("meta", "label")

## Edges from visual
symDataEdge <- backEdge("sym:s", "data:se")
symStatEdge <- backEdge("sym:s", "stat:se")
visDataEdge <- backEdge("vis:s", "data:se")
visDataEdge2 <- backEdge("vis:n", "data:ne")
visDataEdge3 <- backEdge("vis:sw", "data:se")
visStatEdge <- backEdge("vis:s", "stat:se")
visSumEdge <- procEdge("vis", "sum")
visVisEdge <- procEdge("vis", "add")
addDataEdge <- backEdge("add:sw", "data:s")
addStatEdge <- backEdge("add:s", "stat:se")
visShapeEdge <- procEdge("vis", "shape")
shapeObjEdge <- procEdge("shape", "obj")
visObjEdge <- procEdge("vis", "obj")
sumStatEdge <- backEdge("sum:s", "stat:se")
shapeStatEdge <- backEdge("shape:s", "stat:se")
objDataEdge <- backEdge("obj:s", "data:se")
objStatEdge <- backEdge("obj:s", "stat:se")
labelMetaEdge <- backEdge("label:s", "meta:se")
labelStatEdge <- backEdge("label:s", "stat:se")

## Edges from ggplot
dataAesEdge <- mapEdge("data", "aes")
dataGGstatEdge <- mapEdge("data", "ggstat")
ggstatStatEdge <- mapEdge("ggstat", "stat")
dataScaleEdge <- mapEdge("data", "scale")
statScaleEdge <- mapEdge("stat", "scale")
scaleAesEdge <- mapEdge("scale", "aes")
aesGeomEdge <- mapEdge("aes", "geom")
geomSymEdge <- mapEdge("geom", "sym")
geomVisEdge <- mapEdge("geom", "vis")

## Edges for models
eyeBasicEdge <- modelEdge("eye", "basic")
basicShapeEdge <- modelEdge("basic", "shapes")
shapeObjectEdge <- modelEdge("shapes", "objects")

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

graph(eye,
      basic,
      shapes,
      objects,
      eyeBasicEdge,
      basicShapeEdge,
      shapeObjectEdge,
      file="visual-processing.dot")
