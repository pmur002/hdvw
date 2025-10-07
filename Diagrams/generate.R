
source("../colours.R")

n <- 10
grad <- function(i1, i2, level1=1, level2=1) {
    if (is.character(i1)) {
        col1 <- i1
    } else {
        col1 <- switch(level1, cols[i1], colsDarker[i1], colsDarkest[i1])
    }
    col2 <- switch(level2, cols[i2], colsDarker[i2], colsDarkest[i2])
    paste0(apply(colorRamp(c(col1,
                             col2))(seq(0, 1, length=n)),
                 1,
                 function(x) {
                     do.call(rgb, as.list(x/255))
                 }),
           ";", round(1/n, 2), collapse=":")
}

grad1to2 <- grad(1, 2)
grad2to1 <- grad(2, 1)
grad1to3 <- grad(1, 3)
grad3to2 <- grad(3, 2)
grad2to3 <- grad(2, 3)
grad0to2 <- grad("black", 2)
grad2to5 <- grad(2, 5)
    
graphDefault <- "  graph [ rankdir=LR; margin=.2; nodesep=.5 ];"
nodeDefault <- '  node [ fontsize=20; fontname="sans bold"; margin=.2 ];'

dataNode <- function(name, label, level=1) {
    col <- switch(level, cols[1], colsDarker[1], colsDarkest[1])
    paste0("  ", name,
           ' [ label="', label, '"; ',
           'shape=box; style="filled, rounded"; penwidth=0; fontcolor=white; ',
           'fillcolor="', col, '" ]')
}

visualNode <- function(name, label, level=1) {
    col <- switch(level, cols[2], colsDarker[2], colsDarkest[2])
    paste0("  ", name,
           ' [ label="', label, '"; shape=box;\n',
           "  ", paste(rep(" ", nchar(name)), collapse=""), 
           'style="filled, rounded"; penwidth=0; fontcolor=white; ',
           'fillcolor="', col, '" ]')
}

lieNode <- function(name, label) {
    paste0("  ", name,
           ' [ label="', label, '"; ',
           'shape=box; style="filled, rounded"; penwidth=0; fontcolor=white; ',
           'fillcolor="', cols[5], '" ]')
}

dissNode <- function(name, label) {
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
mapEdge <- function(from, to, grad=grad1to2) {
    paste0("  ", from, ' -> ', to,
           ' [ color="', grad, '" ]')
}

## computational processing
compEdge <- function(from, to, level1=1, level2=1) {
    if (level1 == 1 && level2 == 1) {
        col <- cols[1]
    } else {
        col <- grad(1, 1, level1, level2)
    }
    paste0("  ", from, ' -> ', to,
           ' [ color="', col, '" ]')
}

lieEdge <- function(from, to) {
    paste0("  ", from, ' -> ', to,
           ' [ color="', grad1to3, '" ]')
}

## visual processing
procEdge <- function(from, to, level1=1, level2=1) {
    if (level1 == 1 && level2 == 1) {
        col <- cols[2]
    } else {
        col <- grad(2, 2, level1, level2)
    }
    paste0("  ", from, ' -> ', to,
           ' [ color="', col, '" ]')
}

## visual to data
backEdge <- function(from, to, grad=grad2to1) {
    paste0("  ", from, ' -> ', to,
           ' [ style="dashed", color="', grad, '" ]')
}

implicitEdge <- function(from, to, grad=grad2to1) {
    paste0("  ", from, ' -> ', to,
           ' [ style="solid", color="', grad, '" ]')
}

## Edges between model nodes
modelEdge <- function(from, to) {
    paste0("  ", from, ' -> ', to,
           ' [ color="', grad0to2, '" ]')
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
stat <- dataNode("stat", "data\\nsummaries", level=2)
stat2 <- dataNode("stat2", "summary\\nsummaries", level=3)
lie <- lieNode("lie", "garbage\\nand lies")
diss <- dissNode("diss", "data\\nvalues")
meta <- dataNode("meta", "metadata /\\nbackground")
org <- dataNode("org", "structure /\\norganisation")
imp <- dataNode("imp", "importance /\\nsignificance")
dataData2Same <- sameRank("data", "data2")
dataStatSame <- sameRank("data", "stat")
dataStat2Same <- sameRank("data", "stat2")
dataLieSame <- sameRank("data", "lie")
dataDissSame <- sameRank("data", "diss")

## Visual nodes
sym <- visualNode("sym", "data\\nsymbols")
vis <- visualNode("vis", "visual\\nfeatures")
add <- visualNode("add", "emergent\\nfeatures", level=2)
sum <- visualNode("sum", "visual\\nsummaries", level=2)
shape <- visualNode("shape", "visual\\nshapes", level=2)
obj <- visualNode("obj", "visual\\nobjects", level=3)
label <- visualNode("label", "text\\nlabel", level=3)
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
shapes <- modelNode("shapes", "visual\\nshapes", level=2)
objects <- modelNode("objects", "visual\\nobjects", level=3)

## Edges from data
dataSymEdge <- mapEdge("data", "sym")
dataVisEdge <- mapEdge("data", "vis")
dataVisEdge2 <- mapEdge("data:e", "vis:w")
data2VisEdge <- mapEdge("data2", "vis")
data2VisEdge2 <- mapEdge("data2:e", "vis:w")
dataStatEdge <- compEdge("data", "stat", level1=1, level2=2)
dataLieEdge <- lieEdge("data", "lie")
lieVisEdge <- mapEdge("lie", "vis", grad=grad3to2)
statSymEdge <- mapEdge("stat", "sym", grad=grad(1, 2, level1=2, level2=1))
statVisEdge <- mapEdge("stat", "vis", grad=grad(1, 2, level1=2, level2=1))
dataObjEdge <- mapEdge("data", "obj", grad=grad(1, 2, level1=1, level2=3))
statLabelEdge <- mapEdge("stat", "label", grad=grad(1, 2, level1=2, level2=3))
metaLabelEdge <- mapEdge("meta", "label", grad=grad(1, 2, level1=1, level2=3))
orgVisEdge <- mapEdge("org", "vis")
impVisEdge <- mapEdge("imp", "vis")

## Edges from visual
symDataEdge <- backEdge("sym:s", "data:se")
symStatEdge <- backEdge("sym:s", "stat:se", grad=grad(2, 1, level1=1, level2=2))
visDataEdge <- backEdge("vis:s", "data:se")
visDataEdge2 <- implicitEdge("vis:n", "data:ne")
visData2Edge <- backEdge("vis:s", "data2:se")
visData2Edge2 <- implicitEdge("vis:n", "data2:ne")
visDataEdge3 <- backEdge("vis:sw", "data:se")
visDataEdge4 <- backEdge("vis:n", "data:ne")
visStatEdge <- backEdge("vis:s", "stat:se", grad=grad(2, 1, level1=1, level2=2))
visLieEdge <- backEdge("vis:s", "lie:se", grad=grad2to5)
visDissEdge <- implicitEdge("vis:n", "diss:ne", grad=grad2to3)
visSumEdge <- procEdge("vis", "sum")
visVisEdge <- procEdge("vis", "add", level1=1, level2=2)
addDataEdge <- backEdge("add:sw", "data:s", grad=grad(2, 1, level1=2, level2=1))
addStatEdge <- backEdge("add:s", "stat:se", grad=grad(2, 1, level1=2, level2=2))
addLieEdge <- backEdge("add:s", "lie:se", grad=grad(2, 3, level1=2, level2=1))
visShapeEdge <- procEdge("vis", "shape", level1=1, level2=2)
shapeObjEdge <- procEdge("shape", "obj", level1=2, level2=3)
visObjEdge <- procEdge("vis", "obj", level1=1, level2=3)
sumStatEdge <- backEdge("sum:s", "stat:se")
shapeStatEdge <- backEdge("shape:s", "stat:se",
                          grad=grad(2, 1, level1=2, level2=2))
shapeLieEdge <- backEdge("shape:s", "lie:se",
                         grad=grad(2, 3, level1=2, level2=1))
shapeStat2Edge <- backEdge("shape:s", "stat2:se",
                           grad=grad(2, 1, level1=2, level2=3))
objDataEdge <- backEdge("obj:s", "data:se", grad=grad(2, 1, level1=3, level2=1))
objStatEdge <- backEdge("obj:s", "stat:se", grad=grad(2, 1, level1=3, level2=2))
labelMetaEdge <- backEdge("label:s", "meta:se",
                          grad=grad(2, 1, level1=3, level2=1))
labelStatEdge <- backEdge("label:s", "stat:se",
                          grad=grad(2, 1, level1=3, level2=2))
visOrgEdge <- backEdge("vis:s", "org:se")
visImpEdge <- backEdge("vis:s", "imp:se")

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
basicShapeEdge <- procEdge("basic", "shapes", 1, 2)
shapeObjectEdge <- procEdge("shapes", "objects", 2, 3)

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

graph(diss,
      data,
      vis,
      dataVisEdge,
      visDissEdge,
      visDataEdge,
      dataDissSame,
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
      visDataEdge4,
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

graph(data,
      data2,
      vis,
      invis(data2VisEdge),
      dataVisEdge2,
      data2VisEdge2,
      visDataEdge,
      visData2Edge,
      dataData2Same,
      file="conflict-decode.dot")

graph(org,
      vis,
      orgVisEdge,
      visOrgEdge,
      file="org-vis-decode.dot")

graph(imp,
      vis,
      impVisEdge,
      visImpEdge,
      file="imp-vis-decode.dot")

