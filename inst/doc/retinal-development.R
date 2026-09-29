## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", out.width = "100%")
library(ivue)

## ----load-data----------------------------------------------------------------
path <- system.file("extdata", "retinal-development.rds", package = "ivue")
if (!nzchar(path)) {
  candidates <- c(
    file.path("inst", "extdata", "retinal-development.rds"),
    file.path("..", "inst", "extdata", "retinal-development.rds")
  )
  path <- candidates[file.exists(candidates)][1]
}
retina <- readRDS(path)
dim(retina$coordinates$umap)
dim(retina$coordinates$sknn)
nrow(retina$graph$edges)

## ----identity-check-----------------------------------------------------------
ids <- retina$annotations$id
stopifnot(
  identical(rownames(retina$coordinates$umap), ids),
  identical(rownames(retina$coordinates$sknn), ids),
  identical(retina$graph$vertices, ids)
)
table(retina$annotations$age)

## ----scales-------------------------------------------------------------------
age.colors <- c(
  E11 = "#512A84", E12 = "#4148A4", E14 = "#2E68B4",
  E16 = "#1686B7", E18 = "#009FA8", P0 = "#28B58B",
  P2 = "#67C36B", P5 = "#A4C84F", P8 = "#D2B943", P14 = "#E2873C"
)
cell.colors <- c(
  "Early RPCs" = "#3B6C8E", "Late RPCs" = "#7A5195",
  "Neurogenic Cells" = "#D45087", "Retinal Ganglion Cells" = "#E45756",
  "Amacrine Cells" = "#F58518", "Horizontal Cells" = "#9C755F",
  "Photoreceptor Precursors" = "#54A24B", Cones = "#72B7B2",
  Rods = "#4C78A8", "Bipolar Cells" = "#B279A2",
  "Muller Glia" = "#8F9D44"
)
age.scale <- color.scale.groups(retina$annotations$age, colors = age.colors)
cell.scale <- color.scale.groups(
  retina$annotations$cell.type, colors = cell.colors
)
camera <- camera.zup(elevation = 18, turn = -28, fov = 0, zoom = 0.57)

## ----umap-code, eval=FALSE----------------------------------------------------
# umap.by.age <- plot3D.groups(
#   retina$coordinates$umap,
#   groups = retina$annotations$age,
#   scale = age.scale,
#   point.size = 2.2,
#   alpha = 0.78,
#   axes = FALSE,
#   aspect = "equal",
#   camera = camera
# )
# umap.by.age

## ----umap-poster, echo=FALSE--------------------------------------------------
knitr::include_graphics("figures/retinal-umap.png")

## ----sknn-points-code, eval=FALSE---------------------------------------------
# sknn.points.by.age <- plot3D.groups(
#   retina$coordinates$sknn,
#   groups = retina$annotations$age,
#   scale = age.scale,
#   point.size = 2.2,
#   alpha = 0.78,
#   axes = FALSE,
#   aspect = "equal",
#   camera = camera
# )
# sknn.points.by.age

## ----comparison-poster, echo=FALSE--------------------------------------------
knitr::include_graphics("figures/retinal-comparison.png")

## ----prepare-graph------------------------------------------------------------
graph <- prepare.graph(retina$graph)
nrow(graph$vertices)
nrow(graph$edges)
graph$weight.type

## ----graph-code, eval=FALSE---------------------------------------------------
# graph.by.age <- plot3D.graph(
#   graph,
#   X = retina$coordinates$sknn,
#   groups = retina$annotations$age,
#   scale = age.scale,
#   point.size = 2.1,
#   alpha = 0.84,
#   edge.col = "#59687324",
#   edge.width = 1,
#   axes = FALSE,
#   aspect = "equal",
#   camera = camera
# )
# graph.by.age

## ----graph-poster, echo=FALSE-------------------------------------------------
knitr::include_graphics("figures/retinal-sknn.png")

## ----cell-type-code, eval=FALSE-----------------------------------------------
# umap.by.cell.type <- plot3D.groups(
#   retina$coordinates$umap,
#   groups = retina$annotations$cell.type,
#   scale = cell.scale,
#   point.size = 2.2,
#   alpha = 0.78,
#   axes = FALSE,
#   aspect = "equal",
#   camera = camera
# )
# 
# graph.by.cell.type <- plot3D.graph(
#   graph,
#   X = retina$coordinates$sknn,
#   groups = retina$annotations$cell.type,
#   scale = cell.scale,
#   point.size = 2.1,
#   alpha = 0.84,
#   edge.col = "#59687324",
#   edge.width = 1,
#   axes = FALSE,
#   aspect = "equal",
#   camera = camera
# )

## ----regeneration, eval=FALSE-------------------------------------------------
# make retinal-vignette

## ----provenance---------------------------------------------------------------
retina$provenance[c("citation", "sample", "umap")]
retina$provenance$graph.input
retina$provenance$graph.selection$selected
retina$provenance$fitting.graph
retina$provenance$graph.layout$method

