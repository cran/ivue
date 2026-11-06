## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7, fig.height = 5)
library(ivue)
have.rgl <- nzchar(system.file(package = "rgl"))
have.geometry <- requireNamespace("geometry", quietly = TRUE)

## ----saddle-data--------------------------------------------------------------
set.seed(1)
xs <- runif(250, -1, 1)
ys <- runif(250, -1, 1)
zs <- 1.2 * (xs^2 - ys^2)
X <- cbind(x = xs, y = ys, z = zs)

## ----saddle-plain, eval=have.rgl----------------------------------------------
plain <- plot3D.plain(X, col = "#197A68", point.size = 5,
                     axes = TRUE, xlab = "x", ylab = "y", zlab = "z",
                     description = "250 green observations on a saddle; opposite sides curve upward and downward.", controls = FALSE)
plain

## ----height-scale-------------------------------------------------------------
height.scale <- color.scale.cont(zs, center = 0,
  palette = c("#2166AC", "#F7F7F7", "#B2182B"))
mapping <- map.colors(zs, height.scale)
head(mapping$colors)
mapping$legend

## ----saddle-continuous, eval=have.rgl-----------------------------------------
colored <- plot3D.cont(X, values = zs, scale = height.scale,
                       point.size = 5, legend.title = "Saddle height", controls = FALSE)
colored

## ----binned-scale-------------------------------------------------------------
binned <- color.scale.cont(zs, mode = "binned",
  breaks = c(-1.2, -0.4, 0.4, 1.2),
  palette = c("#2166AC", "#EEEEEE", "#B2182B"))
map.colors(zs, binned)$legend

## ----area-uniform-saddle------------------------------------------------------
set.seed(1)
n <- 500
C <- 0.8
half.width <- 1
xy <- matrix(numeric(), ncol = 2)
while (nrow(xy) < n) {
  proposal <- matrix(runif(4000, -half.width, half.width), ncol = 2)
  area <- sqrt(1 + 4*C^2 * rowSums(proposal^2))
  accept <- runif(nrow(proposal)) < area / sqrt(1 + 8*C^2*half.width^2)
  xy <- rbind(xy, proposal[accept, , drop = FALSE])
}
xy <- xy[seq_len(n), , drop = FALSE]
saddle <- cbind(x = xy[, 1], y = xy[, 2],
                z = C * (xy[, 1]^2 - xy[, 2]^2))
byr.scale <- color.scale.cont(saddle[, "z"], center = 0,
                               palette = c("blue", "yellow", "red"))

## ----saddle-coordinate-axes, eval=have.rgl------------------------------------
saddle.view <- plot3D.cont(
  saddle, values = saddle[, "z"], scale = byr.scale,
  point.type = "sphere", sphere.radius = 0.02,
  legend.title = "Saddle height", legend.width = 160,
  axes = FALSE, aspect = "equal",
  layers = list(layer3D.axes(head.length = 0.04, head.angle = pi/8)),
  camera = camera.zup(elevation = 20, turn = -135, fov = 0, zoom = 0.4),
  height = 500L, controls = FALSE
)

## ----saddle-coordinate-display, eval=have.rgl, echo=FALSE---------------------
# Keep the enlarged scene in a 7:5 frame when the vignette column narrows.
# Move the legend above the frame on small screens rather than hiding data.
saddle.view$elementId <- "saddle-coordinate-example"
htmltools::tagList(
  htmltools::tags$style(htmltools::HTML("
    #saddle-coordinate-example, #saddle-mesh-example, #saddle-mesh-groups-example,
    #reference-surface-example {
      height: auto !important; aspect-ratio: 7 / 5;
    }
    @media (max-width: 600px) {
      #saddle-coordinate-example, #saddle-mesh-example,
      #reference-surface-example { margin-top: 160px; }
      #saddle-coordinate-example > .ivue-legend,
      #saddle-mesh-example > .ivue-legend,
      #reference-surface-example > .ivue-legend {
        top: -155px !important; max-height: none !important;
      }
      #saddle-mesh-groups-example { margin-top: 100px; }
      #saddle-mesh-groups-example > .ivue-legend {
        top: -95px !important; max-height: none !important;
      }
    }
  ")),
  saddle.view
)

## ----saddle-triangulation, eval=have.geometry---------------------------------
triangles <- geometry::delaunayn(saddle[, c("x", "y")])
surface <- layer3D.mesh(
  triangles, col = "gray75", alpha = 0.2,
  edge.col = "gray45", edge.alpha = 0.35, edge.width = 1
)

## ----saddle-mesh, eval=have.rgl && have.geometry------------------------------
saddle.mesh <- plot3D.cont(
  saddle, description = "500 saddle observations joined by triangular faces; blue is below zero, yellow is zero, red is above zero.", values = saddle[, "z"], scale = byr.scale,
  point.type = "sphere", sphere.radius = 0.02,
  legend.title = "Saddle height", legend.width = 160,
  axes = FALSE, aspect = "equal",
  layers = list(surface, layer3D.axes()),
  camera = camera.zup(elevation = 20, turn = -135, fov = 0, zoom = 0.4),
  height = 500L, controls = FALSE
)

## ----saddle-mesh-display, eval=have.rgl && have.geometry, echo=FALSE----------
saddle.mesh$elementId <- "saddle-mesh-example"
saddle.mesh

## ----mesh-other-embedding, eval=FALSE-----------------------------------------
# plot3D.cont(
#   Z, values = saddle[, "z"], scale = byr.scale,
#   point.type = "sphere", sphere.radius = 0.02, axes = FALSE,
#   layers = list(surface, layer3D.axes()), camera = camera.zup(), controls = FALSE
# )

## ----group-scale--------------------------------------------------------------
groups <- factor(ifelse(zs < 0, "Negative", "Nonnegative"),
                 levels = c("Negative", "Nonnegative"))
group.scale <- color.scale.groups(groups,
  colors = c(Negative = "#D95479", Nonnegative = "#009F87"))
map.colors(groups, group.scale)$legend

## ----saddle-groups, eval=have.rgl---------------------------------------------
plot3D.groups(X, groups, scale = group.scale, point.size = 5,
  highlight = abs(zs) > 0.4,
  highlight.style = list(point.size = 7),
  non.highlight.style = list(col = "gray75", alpha = 0.25), controls = FALSE)

## ----saddle-mesh-group-labels-------------------------------------------------
saddle.groups <- factor(ifelse(saddle[, "z"] < 0, "Negative", "Nonnegative"),
                        levels = c("Negative", "Nonnegative"))

## ----saddle-mesh-groups, eval=have.rgl && have.geometry-----------------------
saddle.mesh.groups <- plot3D.groups(
  saddle, groups = saddle.groups, scale = group.scale,
  point.type = "sphere", sphere.radius = 0.02,
  legend.title = "Height group", legend.width = 160,
  axes = FALSE, aspect = "equal",
  layers = list(surface, layer3D.axes()),
  camera = camera.zup(elevation = 20, turn = -135, fov = 0, zoom = 0.4),
  height = 500L, controls = FALSE
)

## ----saddle-mesh-groups-display, eval=have.rgl && have.geometry, echo=FALSE----
saddle.mesh.groups$elementId <- "saddle-mesh-groups-example"
saddle.mesh.groups

## ----reference-surface--------------------------------------------------------
grid.x <- grid.y <- seq(-1, 1, length.out = 31)
grid.z <- outer(grid.x, grid.y, function(x, y) 1.2 * (x^2 - y^2))
reference <- layer3D.surface(grid.x, grid.y, grid.z,
                              col = "lightblue", alpha = 0.3)

## ----reference-surface-player, eval=have.rgl----------------------------------
reference.view <- plot3D.cont(
  X, values = X[, "z"], scale = height.scale, point.size = 5,
  legend.title = "Saddle height", legend.width = 160, axes = FALSE,
  layers = list(reference, layer3D.axes()),
  camera = camera.zup(zoom = 0.6), height = 450, controls = FALSE
)

## ----reference-surface-display, eval=have.rgl, echo=FALSE---------------------
reference.view$elementId <- "reference-surface-example"
reference.view

## ----graph-data---------------------------------------------------------------
vertices <- data.frame(id = c("A", "B", "C", "D"),
                       label = c("Start", "Middle", "End", "Isolate"))
edges <- data.frame(from = c("A", "B"), to = c("B", "C"), weight = c(2, 4))
coords <- rbind(A = c(0, 0, 0), B = c(1, 1, 0),
                C = c(2, 0, 1), D = c(0, 2, 1))
graph <- prepare.graph(edges, vertices = vertices, weight.type = "distance")
graph$edges
coords <- coords[c("C", "A", "D", "B"), ]
values <- c(D = 40, B = 20, A = 10, C = 30)

## ----embedded-graph, eval=have.rgl--------------------------------------------
plot3D.graph(graph, X = coords, values = values, edge.col = "gray50",
  edge.width = 1 + graph$edges$weight,
  point.type = "sphere", sphere.radius = 0.07,
  layers = list(layer3D.path(c(1, 2, 3), col = "#D55E00", width = 4),
                layer3D.labels(1:4, vertices$label, offset = c(0, 0, 0.15))), controls = FALSE)

## ----adjacency-list-----------------------------------------------------------
adjacency <- list(
  adj.list = list(A = 2L, B = c(1L, 3L), C = 2L, D = integer()),
  weight.list = list(A = 2, B = c(2, 4), C = 4, D = numeric()))

## ----optional-layout, eval=have.rgl && requireNamespace("igraph", quietly=TRUE)----
layout.widget <- plot3D.graph(adjacency, layout = "kk", weight.type = "distance",
                              seed = 1, controls = FALSE)
stopifnot(inherits(layout.widget, "htmlwidget"))
unweighted <- igraph::make_ring(4)
unweighted.widget <- plot3D.graph(unweighted, layout = "fr", weight.type = "unweighted", controls = FALSE)
stopifnot(inherits(unweighted.widget, "htmlwidget"))

## ----comparison-coordinates---------------------------------------------------
comparison.pca <- prcomp(saddle, center = TRUE, scale. = FALSE)
comparison.exact <- comparison.pca$x
comparison.projected <- comparison.exact
comparison.projected[, 3] <- 0
original.dist <- as.numeric(dist(saddle))
exact.dist <- as.numeric(dist(comparison.exact))
projected.dist <- as.numeric(dist(comparison.projected))
stopifnot(all(original.dist > 0),
          max(abs(original.dist - exact.dist)) < 1e-10,
          all(projected.dist <= original.dist + 1e-10))
relative.error <- (projected.dist - original.dist) / original.dist
short.pairs <- original.dist <= quantile(original.dist, 0.1)
comparison.quality <- data.frame(
  View = c("All three PCA coordinates (exact)", "First two PCA coordinates (projection)"),
  RMS_relative_error_percent = c(100 * sqrt(mean(((exact.dist-original.dist)/original.dist)^2)),
                                  100 * sqrt(mean(relative.error^2))),
  Shortest_decile_RMS_percent = c(0, 100 * sqrt(mean(relative.error[short.pairs]^2))),
  Squared_length_retained_percent = c(100, 100 * sum(comparison.projected^2) / sum(comparison.exact^2)))
knitr::kable(comparison.quality, digits = 2, row.names = FALSE,
  col.names = c("View", "RMS error (%)", "Short pairs (%)", "Squared length retained (%)"))

## ----comparison-view-settings, eval=have.rgl----------------------------------
comparison.limits <- t(apply(rbind(comparison.exact, comparison.projected), 2, range))
comparison.span <- max(comparison.limits[, 2] - comparison.limits[, 1])
comparison.limits[, 1] <- comparison.limits[, 1] - 0.08 * comparison.span
comparison.limits[, 2] <- comparison.limits[, 2] + 0.08 * comparison.span
comparison.camera <- camera.zup(elevation = 25, turn = -125, zoom = 0.55)
comparison.view <- function(coordinates, description) {
  plot3D.cont(coordinates, values = saddle[, "z"], scale = byr.scale,
    point.type = "sphere", sphere.radius = 0.02,
    aspect = "equal", limits = comparison.limits, camera = comparison.camera,
    axes = FALSE, xlab = "", ylab = "", zlab = "",
    legend.title = "Original saddle height", legend.width = 180,
    controls = FALSE, height = 450L, description = description)
}
comparison.exact.widget <- comparison.view(comparison.exact,
  "Exact view: all three PCA coordinates preserve every original pair distance.")
comparison.projected.widget <- comparison.view(comparison.projected,
  "Approximate view: the third PCA coordinate is set to zero; colors still show original height.")

## ----comparison-exact-display, eval=have.rgl, echo=FALSE----------------------
comparison.exact.widget

## ----comparison-projected-display, eval=have.rgl, echo=FALSE------------------
comparison.projected.widget

## ----camera-comparison, eval=have.rgl-----------------------------------------
camera <- attr(colored, "ivue")$camera
comparison <- plot3D.plain(X, camera = camera, controls = FALSE)
stopifnot(identical(attr(comparison, "ivue")$row.ids, seq_len(nrow(X))))
comparison

## ----html-export, eval=have.rgl-----------------------------------------------
out <- tempfile(fileext = ".html")
htmlwidgets::saveWidget(colored, out, selfcontained = FALSE)
stopifnot(file.exists(out))
unlink(c(out, sub("\\.html$", "_files", out)), recursive = TRUE)

