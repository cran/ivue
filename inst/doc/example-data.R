## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 6, fig.height = 3)
library(ivue)
have.rgl <- nzchar(system.file(package = "rgl"))

## ----point-data---------------------------------------------------------------
n <- 24L
t <- seq(0, 2 * pi, length.out = n)
X <- cbind(x = cos(t), y = sin(t), z = seq(-1, 1, length.out = n))
ids <- sprintf("point-%02d", seq_len(n))
rownames(X) <- ids
annotations <- data.frame(
  id = ids, height = X[, "z"],
  half = factor(ifelse(X[, "z"] < 0, "Lower", "Upper"),
                levels = c("Lower", "Upper"))
)
# Simulate an annotation table arriving in another order, then align it.
annotations <- annotations[rev(seq_len(n)), ]
stopifnot(!anyNA(annotations$id), !anyDuplicated(annotations$id),
          setequal(annotations$id, rownames(X)))
annotations <- annotations[match(rownames(X), annotations$id), ]
stopifnot(identical(annotations$id, rownames(X)),
          identical(dim(X), c(n, 3L)), all(is.finite(X)))
head(annotations, 3)

## ----named-annotation---------------------------------------------------------
named.height <- setNames(annotations$height, annotations$id)
named.height <- named.height[rev(seq_along(named.height))]

## ----shared-settings----------------------------------------------------------
original <- annotations$height
reduced <- original / 2
scale <- color.scale.cont(c(original, reduced), limits = c(-1, 1), center = 0,
                          palette = c("#2166AC", "#F7F7F7", "#B2182B"))
camera <- camera.zup(elevation = 20, turn = -135, fov = 0, zoom = 0.65)
original.mapping <- map.colors(original, scale)
reduced.mapping <- map.colors(reduced, scale)
stopifnot(identical(original.mapping$legend, reduced.mapping$legend))
original.mapping$legend

## ----shared-views, eval=have.rgl----------------------------------------------
original.view <- plot3D.cont(X, named.height, scale = scale, camera = camera,
  aspect = "equal", point.size = 7, height = 360,
  legend.title = "Original height", legend.width = 120)
reduced.view <- plot3D.cont(X, reduced, scale = scale, camera = camera,
  aspect = "equal", point.size = 7, height = 360,
  legend.title = "Half height", legend.width = 120)
stopifnot(identical(attr(original.view, "ivue")$mapping$colors,
                    original.mapping$colors),
          identical(attr(original.view, "ivue")$observation.ids, ids))
original.view
reduced.view

## ----shared-summary-----------------------------------------------------------
rows <- c(1L, 12L, 24L)
data.frame(id = ids[rows], original = original[rows], reduced = reduced[rows],
           original.color = original.mapping$colors[rows],
           reduced.color = reduced.mapping$colors[rows])

## ----group-recipe-------------------------------------------------------------
group.scale <- color.scale.groups(annotations$half,
                                  colors = c(Lower = "#2166AC", Upper = "#B2182B"))
map.colors(annotations$half, group.scale)$legend

## ----group-view, eval=have.rgl------------------------------------------------
group.view <- plot3D.groups(X, annotations$half, scale = group.scale,
                            camera = camera, height = 300,
                            legend.width = 120, legend.title = "Helix half")
stopifnot(inherits(group.view, "htmlwidget"))

## ----graph-data---------------------------------------------------------------
vertices <- c("start", "bend", "end", "isolate")
edges <- data.frame(from = c("start", "bend"), to = c("bend", "end"),
                    weight = c(2, 4))
coords <- rbind(start = c(0, 0, 0), bend = c(1, 1, 0),
                end = c(2, 0, 1), isolate = c(0, 2, 1))
graph <- prepare.graph(edges, vertices = vertices, weight.type = "distance")
stopifnot(identical(graph$vertices$id, vertices),
          nrow(graph$edges) == 2L, all(is.finite(coords)),
          identical(rownames(coords), graph$vertices$id))
graph$edges

## ----graph-view, eval=have.rgl------------------------------------------------
coords <- coords[c("end", "start", "isolate", "bend"), , drop = FALSE]
values <- c(isolate = 40, bend = 20, start = 10, end = 30)
graph.view <- plot3D.graph(graph, X = coords, values = values,
  edge.width = c(1, 3), edge.col = "gray55",
  point.size = 8, legend.width = 100, height = 340,
  camera = camera.zup(zoom = 0.55),
  layers = list(layer3D.path(c(1, 3), col = "#D55E00", width = 2),
                layer3D.labels(1:4, vertices, offset = c(0, 0, 0.15))))
info <- attr(graph.view, "ivue")
stopifnot(identical(rownames(info$X), vertices),
          identical(info$mapping$colors,
                    map.colors(unname(values[vertices]), info$mapping$scale)$colors))
graph.view

## ----frame-data---------------------------------------------------------------
triangle <- rbind(a = c(0, 0), b = c(1, 0), c = c(0.5, 0.9))
first <- triangle
first[3, ] <- NA_real_
frames <- list(first, triangle, triangle * 1.4)
frame.labels <- c("Two vertices", "Complete triangle", "Expanded by 1.4")
frame.edges <- rbind(c(1, 2), c(2, 3), c(3, 1))
stopifnot(length(frames) == length(frame.labels),
  all(vapply(frames, function(f) identical(dim(f), c(3L, 2L)) &&
    identical(rownames(f), rownames(triangle)) &&
    all(rowSums(is.finite(f)) == 2L | rowSums(is.na(f)) == 2L), logical(1))),
  all(frame.edges >= 1L & frame.edges <= nrow(triangle)))

## ----frame-view, eval=have.rgl------------------------------------------------
player <- animate.frames(frames, edges = frame.edges, labels = frame.labels,
  fps = 1, loop = FALSE, max.frames = NULL,
  col = c("#2166AC", "#B2182B", "#009F87"), point.size = 9, height = 300)
player

## ----export-recipe, eval=have.rgl---------------------------------------------
local({
  directory <- tempfile("ivue-recipe-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  htmlwidgets::saveWidget(player, file.path(directory, "triangle.html"),
                          selfcontained = FALSE)
  stopifnot(file.exists(file.path(directory, "triangle.html")))
  if (requireNamespace("magick", quietly = TRUE)) {
    gif <- write.animation.gif(player, file.path(directory, "triangle.gif"),
                               width = 240, height = 240, final.hold = 0)
    stopifnot(file.exists(gif))
  }
})

## ----retinal-structure--------------------------------------------------------
retina <- readRDS(system.file("extdata", "retinal-development.rds", package = "ivue"))
ids <- retina$annotations$id
stopifnot(identical(names(retina$coordinates), c("umap", "sknn")),
          identical(dim(retina$annotations), c(12000L, 3L)),
          !anyNA(ids), !anyDuplicated(ids),
          identical(retina$graph$vertices, ids),
          nrow(retina$graph$edges) == 4108L,
          all(retina$graph$edges$from %in% ids),
          all(retina$graph$edges$to %in% ids))
for (coords in retina$coordinates) {
  stopifnot(identical(dim(coords), c(12000L, 3L)),
            identical(rownames(coords), ids), all(is.finite(coords)))
}
data.frame(representation = names(retina$coordinates),
           observations = 12000L, dimensions = 3L)
c(age.levels = nlevels(retina$annotations$age),
  cell.type.levels = nlevels(retina$annotations$cell.type),
  displayed.edges = nrow(retina$graph$edges))

## ----shared-bounds, eval=have.rgl---------------------------------------------
reference <- rbind(a = c(-1, -1, 0), b = c(1, -1, 0), c = c(0, 1, 1))
common.bounds <- rbind(x = c(-2, 2), y = c(-2, 2), z = c(-2, 2))
orientation <- camera.zup(elevation = 35, turn = 0)
full <- plot3D.plain(reference, limits = common.bounds, camera = orientation,
                     width = 300, height = 300, point.size = 8,
                     description = "Reference triangle at its original scale.")
contracted <- plot3D.plain(reference / 2, limits = common.bounds,
                     camera = orientation, width = 300, height = 300, point.size = 8,
                     description = "The same triangle contracted by half.")
htmltools::tagList(full, contracted)

