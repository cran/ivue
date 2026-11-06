## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", out.width = "100%")
library(ivue)
have.rgl <- nzchar(system.file(package = "rgl"))
have.magick <- requireNamespace("magick", quietly = TRUE)
have.grip <- requireNamespace("grip", quietly = TRUE)

## ----installation, eval=FALSE-------------------------------------------------
# install.packages(c("rgl", "magick"))

## ----triangle-trace, eval=have.grip-------------------------------------------
edges <- grip::edges.sierpinski.triangle(level = 4)
tr <- grip::trace.grip(
  edges, n = max(edges), dim = 2, preset = "carpet", seed = 1,
  trace = "round", trace.every = 1
)
length(tr$frames)
dim(tr$frames[[1]])
utils::packageVersion("grip")

## ----triangle-frame-selection, eval=have.grip---------------------------------
frame.index <- unique(as.integer(round(seq(
  1, length(tr$frames), length.out = min(24L, length(tr$frames))
))))
triangle.frames <- tr$frames[frame.index]
triangle.meta <- tr$meta[frame.index, , drop = FALSE]
head(triangle.meta[, c("phase", "active_vertices")])
triangle.labels <- paste0("Frame ", frame.index,
                           " | ", triangle.meta$phase,
                           " | ", triangle.meta$active_vertices, " vertices")

## ----triangle-player, eval=have.grip && have.rgl------------------------------
triangle.player <- animate.frames(
  triangle.frames, edges = edges, labels = triangle.labels,
  fps = 5, col = "#C24E25", point.size = 4,
  edge.col = "#314E6ECC", edge.width = 1,
  background.color = "#FAF7F0", height = 450, controls = FALSE
)
triangle.player

## ----triangle-unavailable, echo=FALSE, results='asis'-------------------------
if (!have.grip) cat("The GRIP example code is shown but not evaluated in this build because `grip` is unavailable. The small triangle and saddle examples below do not require it.\n\n")

## ----small-example, eval=have.rgl---------------------------------------------
X <- rbind(a = c(0, 0), b = c(1, 0), c = c(0.5, 0.9))
first <- X
first[3, ] <- NA
small <- animate.frames(
  list(first, X, X * 1.4),
  edges = rbind(c(1, 2), c(2, 3), c(3, 1)),
  labels = c("Two vertices", "Triangle", "Expanded triangle"),
  fps = 1, loop = FALSE, point.size = 9, height = 280, controls = FALSE
)
small

## ----saddle-frames------------------------------------------------------------
side <- 7L
grid <- expand.grid(x = seq(-1, 1, length.out = side),
                    y = seq(-1, 1, length.out = side))
amplitudes <- seq(0, 1.2, length.out = 17)
saddle.frames <- lapply(amplitudes, function(a) {
  cbind(x = grid$x, y = grid$y, z = a * (grid$x^2 - grid$y^2))
})
ids <- matrix(seq_len(nrow(grid)), side, side)
saddle.edges <- rbind(
  cbind(as.vector(ids[-side, ]), as.vector(ids[-1, ])),
  cbind(as.vector(ids[, -side]), as.vector(ids[, -1]))
)
final.heights <- saddle.frames[[length(saddle.frames)]][, "z"]
height.scale <- color.scale.cont(final.heights, center = 0,
                                 palette = c("#2455A4", "#ECE6C2", "#B83232"))
height.mapping <- map.colors(final.heights, height.scale)
point.colors <- height.mapping$colors

## ----saddle-player, eval=have.rgl---------------------------------------------
saddle.player <- animate.frames(
  saddle.frames, edges = saddle.edges,
  labels = sprintf("Saddle amplitude = %.2f", amplitudes),
  mapping = height.mapping, legend.title = "Final saddle height",
  caption = "Color: final saddle height; positions: current frame.",
  description = "Plane to saddle, with fixed colors for final saddle height.",
  point.size = 6, edge.col = "#314E6E99",
  camera = camera.zup(elevation = 25, turn = -130),
  fps = 8, height = 450, controls = FALSE
)
saddle.player

## ----surface-morph-frames-----------------------------------------------------
stages <- seq(-1, 1, length.out = 33)
surface.frames <- lapply(stages, function(t) {
  cbind(x = grid$x, y = grid$y,
        z = 1.2 * (abs(t) * grid$x^2 - t * grid$y^2))
})
surface.labels <- sprintf("%s | t = %.2f",
  ifelse(stages < 0, "Paraboloid", ifelse(stages == 0, "Flat grid", "Saddle")),
  stages)

## ----surface-morph-player, eval=have.rgl--------------------------------------
surface.player <- animate.frames(
  surface.frames, edges = saddle.edges, labels = surface.labels,
  mapping = height.mapping, legend.title = "Final saddle height",
  caption = "Color: final saddle height; positions: current frame.",
  description = "Paraboloid through a plane to a saddle, with fixed colors for final saddle height.",
  point.size = 6, edge.col = "#314E6E99",
  camera = camera.zup(elevation = 25, turn = -130),
  fps = 8, height = 450, controls = FALSE
)
surface.player

## ----html-export, eval=FALSE--------------------------------------------------
# htmlwidgets::saveWidget(triangle.player, "triangle-playback.html", selfcontained = TRUE)
# htmlwidgets::saveWidget(saddle.player, "saddle-playback.html", selfcontained = TRUE)

## ----gif-export, eval=have.rgl && have.magick---------------------------------
local({
  gif.path <- tempfile(fileext = ".gif")
  on.exit(unlink(gif.path))
  write.animation.gif(small, gif.path, fps = 1,
                       width = 240, height = 240, final.hold = 0)
  stopifnot(file.exists(gif.path))
})

## ----keep-gif, eval=FALSE-----------------------------------------------------
# write.animation.gif(saddle.player, "saddle.gif", fps = 8,
#                      width = 720, height = 560, final.hold = 2,
#                      annotations = TRUE,
#                      loop = TRUE, overwrite = FALSE)

## ----frame-selection, eval=have.grip && have.rgl------------------------------
inspect.index <- unique(as.integer(round(seq(
  1, length(triangle.frames), length.out = min(5L, length(triangle.frames))
))))
selected <- animate.frames(triangle.frames, edges,
                            frame.index = inspect.index,
                            labels = triangle.labels, fps = 2, controls = FALSE)
attr(selected, "ivue.animation")$frame.index

## ----all-frames, eval=FALSE---------------------------------------------------
# all.frames <- animate.frames(my.frames, edges = my.edges, max.frames = NULL, controls = FALSE)

