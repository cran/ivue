## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(ivue)
have.rgl <- nzchar(system.file(package = "rgl"))

## ----first-data---------------------------------------------------------------
t <- seq(0, 2 * pi, length.out = 24)
X <- cbind(x = cos(t), y = sin(t), z = seq(-1, 1, length.out = 24))
rownames(X) <- sprintf("point-%02d", seq_len(nrow(X)))
height <- X[, "z"]
stopifnot(ncol(X) == 3L, all(is.finite(X)), length(height) == nrow(X))
height.scale <- color.scale.cont(height, limits = c(-1, 1))
head(map.colors(height, height.scale)$colors)

## ----first-view, eval=have.rgl------------------------------------------------
view <- plot3D.cont(X, values = height, scale = height.scale,
                   legend.title = "Height", legend.width = 120,
                   point.size = 7, height = 340,
                   camera = camera.zup(zoom = 0.65))
view

## ----keep-first-view, eval=FALSE----------------------------------------------
# htmlwidgets::saveWidget(view, "view.html", selfcontained = FALSE)

## ----save-first-view, eval=have.rgl, include=FALSE----------------------------
local({
  directory <- tempfile("ivue-html-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  htmlwidgets::saveWidget(view, file.path(directory, "view.html"),
                          selfcontained = FALSE)
  stopifnot(file.exists(file.path(directory, "view.html")))
})

## ----recovered-camera, eval=FALSE---------------------------------------------
# source("ivue-view.R")
# recovered <- plot3D.plain(X, camera = view$camera, limits = view$limits,
#                           aspect = view$aspect)

## ----finding-help, eval=FALSE-------------------------------------------------
# help(package = "ivue")
# help("ivue-package", package = "ivue")
# help("color.scale.cont", package = "ivue")
# vignette("example-data", package = "ivue")

