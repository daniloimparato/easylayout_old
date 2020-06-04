devtools::install_github("daniloimparato/easylayout", ref = "dadamorais")

library(igraph)
library(easylayout)

g <- igraph::erdos.renyi.game(n = 5000, p.or.m = 10000, type = "gnm")

layout <- easylayout::vivagraph(g, pin_nodes = TRUE, pinned_cols = 10)

plot(g, layout = layout, vertex.size = 1, vertex.label = NA)
