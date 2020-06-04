devtools::install_github("daniloimparato/easylayout", ref = "dadamorais")

library(igraph)
library(easylayout)

g <- igraph::barabasi.game(n = 5000, directed = FALSE)

layout <- easylayout::vivagraph(g)

plot(g, layout = layout, vertex.size = 1, vertex.label = NA)
