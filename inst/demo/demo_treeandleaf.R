#devtools::install_github("daniloimparato/easylayout", force=T)

library(easylayout)
library(igraph)
library(ggraph)

load()

g <- graph_from_data_frame(string_interactions, directed=F)

V(g)$degree <- degree(g)

layout <- easylayout(g)

ggraph(g, layout = layout) +
  geom_edge_link(color="#999999") +
  geom_node_point(aes(color = degree, size = degree)) +
  theme_void()
