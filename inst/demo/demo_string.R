#devtools::install_github("daniloimparato/easylayout", force=T)

library(easylayout)
library(igraph)
library(ggraph)

string_interactions <- read.table(
  url("https://string-db.org/api/tsv/network?identifiers=UBB&species=9606&add_nodes=200")
  ,sep = "\t"
  ,header = T
  ,stringsAsFactors = F
  ,comment.char = ""
  ,quote = ""
)[,3:4]

g <- graph_from_data_frame(string_interactions, directed=F)

V(g)$size <- degree(g)

layout <- easylayout(g)

ggraph(g, layout = layout) +
  geom_edge_link(color="#999999") +
  geom_node_point(aes(color = size, size = size)) +
  theme_void()

layout_novo <- easylayout(g, layout)

ggraph(g, layout = layout_novo) +
  geom_edge_link(color="#999999") +
  geom_node_point(aes(color = size, size = size)) +
  theme_void()
