library(shiny)
library(htmltools)
library(magrittr)
library(igraph)

setwd("~/R/easylayout/")

edgelist <- read.table(
  "../neurotransmission/resultados/n6_edgelist.tsv"
  ,header=T
  ,sep="\t"
  ,stringsAsFactors=F
)
gene_info <- read.table(
  "../neurotransmission/resultados/n4_gene_info.tsv"
  ,header=T
  ,sep="\t"
  ,stringsAsFactors=F
)

network <- graph_from_data_frame(edgelist, directed=F, vertices=gene_info)
V(network)$size <- V(network)$system_count * 10

# network <- GSE13904_tree$g
# V(network)$size <- V(network)$nodeSize / 5

graph_json <- jsonlite::toJSON(list(
  nodes = igraph::as_data_frame(network, "vertices")
  ,links = igraph::as_data_frame(network, "edges")))


server <- function(input, output, session) {
  session$sendCustomMessage(type = "dataTransferredFromServer", graph_json)

  observeEvent(input$coordinates, {
    if(!is.null(input$coordinates)) stopApp(input$coordinates)
  })
}

addResourcePath('vivagraph.min.js', 'www/vivagraph.min.js')
addResourcePath('multiselect.js', 'www/multiselect.js')
addResourcePath('index.js', 'www/index.js')

layout <- runGadget(shinyApp(ui = htmlTemplate("www/index.html"), server)) %>% matrix(ncol=2,byrow=T)

plot(as.undirected(network), layout = layout, vertex.size = 1, vertex.label = NA)
