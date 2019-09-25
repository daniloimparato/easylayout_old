library(shiny)
library(htmltools)
library(magrittr)
library(igraph)

setwd("C:/R/easylayout/")

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
V(network)$size <- V(network)$system_count * 3

graph_json <- jsonlite::toJSON(list(
  nodes = igraph::as_data_frame(network, "vertices")
  ,links = igraph::as_data_frame(network, "edges")))


server <- function(input, output, session) {
  session$sendCustomMessage(type = "dataTransferredFromServer", graph_json)
  
  session$onEnded(function() {
    session$sendCustomMessage(type = "getLatestCoordinates", TRUE)
  })

  onStop(function() {
    session$sendCustomMessage(type = "getLatestCoordinates", TRUE)
  })
  
  observeEvent(input$mydata, {
    if(!is.null(input$mydata)) stopApp(input$mydata)
  })
}

addResourcePath('vivagraph.min.js', 'www/vivagraph.min.js')
addResourcePath('multiselect.js', 'www/multiselect.js')
addResourcePath('index.js', 'www/index.js')

layout <- runApp(shinyApp(ui = htmlTemplate("www/index.html"), server))# %>% matrix(ncol=2,byrow=T)


plot(network, layout = layout, vertex.size = 1, vertex.label = NA)