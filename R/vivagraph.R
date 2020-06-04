rescale <- function(x, from, to) approxfun(range(x), c(from, to))(x)

#' @export
vivagraph <- function(
   graph
  ,layout                  = NULL
  ,precompute_iterations   = 1000
  ,initial_size_multiplier = 75
  ,pin_nodes               = FALSE
  ,pin_threshold           = 4
  ,pinned_cols             = 2
  ,pinned_rows             = "auto"
  ,pinned_size_multiplier  = 20
  ,lcc_margin_left         = 300
){

  if(is.null(igraph::V(graph)$name)){
    V(graph)$name <- 1:igraph::vcount(graph)
  }

  if(pinned_rows == "auto") {
    pinned_rows <- 0
  }

  subgraphs        <- igraph::decompose.graph(graph)
  subgraphs_to_pin <- sapply(subgraphs, vcount) <= pin_threshold

  # Magic precomputing
  vertices <- igraph::as_data_frame(graph, "vertices")
  numeric_columns <- vertices %>% dplyr::select(-name) %>% dplyr::select_if(is.numeric)

  if(all(numeric_columns %>% dim) != 0){

    g_v <- numeric_columns %>%
      apply(2, rescale, from = 0.001, to = 1) %>%
      dist %>%
      as.matrix

    g_v <- 1/g_v^2

    g_v[g_v == Inf] <- max(g_v[g_v != max(g_v)])
    # g_v[g_v <= 1] <- 0

    row.names(g_v) <- igraph::V(graph)$name
    colnames(g_v)  <- igraph::V(graph)$name

    dist_graph <- igraph::graph_from_adjacency_matrix(g_v, mode = "undirected", weighted = TRUE, diag = FALSE)

    dist_layout <- igraph::layout_with_fr(dist_graph, niter = precompute_iterations) * initial_size_multiplier

    V(graph)$x <- dist_layout[,1]
    V(graph)$y <- dist_layout[,2]

  }

  if(is.matrix(layout)){
    igraph::V(graph)$x <- layout[,1]
    igraph::V(graph)$y <- layout[,2]
  }

  # Lay unconnected nodes on grid
  if(pin_nodes == TRUE) {

    unconnected_nodes <- lapply(subgraphs[subgraphs_to_pin], igraph::as_data_frame, what = "vertices") %>% dplyr::bind_rows()
    unconnected_edges <- lapply(subgraphs[subgraphs_to_pin], igraph::as_data_frame, what = "edges") %>% dplyr::bind_rows()

    if(all(unconnected_nodes %>% dim) != 0 & all(unconnected_edges %>% dim) != 0){
      unconnected_graph  <- igraph::graph_from_data_frame(unconnected_edges, directed = F, vertices = unconnected_nodes)
      unconnected_layout <- igraph::layout_on_grid(unconnected_graph, width = pinned_cols, height = pinned_rows) * pinned_size_multiplier

      igraph::V(graph)[igraph::V(unconnected_graph)$name]$x <- unconnected_layout[,1] - lcc_margin_left
      igraph::V(graph)[igraph::V(unconnected_graph)$name]$y <- unconnected_layout[,2]
      igraph::V(graph)[igraph::V(unconnected_graph)$name]$pinned <- 1
    }
  }

  # Shiny stuff ---------------------
  graph_json <- jsonlite::toJSON(list(
     nodes = igraph::as_data_frame(graph, "vertices")
    ,links = igraph::as_data_frame(graph, "edges"))
  )

  server <- function(input, output, session) {
    session$sendCustomMessage(type = "dataTransferredFromServer", graph_json)

    shiny::observeEvent(input$coordinates, {
      if(!is.null(input$coordinates)) shiny::stopApp(input$coordinates)
    })
  }

  shiny::addResourcePath("www", system.file("www", package = "easylayout"))

  layout <- shiny::runGadget(shiny::shinyApp(ui = shiny::htmlTemplate(system.file("www/index.html", package = "easylayout")), server))

  layout <- matrix(layout, ncol = 2, byrow = TRUE)

  layout[,2] <- -1 * layout[,2]

  layout
}

#' @export
rotate_layout <- function(layout, angle){
  angle <- angle*(pi/180)
  rotm <- matrix(c(cos(angle),sin(angle),-sin(angle),cos(angle)),ncol=2)
  layout <- t(rotm %*% (t(layout)))
}
