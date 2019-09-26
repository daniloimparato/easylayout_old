devtools::install_github("daniloimparato/easylayout")

library(easylayout)
library(igraph)
library(ggraph)

resolve_identifier <- read.table(
  url("https://string-db.org/api/tsv/get_string_ids?identifiers=UBB&species=9606")
  ,sep = "\t"
  ,header = T
  ,stringsAsFactors = F
  ,comment.char = ""
  ,quote = ""
)[1,"stringId"]


string_resolve <- read.table(
  url(paste0("https://string-db.org/api/tsv/interaction_partners?identifiers=", resolve_identifier))
  ,sep = "\t"
  ,header = T
  ,stringsAsFactors = F
  ,comment.char = ""
  ,quote = ""
)
