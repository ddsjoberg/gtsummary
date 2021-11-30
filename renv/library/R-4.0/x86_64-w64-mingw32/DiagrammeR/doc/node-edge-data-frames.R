## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load_packages, message=FALSE, warning=FALSE, include=FALSE---------------
library(DiagrammeR)

## -----------------------------------------------------------------------------
# Create a node data frame
nodes_1 <-
  create_node_df(
    n = 4, 
    type = "lower",
    label = c("a", "b", "c", "d"),
    style = "filled",
    color = "aqua",
    shape = c("circle", "circle",
              "rectangle", "rectangle"),
    data = c(3.5, 2.6, 9.4, 2.7))

# Inspect the `nodes_1` NDF
nodes_1

## -----------------------------------------------------------------------------
# Create another node data frame
nodes_2 <-
  create_node_df(
    n = 4,
    type = "upper",
    label = TRUE,
    style = "filled",
    color = "red",
    shape = "triangle",
    data = c(0.5, 3.9, 3.7, 8.2))

# Inspect the `nodes_2` NDF
nodes_2

## -----------------------------------------------------------------------------
# Create an edge data frame
edges_1 <-
  create_edge_df(
    from = c(1, 1, 2, 3),
      to = c(2, 4, 4, 1),
    rel = "requires",
    color = "green",
    data = c(2.7, 8.9, 2.6, 0.6))

edges_1

## -----------------------------------------------------------------------------
# Create another edge data frame
edges_2 <-
  create_edge_df(
    from = c(5, 7, 8, 8),
    to = c(8, 8, 6, 5),
    rel = "receives",
    arrowhead = "dot",
    color = "red")

edges_2

## -----------------------------------------------------------------------------
# Create an NDF
nodes_1 <-
  create_node_df(
    n = 4,
    label = 1:4,
    type = "lower",
    data = c(8.2, 5.2, 1.2, 14.9))

# Create another NDF
nodes_2 <-
  create_node_df(
    n = 4,
    label = 5:8,
    type = "upper",
    data = c(0.3, 6.3, 10.7, 1.2))

# Combine the NDFs
all_nodes <- combine_ndfs(nodes_1, nodes_2)

all_nodes

## -----------------------------------------------------------------------------
# Create an edge data frame
edges_1 <-
  create_edge_df(
    from = c(1, 1, 2, 3),
      to = c(2, 4, 4, 1),
    rel = "requires",
    color = "green",
    data = c(2.7, 8.9, 2.6, 0.6))

# Create another edge data frame
edges_2 <-
  create_edge_df(
    from = c(5, 7, 8, 8),
    to = c(8, 8, 6, 5),
    rel = "receives",
    arrowhead = "dot",
    color = "red")

# Combine edge data frames with 'combine_edfs'
all_edges <- combine_edfs(edges_1, edges_2)

all_edges

