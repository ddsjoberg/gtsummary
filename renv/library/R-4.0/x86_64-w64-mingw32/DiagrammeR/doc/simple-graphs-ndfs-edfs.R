## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load_packages, message=FALSE, warning=FALSE, include=FALSE---------------
library(DiagrammeR)

## -----------------------------------------------------------------------------
# Create the graph object
graph <- create_graph()

## -----------------------------------------------------------------------------
# Get the class of the object
class(graph)

## -----------------------------------------------------------------------------
# It's an empty graph, so the NDF has no rows
get_node_df(graph)

## -----------------------------------------------------------------------------
# The EDF doesn't have any rows either
get_edge_df(graph)

## -----------------------------------------------------------------------------
# By default, the graph is considered directed
is_graph_directed(graph)

## -----------------------------------------------------------------------------
# Create a node data frame
ndf <-
  create_node_df(
    n = 4,
    label = 1:4,
    type  = "lower",
    style = "filled",
    color = "aqua",
    shape = c("circle", "circle",
              "rectangle", "rectangle"),
    data = c(3.5, 2.6, 9.4, 2.7)
  )

# Inspect the NDF
ndf

## -----------------------------------------------------------------------------
# Create the graph and include the
# `nodes` NDF
graph <- create_graph(nodes_df = ndf)

# Examine the NDF within the graph object
get_node_df(graph)

## -----------------------------------------------------------------------------
# Check if it's the same NDF (both externally
# and internally)
all(ndf == graph %>% get_node_df())

## -----------------------------------------------------------------------------
###
# Create a graph with both nodes and edges
# defined, and, add some default attributes
# for nodes and edges
###

# Create a node data frame
ndf <-
  create_node_df(
    n = 4,
    label = c("a", "b", "c", "d"),
    type  = "lower",
    style = "filled",
    color = "aqua",
    shape = c("circle", "circle",
              "rectangle", "rectangle"),
    data = c(3.5, 2.6, 9.4, 2.7)
  )

edf <-
  create_edge_df(
    from = c(1, 2, 3),
    to   = c(4, 3, 1),
    rel  = "leading_to"
  )

graph <-
  create_graph(
    nodes_df = ndf,
    edges_df = edf
  ) %>%
  set_node_attrs(
    node_attr = "fontname",
    values = "Helvetica"
  ) %>%
  set_edge_attrs(
    edge_attr = "color",
    values = "blue"
  ) %>%
  set_edge_attrs(
    edge_attr = "arrowsize",
    values = 2
  )

## -----------------------------------------------------------------------------
# Examine the NDF within the graph object
get_node_df(graph)

## -----------------------------------------------------------------------------
# Have a look at the graph's EDF
get_edge_df(graph)

## ----fig.width=7.3------------------------------------------------------------
graph %>% render_graph()

## -----------------------------------------------------------------------------
# Take the graph object and generate a character
# vector with Graphviz DOT code (using cat() for
# a better appearance)
graph %>% 
  generate_dot() %>%
  cat()

