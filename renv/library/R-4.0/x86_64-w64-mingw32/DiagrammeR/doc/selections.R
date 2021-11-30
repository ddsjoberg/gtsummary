## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load_packages, message=FALSE, warning=FALSE, include=FALSE---------------
library(DiagrammeR)

## ----example_graph_1----------------------------------------------------------
# Create a node data frame
ndf <-
  create_node_df(
    n = 4,
    data = c(
      9.7, 8.5, 2.2, 6.0)
  )

# Create a new graph based on the ndf
graph <- create_graph(nodes_df = ndf)

# Inspect the graph's NDF
graph %>% get_node_df()

## -----------------------------------------------------------------------------
# Select nodes where the `data` attribute
# has a value greater than 7.0 (it's the
# first 2 nodes)
graph <-
  graph %>%
  select_nodes(
    conditions = data > 7.0
  )

# Get the graph's current selection
# of nodes as a table
graph %>% get_node_df_ws()

## -----------------------------------------------------------------------------
# Create a node data frame
ndf <-
  create_node_df(
    n = 4,
    fruits = c(
      "apples", "apricots",
      "bananas", "plums")
  )

# Create a new graph based on the ndf
graph <- create_graph(nodes_df = ndf)

# Select nodes where the `fruits`
# attribute has a match on the first
# letters being `ap` (the first 2 nodes)
graph <-
  graph %>%
  select_nodes(
    conditions = grepl("^ap", fruits)
  )

# Get the graph's current selection
# of nodes as a table
graph %>% get_node_df_ws()

## -----------------------------------------------------------------------------
# Create a node data frame
ndf <-
  create_node_df(
    n = 9,
    type = c(
      "fruit", "fruit", "fruit",
      "veg", "veg", "veg",
      "nut", "nut", "nut"),
    label = c(
      "pineapple", "apple", "apricot",
      "cucumber", "celery", "endive",
      "hazelnut", "almond", "chestnut"),
    count = c(
      6, 3, 8, 7, 2, 6, 9, 9, 7)
  )

# Create a new graph based on the ndf
graph <- create_graph(nodes_df = ndf)

# Inspect the graph's NDF
graph %>% get_node_df()

## -----------------------------------------------------------------------------
# Select all foods that either begin
# with `c` or ending with `e`
graph_1 <-
  graph %>%
  select_nodes(
    conditions = grepl("^c", label)
  ) %>%
  select_nodes(
    conditions = grepl("e$", label),
    set_op = "union"
  )

# Get the graph's current selection
# of nodes as a table
graph_1 %>% get_node_df_ws()

## -----------------------------------------------------------------------------
# Select any food beginning with `a` and
# having a count less than 5
graph_2 <-
  graph %>%
  select_nodes(
    conditions = grepl("^a", label)
  ) %>%
  select_nodes(
    conditions = count < 5,
    set_op = "intersect"
  )

# Get the graph's current selection
# of nodes as a table
graph_2 %>% get_node_df_ws()

## -----------------------------------------------------------------------------
# Select any fruit not containing
# `apple` in its name
graph_3 <-
  graph %>%
  select_nodes(
    conditions = type == "fruit"
  ) %>%
  select_nodes(
    conditions = grepl("apple", label),
    set_op = "difference"
  )

# Get the graph's current selection
# of nodes as a table
graph_3 %>% get_node_df_ws()

## -----------------------------------------------------------------------------
# Create a node data frame
ndf <-
  create_node_df(
    n = 10,
    data = seq(0.5, 5, 0.5)
  )

# Create a new graph based on the ndf
graph <- create_graph(nodes_df = ndf)

# Inspect the graph's NDF
graph %>% get_node_df()

## -----------------------------------------------------------------------------
# Select from a subset of nodes
# (given as `nodes = 1:6`) where
# the `data` value is greater than `1.5`
graph <-
  graph %>%
  select_nodes(
    conditions = data > 1.5,
    nodes = 1:6
  )

# Get the graph's current selection
# of nodes as a table
graph %>% get_node_df_ws()

## -----------------------------------------------------------------------------
# Create a node data frame
ndf <- create_node_df(n = 4)

# Create an edge data frame
edf <-
  create_edge_df(
    from = c(1, 2, 3, 4),
      to = c(2, 3, 4, 1),
    data = c(
      8.6, 2.8, 6.3, 4.5)
  )

# Create a new graph from
# the NDF and EDF
graph <- 
  create_graph(
    nodes_df = ndf,
    edges_df = edf
  )

# Inspect the graph's EDF
graph %>% get_edge_df()

## -----------------------------------------------------------------------------
# Select edges where the `data`
# attribute has a value
# greater than 5.0
graph <-
  graph %>%
  select_edges(
    conditions = data > 5.0
  )

# Get the graph's current selection
# of edges as a table
graph %>% get_edge_df_ws()

## -----------------------------------------------------------------------------
# Clear the graph's selection
graph <-
  graph %>%
  clear_selection()

# Check whether there is still
# a selection present
graph %>% get_selection()

## -----------------------------------------------------------------------------
# Select the last node in the graph's NDF and confirm
# that the selection was made
graph %>%
  select_last_nodes_created() %>%
  get_node_df_ws()

## -----------------------------------------------------------------------------
# Select the last edge in the graph's EDF and confirm
# that the selection was made
graph %>%
  select_last_edges_created() %>%
  get_edge_df_ws()

## -----------------------------------------------------------------------------
# Create a graph, node-by-node and
# edge-by-edge and add attributes
graph_2 <-
  create_graph() %>%
  add_node() %>%
  select_last_nodes_created() %>%
  set_node_attrs_ws(
    node_attr = "timestamp",
    value = as.character(Sys.time())
  ) %>%
  set_node_attrs_ws(
    node_attr = "type",
    value = "A"
  ) %>%
  clear_selection() %>%
  add_node() %>%
  select_last_nodes_created() %>%
  set_node_attrs_ws(
    node_attr = "timestamp",
    value = as.character(Sys.time())
  ) %>%
  set_node_attrs_ws(
    node_attr = "type",
    value = "B"
  ) %>%
  add_edge(
    from = 1,
    to = 2,
    rel = "AB"
  ) %>%
  select_last_edges_created() %>%
  set_edge_attrs_ws(
    edge_attr = "timestamp",
    value = as.character(Sys.time())
  ) %>%
  clear_selection()

# View the new graph
graph_2 %>% render_graph()

## -----------------------------------------------------------------------------
# Inspect the new graph's NDF
graph_2 %>% get_node_df()

## -----------------------------------------------------------------------------
# Inspect the new graph's EDF
graph_2 %>% get_edge_df()

