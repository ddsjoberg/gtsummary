## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load_packages, message=FALSE, warning=FALSE, include=FALSE---------------
library(DiagrammeR)

## -----------------------------------------------------------------------------
pre <- 
  create_graph() %>%
  add_node() %>%
  add_node() %>%
  add_edge(from = 1, to = 2) %>%
  select_nodes_by_id(nodes = 1)

## -----------------------------------------------------------------------------
# Create a simple graph, create a single-
# node selection and traverse to the other
# node; obtain the final selection
create_graph() %>%
  add_node() %>%
  add_node() %>%
  add_edge(from = 1, to = 2) %>%
  select_nodes_by_id(nodes = 1) %>%
  trav_out() %>%
  get_selection()

## -----------------------------------------------------------------------------
# Create a simple graph, create a single-
# node selection and then attempt to traverse
# to the other node; obtain the final selection
create_graph() %>%
  add_node() %>%
  add_node() %>%
  add_edge(from = 2, to = 1) %>%
  select_nodes_by_id(nodes = 1) %>%
  trav_out() %>%
  get_selection()

## -----------------------------------------------------------------------------
# A traversal can occur if `trav_in()` is used
# instead of `trav_out()`
create_graph() %>%
  add_node() %>%
  add_node() %>%
  add_edge(from = 2, to = 1) %>%
  select_nodes_by_id(nodes = 1) %>%
  trav_in() %>%
  get_selection()

## -----------------------------------------------------------------------------
# Traverse across a path graph one
# step at a time with `trav_out()`
create_graph() %>%
  add_path(n = 5) %>%
  select_nodes_by_id(nodes = 1) %>%
  trav_out() %>%
  trav_out() %>%
  trav_out() %>%
  trav_out() %>%
  get_selection()

## -----------------------------------------------------------------------------
graph_1 <- 
  create_graph() %>%
  add_node() %>%
  select_nodes_by_id(nodes = 1) %>%
  add_n_nodes_ws(
    n = 5,
    direction = "from"
  ) %>%
  add_n_nodes_ws(
    n = 5,
    direction = "to"
  )

graph_1 %>% render_graph()

## -----------------------------------------------------------------------------
graph_1 %>%
  trav_out() %>%
  get_selection()

## -----------------------------------------------------------------------------
graph_1 %>%
  trav_in() %>%
  get_selection()

## -----------------------------------------------------------------------------
# Create the graph described in the paragraph
# above ({`2...4`} -> `1` -> {`5...7`}),
# start from node `1` (as a selection),
# traverse to all other adjacent nodes and
# then obtain the current selection
create_graph() %>%
  add_node() %>%
  select_nodes_by_id(nodes = 1) %>%
  add_n_nodes_ws(
    n = 3,
    direction = "to"
  ) %>%
  add_n_nodes_ws(
    n = 3,
    direction = "from"
  ) %>%
  trav_both() %>%
  get_selection()

## -----------------------------------------------------------------------------
# Create a common graph with nodes having
# various `type` values; set to render
# always using `visNetwork` when calling
# `render_graph()`
graph <-
  create_graph() %>%
  add_node(type = "type_a") %>%
  add_n_nodes(
    n = 4,
    type = "type_b"
  ) %>%
  add_edge(from = 1, to = 2) %>%
  add_edge(from = 1, to = 3) %>%
  add_edge(from = 4, to = 1) %>%
  add_edge(from = 5, to = 1) %>%
  add_n_nodes(
    n = 4,
    type = "type_c"
  ) %>%
  add_edge(from = 1, to = 6) %>%
  add_edge(from = 1, to = 7) %>%
  add_edge(from = 8, to = 1) %>%
  add_edge(from = 9, to = 1)

# View the created graph
graph %>% render_graph()

## -----------------------------------------------------------------------------
graph %>%
  select_nodes_by_id(nodes = 1) %>%
  trav_out() %>%
  get_selection()

graph %>%
  select_nodes_by_id(nodes = 1) %>%
  trav_out(conditions = type == "type_b") %>%
  get_selection()

graph %>%
  select_nodes_by_id(nodes = 1) %>%
  trav_out(conditions = type == "type_c") %>%
  get_selection()

# Once the nodes have been selected via
# a traversal, a useful thing to do would
# be to attach new nodes to that selection
updated_graph <-
  graph %>%
  select_nodes_by_id(nodes = 1) %>%
  trav_out(conditions = type == "type_c") %>%
  add_n_nodes_ws(
    n = 1,
    direction = "from",
    type = "type_d"
  )

# View the updated graph
updated_graph %>% render_graph()

## -----------------------------------------------------------------------------
# Create a graph with fruit, vegetables,
# and nuts
ndf <-
  create_node_df(
    n = 9,
    type = c(
      "fruit", "fruit", "fruit",
      "veg", "veg", "veg",
      "nut", "nut", "nut"
    ),
    label = c(
      "pineapple", "apple",
      "apricot", "cucumber",
      "celery", "endive",
      "hazelnut", "almond",
      "chestnut"
    )
  )

edf <-
  create_edge_df(
    from = c(
      9, 3, 6, 2, 6,
      2, 8, 2, 5, 5
    ),
    to = c(
      1, 1, 4, 3, 7,
      8, 1, 5, 3, 6
    )
  )

graph <-
  create_graph(
    nodes_df = ndf,
    edges_df = edf
  )

# View the graph
graph %>% render_graph()

## -----------------------------------------------------------------------------
# View the internal NDF for sake of
# reference
graph %>% get_node_df()

## -----------------------------------------------------------------------------
# Select all nodes with a label beginning
# with `a` and traverse outward to all nodes
graph %>%
  select_nodes(
    conditions = grepl("^a", label)
  ) %>%
  trav_out() %>%
  get_selection()

# This traversal results in a rather large
# selection of nodes: `3` (`apricot`), `8`
# (`almond`), `5` (`celery`), and `1`
# (`pineapple`)

# Now, select all nodes with a label beginning
# with `c` (in this case, the `cucumber` and
# `chestnut` and then traverse outward to any
# node of the `fruit` type
graph %>%
  select_nodes(
    conditions = grepl("^c", label)
  ) %>%
  trav_out(
    conditions = type == "fruit"
  ) %>%
  get_selection()

# The traversal has resulted in a selection of
# nodes `3` (`apricot`) and `1` (`pineapple`)

## -----------------------------------------------------------------------------
# Create a simple graph with two nodes, an
# edge between them (`1` -> `2`); starting
# from node `1` (as a selection), traverse
# to the edge and then obtain the current
# selection
create_graph() %>%
  add_node() %>%
  add_node() %>%
  add_edge(from = 1, to = 2) %>%
  select_nodes_by_id(nodes = 1) %>%
  trav_out_edge() %>%
  get_selection()

# If no traversal can occur the selection is
# not altered. To demonstrate, use a similar
# pipeline but reverse the edge direction
create_graph() %>%
  add_node() %>%
  add_node() %>%
  add_edge(from = 2, to = 1) %>%
  select_nodes_by_id(nodes = 1) %>%
  trav_out_edge() %>%
  get_selection()

# A traversal can occur if `trav_in_edge()`
# is used instead of `trav_out_edge()`
create_graph() %>%
  add_node() %>%
  add_node() %>%
  add_edge(from = 2, to = 1) %>%
  select_nodes_by_id(nodes = 1) %>%
  trav_in_edge() %>%
  get_selection()

# A selection of multiple edges can occur
# as a result of a traversal
create_graph() %>%
  add_node() %>%
  select_nodes_by_id(nodes = 1) %>%
  add_n_nodes_ws(
    n = 10,
    direction = "from"
  ) %>%
  add_n_nodes_ws(
    n = 10,
    direction = "to"
  ) %>%
  trav_out_edge() %>%
  get_selection()

create_graph() %>%
  add_node() %>%
  select_nodes_by_id(nodes = 1) %>%
  add_n_nodes_ws(
    n = 10,
    direction = "from"
  ) %>%
  add_n_nodes_ws(
    n = 10,
    direction = "to"
  ) %>%
  trav_in_edge() %>%
  get_selection()

## -----------------------------------------------------------------------------
# First, set a seed so the example
# is reproducible
suppressWarnings(RNGversion("3.5.0"))
set.seed(20)

# Create a graph with fruit,
# vegetables, nuts, and... people!
ndf <-
  create_node_df(
    n = 14,
    type = c(
      "person", "person",
      "person", "person",
      "person", "fruit",
      "fruit", "fruit",
      "veg", "veg", "veg",
      "nut", "nut", "nut"
    ),
    label = c(
      "Annie", "Donna",
      "Justine", "Ed",
      "Graham", "pineapple",
      "apple", "apricot",
      "cucumber", "celery",
      "endive", "hazelnut",
      "almond", "chestnut"
    )
  )

edf <-
  create_edge_df(
    from = sort(
      as.vector(replicate(5, 1:5))
    ),
    to = as.vector(
      replicate(5, sample(6:14, 5))
    ),
    rel = as.vector(
      replicate(
        5, sample(
          c(
            "likes", "dislikes",
            "allergic_to"
          ), 5,
          TRUE,
          c(0.5, 0.25, 0.25)
        )
      )
    )
  )

graph <-
  create_graph(
    nodes_df = ndf,
    edges_df = edf
  )

graph %>% render_graph()

