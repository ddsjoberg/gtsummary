## ---- eval=FALSE---------------------------------------------------------
#  labels <- c("AO-1002", "AEO-1004", "AAI-1009", "AFT-1403", "QZ-9065", "QZ-1021", "RF-0901",
#              "AO-1099", "AFT-1101", "QZ-4933")

## ---- eval=FALSE---------------------------------------------------------
#  library(triebeard)
#  trie <- trie(keys = c("AO", "AEO", "AAI", "AFT", "QZ", "RF"),
#               values = c("Audobon", "Atlanta", "Ann Arbor", "Austin", "Queensland", "Raleigh"))
#  
#  longest_match(trie = trie, to_match = labels)
#  
#   [1] "Audobon"    "Atlanta"    "Ann Arbor"  "Austin"     "Queensland" "Queensland" "Raleigh"    "Audobon"    "Austin"
#  [10] "Queensland"

## ---- eval=FALSE---------------------------------------------------------
#  prefix_match(trie = trie, to_match = "A")
#  
#  [[1]]
#  [1] "Ann Arbor" "Atlanta"   "Austin"    "Audobon"

## ---- eval=FALSE---------------------------------------------------------
#  greedy_match(trie = trie, to_match = "AO")
#  
#  [[1]]
#  [1] "Ann Arbor" "Atlanta"   "Austin"    "Audobon"

## ---- eval=FALSE---------------------------------------------------------
#  library(triebeard)
#  library(microbenchmark)
#  
#  trie <- trie(keys = c("AO", "AEO", "AAI", "AFT", "QZ", "RF"),
#               values = c("Audobon", "Atlanta", "Ann Arbor", "Austin", "Queensland", "Raleigh"))
#  
#  labels <- rep(c("AO-1002", "AEO-1004", "AAI-1009", "AFT-1403", "QZ-9065", "QZ-1021", "RF-0901",
#                  "AO-1099", "AFT-1101", "QZ-4933"), 100000)
#  
#  microbenchmark({longest_match(trie = trie, to_match = labels)})
#  
#  Unit: milliseconds
#                                                    expr      min       lq     mean   median       uq      max neval
#   {     longest_match(trie = trie, to_match = labels) } 284.6457 285.5902 289.5342 286.8775 288.4564 327.3878   100

## ---- eval=FALSE---------------------------------------------------------
#  to_match = "198.0.0.1"
#  trie_inst <- trie(keys = "197", values = "fake range")
#  
#  longest_match(trie_inst, to_match)
#  [1] NA
#  
#  trie_add(trie_inst, keys = "198", values = "home range")
#  longest_match(trie_inst, to_match)
#  [1] "home range"
#  
#  trie_remove(trie_inst, keys = "198")
#  longest_match(trie_inst, to_match)
#  [1] NA

## ---- eval=FALSE---------------------------------------------------------
#  trie <- trie(keys = c("AO", "AEO", "AAI", "AFT", "QZ", "RF"),
#               values = c("Audobon", "Atlanta", "Ann Arbor", "Austin", "Queensland", "Raleigh"))
#  
#  str(as.data.frame(trie))
#  'data.frame':	6 obs. of  2 variables:
#   $ keys  : chr  "AAI" "AEO" "AFT" "AO" ...
#   $ values: chr  "Ann Arbor" "Atlanta" "Austin" "Audobon" ...
#  
#  str(as.list(trie))
#  
#  List of 2
#   $ keys  : chr [1:6] "AAI" "AEO" "AFT" "AO" ...
#   $ values: chr [1:6] "Ann Arbor" "Atlanta" "Austin" "Audobon" ...

