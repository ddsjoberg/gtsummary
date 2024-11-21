# modify_source_note() messaging

    Code
      modify_source_note(tbl_summary(trial, include = trt), source_note = letters)
    Condition
      Error in `modify_source_note()`:
      ! The `source_note` argument must be a string, not a character vector.

---

    Code
      modify_source_note(tbl_summary(trial, include = trt), source_note = "ttt",
      text_interpret = letters)
    Condition
      Error in `modify_source_note()`:
      ! `text_interpret` must be one of "md" or "html", not "a".

# remove_source_note(source_note_id) messaging

    Code
      remove_source_note(modify_source_note(tbl_summary(trial, include = trt),
      "Created June 26, 2015"), source_note_id = 100)
    Condition
      Error in `remove_source_note()`:
      ! Argument `source_note_id` is out of bounds.
      i Must be one or more of 1 or `NULL`.

