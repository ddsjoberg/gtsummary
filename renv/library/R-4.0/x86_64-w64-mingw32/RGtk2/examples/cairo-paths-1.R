path <- cr$copyPath()$data

for (data in path) {
  switch(CairoPathDataType[attr(data, "type") + 1L],
         "move-to" = do_move_to_things(data[1], data[2]),
         "line-to" = do_line_to_things(data[1], data[2]),
         "curve-to" =  do_curve_to_things(data[1], data[2], data[3], data[4],
           data[5], data[6]),
         "close-path" = do_close_path_things())
}
