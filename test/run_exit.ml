let () = if Array.length Sys.argv > 1 then Sys.argv.(1) |> int_of_string |> exit
