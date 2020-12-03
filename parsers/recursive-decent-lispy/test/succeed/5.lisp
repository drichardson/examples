; my binary
(binary
  (name mybinary)
  (sources
    file1.c
    file2.c
    file3.h
    )
  (dependencies
    //package:somelib
    :mylib1
    )
  )

; my library that does cool things
(library
  (name mylib1)
  (sources
    file3.c
    file4.c
    )
  (dependencies
    //base:base
    )
  )

