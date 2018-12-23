(progn
  (asdf:oos 'asdf:compile-op :nautowrite)
  (asdf:oos 'asdf:compile-op :gautowrite)
  (asdf:oos 'asdf:load-op :nautowrite)
  (asdf:oos 'asdf:load-op :gautowrite)
  )

