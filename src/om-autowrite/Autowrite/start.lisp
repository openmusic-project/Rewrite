#+asdf
 (progn
 (asdf:oos 'asdf:compile-op :nautowrite)
 (asdf:oos 'asdf:load-op :nautowrite)
 )

#-asdf
 (progn
   ;; 1. Load the system definition file:
					;   (load "system.lisp")
   ;; 2. Load the source code of the system:
   (mk:operate-on-system :nautowrite :compile)
   ;; 3. Load the compiled system:
   (mk:operate-on-system :nautowrite :load)
)

