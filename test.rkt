

; (defelixir Ecto.Changeset
;   (deftype Result integer)
;   (def cast ([Any attrs] Result) "Ecto.Changeset.cast" ))
(defmodule Kek
  ; (alias Ecto.Changeset)  
  (defn increment 
    ([integer] integer) 
    [a] (Kernel.+ a 1)))
