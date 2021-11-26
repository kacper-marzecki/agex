

(defelixir Ecto.Changeset
  (deftype Result Integer)
  (def cast ([Any attrs] Result) "Ecto.Changeset.cast" ))

(defmodule Kek
  (alias Ecto.Changeset)  
  (defn increment 
    ([Integer] Integer) 
    [a] (Math.plus a amount)))
