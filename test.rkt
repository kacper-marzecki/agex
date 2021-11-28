

(defmodule App.Example
  (alias App.Example.Types)  

  (defn parse 
    ([string] (| {:ok integer} 
                 {:error Types.Errors}))
     [input]  (let [error_fn (unfortunate_error_checker "13")] 
                (if (= input "42") 
                  {:ok 42}
                  {:error (error_fn input)})))

  ; generates a function that checks if the number is 'unlucky' and returns an :unfortunate_error of other_error
  (defn unfortunate_error_checker 
    ([string] (fn [string] (| :other_error :unfortunate_error)))
    [unlucky_string] 
      (fn [input]
        (if (= input unlucky_string) 
                            :unfortunate_error 
                            :other_error)))

  (defn increment 
    ([integer] integer) 
    [a] (let [z 2]
          (+ a z))))

(defmodule App.Example.Types
  (deftype Errors (| :unfortunate_error :error_666 :other_error )))