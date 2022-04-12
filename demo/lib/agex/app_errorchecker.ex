
    defmodule App.ErrorChecker do
    
    
 def unfortunate_error_checker(unlucky_string) do 
  fn (str) -> if str  == unlucky_string do
            :unfortunate_error 
          else 
            :other_error 
          end 
         end
end 
    
    end
    