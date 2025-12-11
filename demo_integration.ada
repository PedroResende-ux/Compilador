-- Demonstration of integrated compiler features
-- Combines symbol table (PR #2) and code generation (PR #3)

procedure Main is
  -- Variable declarations with types (PR #2)
  counter : Integer;
  limit : Integer;
  sum : Integer;
  flag : Boolean;
begin
  -- Arithmetic operations
  counter := 1;
  limit := 5;
  sum := 0;
  
  -- While loop with semantic checking
  while counter <= limit loop
    sum := sum + counter;
    counter := counter + 1
  end loop;
  
  -- Conditional with boolean
  flag := sum > 10;
  
  if flag then
    Put_Line("Sum is greater than 10")
  else
    Put_Line("Sum is 10 or less");
  
  Put_Line("Computation complete")
end Main;
