-- Comprehensive test for symbol table functionality
-- Tests: declarations, scopes, variable usage, nested blocks

procedure Main is
  -- Global declarations
  x : Integer;
  y : Integer;
  result : Integer;
  isPositive : Boolean;
  counter : Integer;
begin
  -- Test 1: Basic assignments
  x := 10;
  y := 20;
  result := x + y;
  
  -- Test 2: Conditional with boolean
  if result > 25 then
    Put_Line("Result is greater than 25")
  else
    Put_Line("Result is not greater than 25");
  
  -- Test 3: While loop
  counter := 0;
  while counter < 3 loop
    Put_Line("Loop iteration");
    counter := counter + 1
  end loop;
  
  -- Test 4: Nested block with scope
  begin
    x := 100;
    Put_Line("Inner block - x modified");
    
    -- Test 5: Nested nested block
    begin
      y := 200;
      Put_Line("Nested nested block")
    end
  end;
  
  -- Test 6: Complex expression
  result := (x * 2 + y / 2) mod 10;
  
  -- Test 7: Boolean operations
  isPositive := result > 0 and x > 0;
  
  Put_Line("All tests completed successfully")
end Main;
