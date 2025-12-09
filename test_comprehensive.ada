procedure Main is
  a : Integer;
  b : Integer;
  c : Integer;
  counter : Integer;
  factorial : Integer;
begin
  a := 10;
  b := 20;
  c := a + b * 2;
  
  if c > 30 then
    Put_Line("Large number")
  else
    Put_Line("Small number");
  
  counter := 0;
  factorial := 1;
  
  while counter < 5 loop
    counter := counter + 1;
    factorial := factorial * counter
  end loop;
  
  Put_Line("Factorial of 5 is:");
  Put_Line(factorial);
  
  if factorial > 100 then
    Put_Line("Factorial is greater than 100")
end Main;
