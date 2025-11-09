procedure Main is
begin
  x := 10;
  y := 20;
  z := x + y * 2;
  
  if z > 30 then
    Put_Line("Z is greater than 30")
  else
    Put_Line("Z is not greater than 30");
  
  counter := 0;
  while counter < 5 loop
    Put_Line("Counter value:");
    counter := counter + 1
  end loop;
  
  Put_Line("Program finished")
end Main;