procedure Main is
begin
  i := 0;
  sum := 0;
  
  while i < 10 loop
    sum := sum + i;
    i := i + 1
  end loop;
  
  Put_Line("Sum of 0 to 9:");
  Put_Line(sum)
end Main;
