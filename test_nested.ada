procedure Main is
begin
  a := 5;
  b := 10;
  
  if a < b then
    begin
      c := a + b;
      
      while c > 0 loop
        if c mod 2 = 0 then
          Put_Line("Even")
        else
          Put_Line("Odd");
        
        c := c - 1
      end loop
    end;
  
  Put_Line("Done")
end Main;
