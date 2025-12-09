procedure Main is
  x : Integer;
  y : Integer;
begin
  x := 10;
  begin
    y := 20;
    Put_Line("Inner block")
  end;
  Put_Line("Outer block")
end Main;
