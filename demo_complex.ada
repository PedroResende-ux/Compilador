-- Advanced demonstration of symbol table features
procedure Main is
  -- Global variables
  totalScore : Integer;
  averageScore : Integer;
  highScore : Integer;
  isWinner : Boolean;
  
begin
  -- Initialize scores
  totalScore := 0;
  highScore := 100;
  
  -- Nested block: First player
  begin
    totalScore := totalScore + 85;
    Put_Line("Player 1 scored")
  end;
  
  -- Nested block: Second player
  begin
    totalScore := totalScore + 92;
    Put_Line("Player 2 scored")
  end;
  
  -- Calculate average
  averageScore := totalScore / 2;
  
  -- Check if winner
  if averageScore > 80 then
    Put_Line("Team wins!")
  else
    Put_Line("Team loses");
  
  -- Deeply nested logic
  begin
    begin
      Put_Line("Outstanding performance!")
    end
  end;
  
  Put_Line("Game complete")
end Main;
