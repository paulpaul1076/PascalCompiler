PROGRAM hello (output);

{Write 'Hello, world.' ten times.}

VAR
  i : integer;

BEGIN
  {hello}
  FOR i := 1 TO 10 DO
  begin
    writeln('Hello, world.');
  end;
END {hello.}.
