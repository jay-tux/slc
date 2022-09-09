x := 5;
a := 1;
c := 0 < x;
while c do (a := a * x ; x := x - 1 ; c := 0 < x);
print a
#