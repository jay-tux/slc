x := 45 ;
b := x = 1 ;
c := !b ;
a := 1 ;

while c do
    (   print x ;
        y := x / 2 ;
        z := 2 * y ;
        c := z = x ;
        if c then
            ( x := y )
        else
        (   b := x = 1 ;
            c := !b ;
            x := x * 3 ;
            x := x + 1
        )
    )
#
