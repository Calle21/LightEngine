ret; in0, in1; count, help
   store 20 -> count
   store 0  -> ret
loop:
   bez   count -> end
   dec   count
   and   in0, in1 -> help
   add   help, ret -> ret
   sl    in0
   b     loop
end:
   return
