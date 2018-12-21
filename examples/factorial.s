ret; in0;
   store 1 -> ret
loop:
   beq   in0, 1 -> end ; ?
   (acc) <- mul (ret, in0)
      move  acc -> ret
   dec   in0
   b     loop
end:
   return
