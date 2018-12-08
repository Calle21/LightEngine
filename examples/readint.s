ret0, ret1, arg0, ccatch, feight

   store    #0  -> ret0
loop:
   transfer [arg0] -> ccatch
   inc      arg0
   beq      ccatch, #0 -> end
   sub      ccatch, #48 -> ccatch
   mul      ret0, #10 -> ret0
   add      ret0, ccatch -> ret0
   b        loop
end:
   move     arg0 -> ret1
   return


in primitive
   store 0[10],4[5]
loop:
   trans 1[5],2[5],0[1]
   inc   2[5]
   eqz   1[5]
   subr  1[1],3[14]
   subi  1[5],48[5]!,1[5]
   muli  4[5],10[5],4[5]
   add   4[5],1[5],4[5]
   subr  1[1],7[14]
end:
   move  2[5],3[5]
   return
