@factorial (r0 = arg, r1 = ret)
    ret = 1
loop:
    if arg <= 1 -> end
    ret = arg * ret
    arg = arg - 1
    b    loop
end:
    ret0 = ret
    return







 -- old
@factorial
   li    1 -> r1 ; return value
loop:
   beq   r0, r1 -> end
   mul   s, r0, r1 -> r1
   addi  s, r0, -1 -> r0
   b     loop
end:
   move  r1 -> r0
   br    link
