not  r0 -> r1       ; [RG,RG]       [4,4]
move r0 -> r1       ; [RG,RG]       [4,4]

add  r0, r1 -> r2   ; [RG,RG,RG]    [4,4,4]
and  r0, r1 -> r2   ; [RG,RG,RG]    [4,4,4]
...

li   55 -> r0
li   lab -> r0      ; [IA,RG]       [55,4]

addi r0, 44 -> r1   ; [RG,IM,RG]    [4,51,4]
...

lw   r0 -> r1
lw   r0 + 3 -> r1
lw   r0 - 8 -> r1   ; [PL,RG]       [4,51,4]

sw   r0 -> r1
sw   r0 -> r1 + 1
sw   r0 -> r1 - 8   ; [RG,PL]       [4,4,51]

b    end            ; [LB]          [59]

br   link           ; [RG]          [4]

beq  r0, r1 -> end  ; [RG,RG,LB]    [4,4,51]
...
beqi r0, 0 -> end   ; [RG,IM,LB]    [4,16,39]

exit                ; []            []

syscall pInt        ; [SC]          [3]
