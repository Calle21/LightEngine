@reverse
    li   0 -> $1
loop:
    addi $0, 1 -> $0
    lw   $0 -> $2
    sw   $1 -> $0
    beqi $2, 0 -> end
    addi $0, -1 -> $1
    move $2 -> $0
    b    loop
end:
    addi $0, -1 -> $0
    br   $13
