@make
    sw   $13 -> $12
    addi $12, 1 -> $12
    addi $15, 1 -> $13
    b    cons
    addi $12, -1 -> $12
    lw   $12 -> $13
    move $0 -> $1
    lw   $1 -> $0
    beqi $0, 0 -> end
    addi $0, -1 -> $0
    b    make
end:
    move $1 -> $0
    br   $13
