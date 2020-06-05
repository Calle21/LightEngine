@factorial
   li    1 -> $1 ; return value
loop:
   beq   $0, $1 -> end
   mul   $0, $1 -> $1
   addi  $0, -1 -> $0
   b     loop
end:
   move  $1 -> $0
   br    $13
