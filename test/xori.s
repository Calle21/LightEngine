@main
       li   10 -> r1
       xori r1, 10 -> r0
       addi r0, 26 -> r0
       syscall pInt
       li   10 -> r0
       syscall pChar
       exit
