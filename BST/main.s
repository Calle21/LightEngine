str:        "Please enter a string: "

@main
        li   mem -> r0   ; prepare
        li   poi -> r1   ;   for
        sw   r0 -> r1    ;     alloc

        li   str -> r0
        syscall pString
        addi ic, 1 -> link
        b    get
        addi ic, 1 -> link
        b    print
        li   10 -> r0
        syscall pChar
        exit
