str0:       "Enter numbers. Find the secret number to break the loop\n"
str1:       "Enter a number: "

@main
        li   str0 -> r0
        syscall pString
loop:
        li   str1 -> r0
        syscall pString
        syscall rInt
        beqi r0, 9 -> end
        addi r0, 1 -> r0
        syscall pInt
        li   10 -> r0
        syscall pChar
        b    loop
end:
        exit

