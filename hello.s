str0:       "Hello. Enter your name: "
str1:       |100|
str2:       "Hello "

@main
        li   str0 -> r0
        syscall pString
        li   str1 -> r0
        syscall rString
        li   str2 -> r0
        syscall pString
        li   str1 -> r0
        syscall pString
        li   10 -> r0
        syscall pChar
        exit
