test:       "Please enter a number: "
mem:        22
poi:        22

@main
        li   test -> r0
        syscall pString
        exit
