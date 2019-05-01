ORG $#1000

DATA:
arr1: 
arr2:   

CODE:   
    LDX $#FF, X
    TXS
    BRK

    JSR SUBRT_2

SUBRT_1:    
    JSR SUBRT_2

SUBRT_2:    
    JSR SUBRT_1

