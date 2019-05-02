ORG $1000
    
DATA {
arr1: DUP 16
arr2:
    DB $00
    DB $01
    DB $02
}
    
CODE {
    LDX $FF
    TXS
    BRK

    JSR $FFFF
    JMP ($ABCD)
    ADC ($FF, X)
    LDA #arr1
    JMP (SUBRT_2)
    STA MEM,Y 
    
SUBRT_1:    
    JSR $ABCD

SUBRT_2:    
    JSR $DDEE
}
