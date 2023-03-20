;*
;IABSS  (HL) 16 bit signed integer absolute value.  Returns HL, CY = sign
;IADD   (HL, DE) 16 bit unsigned integer addition: HL + DE. Returns HL, CY = overflow
;IADDS  (HL, DE) 16 bit signed integer addition: HL + DE. Returns HL, CY = overflow
;IASC   (@DE) 16 bit signed convert ASCII Decimal number to Integer.
;ICMP   (HL, DE) 16 bit unsigned integer compare: HL - DE
;IDIVS  (HL, DE) 16 bit signed division: HL / DE
;INEGS  (HL) 16 bit signed integer negation: returns 2's complement of HL
;ISUB   (HL, DE) 16 bit unsigned integer subtraction: HL - DE.  Returns HL
;ISUBS  (HL, DE) 16 bit signed integer subtraction



DSIGN:  DS 1            ;byte used by IDIVS
ISIGN:  DS 1            ;byte used by IASC

;-----------------------------------------------------------------------------
;(HL) 16 bit signed integer absolute value.  Returns HL, CY = sign
IABSS:  MOV A, H        ;Check the sign
        ORA A
        RP              ;Done if positive, CY=0
        CALL INEGS      ;Form 2's complement
        STC             ;CY=1
        RET


;------------------------------------------------------------------------------
;(HL, DE) 16 bit unsigned integer addition: HL + DE. Returns HL, CY = overflow
IADD:   MOV A, L
        ADD E
        MOV L, A
        MOV A, H
        ADC D
        MOV H, A
        RET

;------------------------------------------------------------------------------
;(HL, DE) 16 bit signed integer addition: HL + DE. Returns HL, CY = overflow
IADDS:  MOV A, H        ;HL and DE both positive?
        RLC
        JC IADDN
        MOV A, D
        RLC
        JC IADDN
        DAD D           ;  Yes, then add
        MOV A, H
        RLC
        RET
IADDN:  MOV A, H        ;HL and DE both negative?
        RLC
        JNC IADDM
        MOV A, D
        RLC
        JNC IADDM
        DAD D           ;  Yes, then add
        MOV A, H
        RLC
        CMC
        RET
IADDM:  DAD D           ;HL and DE have mixed signs
        STC
        CMC
        RET

;------------------------------------------------------------------------------
;(@DE) 16 bit signed convert ASCII Decimal number to Integer.
;Returns HL=value, CY=err
IASC:   PUSH B
        LXI H, 0        ;Init result = 0
        SUB A
        STA ISIGN       ;Init sign = 0 (positive)
        CALL NOSPC      ;Skip leading spaces, A=1st char
        CPI '+'         ;Skip leading '+'
        JNZ ASCIN0
        INX D
        LDAX D
        JMP ASCIN1
ASCIN0: CPI '-'         ;Negative?
        JNZ ASCIN1
        MVI A, 1        ;Sign = 1 (negative)
        STA ISIGN
        INX D
        LDAX D
ASCIN1: CALL IS09       ;Check for a digit
        JC ASCIXT       ;  if not exit with CY=1
ASCIN2: SUI '0'         ;Convert ASCII digit to hex byte
        MOV B, H        ;Mult HL by 10
        MOV C, L
        DAD H           ;  x2
        DAD H           ;  x4
        DAD H           ;  x8
        DAD B           ;  x9
        DAD B           ;  x10
        JC ASCIXT       ;Overflow (>FFFF)?
        MVI B, 0        ;Add byte in A to HL
        MOV C, A
        DAD B
        JC ASCIXT       ;Overflow (>FFFF)?
        INX D           ;A = next Char
        LDAX D
        CALL IS09       ;Another digit?
        JNC ASCIN2      ;  loop if yes
        MOV A, H        ;Result > 7FFFH (Overflow)?
        ANI 80H
        JNZ ASCIXT
        LDA ISIGN       ;Check sign
        ORA A
        JZ ASCIN4
        CALL INEGS      ;Negative so form 2's complement
ASCIN4: SUB A           ;Successful conversion, CY=0
ASCIXT: POP B           ;Exit
        RET

;------------------------------------------------------------------------------
;(HL, DE) 16 bit unsigned integer compare: HL - DE
;Returns correct C & Z flags. HL, DE unchanged
ICMP:   MOV A, H
        CMP D
        RNZ
        MOV A, L
        CMP E
        RET

;------------------------------------------------------------------------------
;(HL, DE) 16 bit signed division: HL / DE
;Returns HL=result, DE=remainder, CY=1 if divide by 0 error
IDIVS:  PUSH B
        MVI B, 0        ;Make both args positive
        CALL IABSS      ;  while saving their signs
        JNC IDIVB
        INR B
IDIVB:  XCHG
        CALL IABSS
        JNC IDIVC
        INR B
IDIVC:  XCHG
        MOV A, B
        STA DSIGN       ;Save sign
        LXI B, 0        ;Init quotient = 0
        MOV A, D        ;Divide by 0?
        ADD E
        JNZ IDIV0
        STC             ;  CY=1
        JMP IDIVX
IDIV0:  CALL ICMP       ;Compare values
        JZ IDIVL        ;Is divisor = dividend?
        JC IDIVD        ;Is divisor > dividend?
        MOV A, D        ;Is devisor > 255?
        ORA A
        JNZ IDIVL
        MOV A, E
        CPI 1           ;Is divisor = 1?
        JNZ IDIV2
IDIV1:  LXI D, 0
        JMP IDIVF
IDIV2:  CPI 2           ;Is divisor = 2?
        JNZ IDIV10
        LXI D, 0
        ARHL
        JNC IDIVF
        INR E
        JMP IDIVF
IDIV10: CPI 10          ;Is divisor = 10?
        JNZ IDIVL
        PUSH H          ;  save a copy of dividend
        MOV D, H
        MOV E, L
        ARHL
        XCHG
        ARHL
        ARHL
        DAD D           ;  HL = dividend/2 + dividend/4
        MOV D, H
        MOV E, L
        MVI A, 4
        CALL IDIV2S
        DAD D           ;  HL = HL + HL/16
        MOV D, H
        MOV E, L
        MVI A, 8
        CALL IDIV2S
        DAD D           ;  HL = HL + HL/256
        ARHL
        ARHL
        ARHL
        MOV B, H
        MOV C, L        ;  BC (result) = HL/8
        MOV D, H
        MOV E, L
        DAD H
        DAD H
        DAD D
        DAD H
        MOV D, H
        MOV E, L
        POP H
        CALL ISUB
        XCHG            ;  DE (remainder) = (dividend-HL*4 + HL)*2
        MVI A, 9        ;Is remainder > 9?
        CMP E
        JNC IDIVE
        INX B           ;  Yes, then inc B & remainder -= 10
        MOV A, E
        SUI 10
        MOV E, A
        JMP IDIVE
IDIVL:  CALL ISUB       ;Subtract divisor from dividend
        INX B           ;Increment quotient
        CALL ICMP
        JNC IDIVL
IDIVD:  XCHG            ;Remainder in DE
IDIVE:  MOV H, B        ;Result in HL
        MOV L, C
IDIVF:  LDA DSIGN       ;Get sign
        RAR             ;Is sign negative?
        JNC IDIVX
        CALL INEGS      ;Quotient is negative
        SUB A           ;CY=0
IDIVX:  POP B
        RET
;(HL, A) Divide HL by 2 A times
IDIV2S: ARHL        
        DCR A
        JNZ IDIV2S
        RET


;----------------------------------------------------------------------
;(HL) 16 bit signed integer negation: returns 2's complement of HL
INEGS:  PUSH PSW
        MOV A, H
        CMA
        MOV H, A
        MOV A, L
        CMA
        MOV L, A
        INX H
        POP PSW
        RET

;----------------------------------------------------------------------
;(HL, DE) 16 bit unsigned integer subtraction: HL - DE.  Returns HL
ISUB:   PUSH PSW
        MOV A, L
        SUB E
        MOV L, A
        MOV A, H
        SBB D
        MOV H, A
        POP PSW
        RET

;----------------------------------------------------------------------
;(HL, DE) 16 bit signed integer subtraction
;Returns HL, CY = overflow
ISUBS:  MOV A, H    ;Is HL negative and DE positive?
        RLC
        JNC ISUBNC  ;  No, HL is positive
        MOV A, D
        RLC
        JC ISUBNC   ;  No, DE is negative
        MOV A, L    ;Subtract and test sign of result
        SUB E
        MOV L, A
        MOV A, H
        SBB D
        MOV H, A
        RLC
        JC ISUBX    ;  jump if result negative
        CMC
        RET         ;Result positive means overflow
ISUBNC: MOV A, L    ;Subtract, CY=0
        SUB E
        MOV L, A
        MOV A, H
        SBB D
        MOV H, A
        STC
ISUBX:  CMC         ;CY = 0
        RET






;******************************************************************************

;------------------------------------------------------------------------------
;(@DE) Skip spaces in text pointed to by DE
NOSPC:  LDAX D
        CPI ' '         ;  (where DE->) and return first
        RNZ             ;  non-space char in A
        INX D
        JMP NOSPC

;----------------------------------------------------------------------
;(A) Test for char '0'-'9', returns CY=0 if yes
IS09:   CPI '0'         ;If A is '0'-'9' CY=0
        RC              ;  else CY=1
        CPI ':'
        CMC
        RET

