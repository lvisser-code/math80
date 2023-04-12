;******************************************************************************
; Math routines for 8080 & 8085 microprocessors.  Integer & floating point
; Author - Leonard Visser
;
;Subroutine call parameters shown in ( ).  @RP is a pointer.
;  IABS   (HL) 16 bit signed integer absolute value
;  IADD   (HL, DE) 16 bit signed integer addition: HL + DE. Returns HL, CY
;  IADDU  (HL, DE) 16 bit unsigned integer addition: HL + DE. Returns HL, CY
;  IASC   (@DE) 16 bit signed convert ASCII Decimal number to Integer
;  ICMP   (HL, DE) 16 bit unsigned integer compare: HL - DE
;  IDIV   (HL, DE) 16 bit signed division: HL / DE
;  IMUL   (HL, DE) 16 bit signed integer multiplication: HL * DE
;  INEG   (HL) 16 bit signed integer negation: returns 2's complement of HL
;  IPRINT (HL) Print 16 bit signed integer value as ASCII number
;  IRND   (HL) 8 but integer pseudo-random number generator (1..255)
;  ISUB   (HL, DE) 16 bit signed integer subtraction
;  ISUBU  (HL, DE) 16 bit unsigned integer subtraction: HL - DE.  Returns HL
;******************************************************************************
TEST:   CALL ISUB
        HLT

;Manually relocate these labels
MSG:    EQU 0           ;(@HL) Routine to display string, end with 0
STR1:   EQU 8000        ;String buffer
DSIGN:  DS 1            ;byte data used by IDIV
ISIGN:  DS 1            ;byte data used by IASC
RANDOM: DS 4            ;random number seeds


;-----------------------------------------------------------------------------
;(HL) 16 bit signed integer absolute value.
IABS:   MOV A, H        ;Check the sign
        ORA A
        RP              ;Done if positive, CY=0
        CALL INEG       ;Form 2's complement
        RET

;------------------------------------------------------------------------------
;(HL, DE) 16 bit signed integer addition: HL + DE. Returns HL, CY = overflow
IADD:   MOV A, H        ;HL and DE both positive?
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
;(HL, DE) 16 bit unsigned integer addition: HL + DE. Returns HL, CY = overflow
IADDU:  MOV A, L
        ADD E
        MOV L, A
        MOV A, H
        ADC D
        MOV H, A
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
        CALL INEG       ;Negative so form 2's complement
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
;Returns HL=result, DE=remainder, CY=1 if divide by 0 error.  Uses DSIGN
IDIV:   PUSH B
        MVI B, 0        ;Make both args positive
        CALL IABS       ;  while saving their signs
        JNC IDIVB
        INR B
IDIVB:  XCHG
        CALL IABS
        JNC IDIVC
        INR B
IDIVC:  XCHG
        MOV A, B
        STA DSIGN       ;Save sign
        LXI B, 0        ;Init quotient = 0
        MOV A, D        ;Divide by 0?
        ADD E
        JNZ IDIV1
        STC             ;  CY=1
        JMP IDIVX
IDIV1:  CALL ICMP       ;Compare values
        JZ IDIVL        ;Is divisor = dividend?
        JC IDIVD        ;Is divisor > dividend?
        MOV A, D        ;Is devisor > 255?
        ORA A
        JNZ IDIVL
        MOV A, E
        CPI 1           ;Is divisor = 1?
        JNZ IDIV2
        LXI D, 0        ;  yes, then accelerate
        JMP IDIVF
IDIV2:  CPI 2           ;Is divisor = 2?
        JNZ IDIV10
        LXI D, 0        ;  yes, then accelerate
        CALL ARHL       ;  shift HL right (HL/2)
        JNC IDIVF
        INR E
        JMP IDIVF
IDIV10: CPI 10          ;Is divisor = 10?
        JNZ IDIVL
        PUSH H          ;  yes, then accelerate
        MOV D, H
        MOV E, L
        CALL ARHL
        XCHG
        CALL ARHL
        CALL ARHL
        DAD D           ;  HL = dividend/2 + dividend/4
        MOV D, H
        MOV E, L
        MVI C, 4
IDIV3:  CALL ARHL
        DCR C
        JNZ IDIV3
        DAD D           ;  HL = HL + HL/16
        MOV D, H
        MOV E, L
        MVI C, 8
IDIV4:  CALL ARHL
        DCR C
        JNZ IDIV4
        DAD D           ;  HL = HL + HL/256
        CALL ARHL
        CALL ARHL
        CALL ARHL
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
        CALL INEG       ;Quotient is negative
        SUB A           ;CY=0
IDIVX:  POP B
        RET
;(HL) Rotate HL right 1 bit position, CY = LSB
ARHL:   SUB A           ;clear CY flag
        MOV A, H        ;rotate H
        RAR
        MOV H, A
        MOV A, L        ;rotate L
        RAR
        MOV L, A
        RET

;----------------------------------------------------------------------
;(HL, DE) 16 bit signed integer multiplication: HL * DE
;Returns HL, CY = 1 if overflow
IMUL:  PUSH B
        PUSH D
        LXI B, 8        ;B = 0 (sign), C = 8 (loop counter)
        CALL IABS       ;Make both args positive while
        JNC IMUL1       ;  saving their signs in B
        INR B
IMUL1:  XCHG
        CALL IABS
        JNC IMUL2
        INR B
IMUL2:  CALL ICMP       ;Compare HL&DE
        JC IMUL3        ;If HL is larger, then exchange
        XCHG            ;DE = multiplicand, HL = multiplier
IMUL3:  SUB A           ;H must be = 0, else overflow
        CMP H
        JNZ IMULX       ;Exit with CY=1 if overflow
        MOV A, L        ;Let A = multipler
        LXI H, 0        ;Init product = 0
IMUL4:  DAD H           ;Rotate product left
        RAL             ;Rotate multiplier left into CY
        JNC IMUL5
        DAD D           ;Add multiplicand to partial sum
        JC IMULX        ;Exit with CY=1 if overflow
IMUL5:  DCR C           ;Next
        JNZ IMUL4
        MOV A, B        ;If sign of 1 arg was negative
        RAR
        JNC IMULX
        CALL INEG       ;  then product is negative
        SUB A           ;CY=0
IMULX:  POP D
        POP B
        RET

;----------------------------------------------------------------------
;(HL) 16 bit signed integer negation: returns 2's complement of HL
INEG:   PUSH PSW
        MOV A, H
        CMA
        MOV H, A
        MOV A, L
        CMA
        MOV L, A
        INX H
        POP PSW
        RET

;------------------------------------------------------------------------------
;(HL) Print 16 bit signed integer value as ASCII number between -32768 to 32767
IPRINT: CALL IPSTR1     ;Convert integer value to ASCII string
        LXI H, STR1+1
        CALL MSG        ;Display the string
        RET
;Convert integer value to ASCII string. Save in STR1 with format:
;  length|string|0.
IPSTR1: PUSH B
        PUSH D
        LXI B, 0        ;B=SIGN, C=digit counter
        MOV A, H        ;If integer is negative..
        ANI 80H
        JZ IPSTR2
        MVI B, '-'      ;  then SIGN = '-'
        CALL IABS       ;  HL = |HL|
IPSTR2: LXI D, 10
        CALL IDIV       ;(HL) ret HL / 10
        PUSH D          ;E = digit, stack it
        INR C           ;Inc digit counter
        MOV A, L        ;Loop until HL = 0
        ORA H
        JNZ IPSTR2

        LXI H, STR1     ;Write string to STR1
        MOV M, C        ;Write length
        MOV A, B        ;Write '-' if negative
        ORA A
        JZ IPSTR3
        MOV A, C
        INR A
        MOV M, A
        INX H
        MVI M, '-'
IPSTR3: INX H
        POP D           ;Unstack digit
        MOV A, E
        ADI '0'         ;Convert to ASCII
        MOV M, A        ;Write to STR1
        DCR C
        JNZ IPSTR3      ;loop till digit counter=0
        INX H
        SUB A
        MOV M, A        ;Write terminating 0
        POP D
        POP B
        RET

;------------------------------------------------------------------------------
;(HL) Integer pseudo-random number generator (1..255)
;Call with HL = upper value, returns random in L
IRND:   PUSH B
        PUSH D
        MOV A, H        ;HL cannot be 0
        ADD L
        JZ IRNDX
        XCHG            ;Save upper in DE
        LXI H, RANDOM+3 ;HL=ptr to seed data
        MVI B, 8        ;Loop count
        MVI A, 241Q
IRNDL:  RLC
        RLC
        RLC
        XRA M
        RAL
        RAL
        DCR L
        DCR L
        DCR L
        MOV A, M
        RAL
        MOV M, A
        INR L
        MOV A, M
        RAL
        MOV M, A
        INR L
        MOV A, M
        RAL
        MOV M, A
        INR L
        MOV A, M
        RAL
        MOV M, A
        DCR B
        JNZ IRNDL
        MOV L, A        ;HL = result
        MVI H, 0        ;DE = upper limit
        CALL IDIV       ;DE = remainder of result/limit
        INX D           ;+1
        XCHG
IRNDX:  POP D
        POP B
        RET

;----------------------------------------------------------------------
;(HL, DE) 16 bit signed integer subtraction (HL - DE)
;Returns HL, CY=1 if overflow
ISUB:   MOV A, H    ;Is HL negative and DE positive?
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

;----------------------------------------------------------------------
;(HL, DE) 16 bit unsigned integer subtraction: HL - DE.  Returns HL
ISUBU:  PUSH PSW
        MOV A, L
        SUB E
        MOV L, A
        MOV A, H
        SBB D
        MOV H, A
        POP PSW
        RET

;------------------------------------------------------------------------------
;(@DE) Skip spaces in text pointed to by DE
NOSPC:  LDAX D
        CPI ' '         ;  (where DE->) and return first
        RNZ             ;  non-space char in A
        INX D
        JMP NOSPC

;------------------------------------------------------------------------------
;(A) Test for char '0'-'9', returns CY=0 if yes
IS09:   CPI '0'         ;If A is '0'-'9' CY=0
        RC              ;  else CY=1
        CPI ':'
        CMC
        RET

