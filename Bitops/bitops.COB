       IDENTIFICATION DIVISION.
       PROGRAM-ID. mf-bitwise-ops.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  result                  USAGE BINARY-LONG.

       78  arg-len                 VALUE LENGTH OF result.
       01  a value 2 usage binary-long.
       01  b value 5 usage binary-long .

       LINKAGE SECTION.

       PROCEDURE DIVISION.
       main-line.
           MOVE b TO result
           CALL "CBL_AND" USING a, result, VALUE arg-len
           DISPLAY "a and b is " result

           MOVE b TO result
           CALL "CBL_OR" USING a, result, VALUE arg-len
           DISPLAY "a or b is " result

           MOVE a TO result
           CALL "CBL_NOT" USING result, VALUE arg-len
           DISPLAY "Not a is " result

           MOVE b TO result
           CALL "CBL_XOR" USING a, result, VALUE arg-len
           DISPLAY "a exclusive-or b is " result

           MOVE b TO result
           CALL "CBL_EQ" USING a, result, VALUE arg-len
           DISPLAY "Logical equivalence of a and b is " result

           MOVE b TO result
           CALL "CBL_IMP" USING a, result, VALUE arg-len
           DISPLAY "Logical implication of a and b is " result

           GOBACK.
