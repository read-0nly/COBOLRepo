Useful Links

https://www.mainframestechhelp.com/tutorials/cobol/

https://www.tutorialspoint.com/compile_cobol_online.php



Useful snip
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
