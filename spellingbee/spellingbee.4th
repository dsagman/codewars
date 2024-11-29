( Totally does not work yet)

\ Define the file name as a constant
: input-file-name  ( -- addr u )
    s" /usr/share/dict/words" ;

\ Define a word to check the length of a string
: long-word? ( addr u -- flag )
    3 > ;

\ Define a word to process each line in the file
: process-line ( addr u -- )
    2dup long-word? if
        type cr
    then ;

\ Define a word to read and process the file
: scan-file ( -- )
    input-file-name r/o open-file throw \ Open the file for reading
    dup >r \ Save file-id on the return stack
    begin
        r@ read-line throw \ Read a line from the file
        dup while          \ Check if there's more to read
            process-line   \ Process the current line
        repeat
    drop                   \ Drop the last line flag
    r> close-file throw ;  \ Close the file

\ Example usage:
\ scan-file
