Simple Text Editor
==================


Implementation:
  
  Main idia is state with current text, list of deleted strings, and command log.
  
  When you append you simply add to current text + log ADD command.
  
  When you delete you move deleted symbols from current text to list of deleted strings + log REMOVE command.
  
  When you undo, you either remove from current text or move string from deleted string list to the and of current text + remove from command log.

    + APPEND: "rebrab" ~~> "paradox" ~~> "xodaraprebrab" [Add 7]
    + DELETE: "xodaraprebrab" ~~> delete 7 ~~> "rebrab" deleted:[["paradox"]] [Add 7, Delete 7]
    + UNDO:   "rebrab" [Add 7, Delete 7] ~~> undo ~~> "xodaraprebrab" [Add 7] 


  - Sidenotes:
  
    - In accordance with https://www.hackerrank.com/challenges/simple-text-editor I will assume that all input is valid, therefor no input sanity check is performed

    - Current text in state is reversed to make Delete command faster. 


Testing:

  QuickTest was used to "prove/check":

    - Print and then parse command A equal A  

      - (parse (print A)) == A

    - Concatenation of all string from list of Append commands equal to evalCommnads applyed to the same list of Append commands

      - commands = [(Appned s1), (Appned s2), (Appned s3)] 
      - (s1 ++ s2 ++ s3) == evalCommnads commands

    - Summ of length of all Append string minus summ of all numbers in Delete commands are equal to length of result of evalCommnads applyed to the same list of Append/Delete commands

      - commands = [(Appned s1), (Delete n), (Appned s2)] 
      - (lenght s1) - n + (lenght s1)  == length evalCommnads commands

    -  List of 'Command' without Undo affected commands will be evaluated the same as initial list of 'Commnad'

      - 'evalCommands' [a, b, c, Undo, Undo, d] == 'evalCommands' [a, d]


Building/Running:
  
  In case you do not use "stack", YOU SHOULD :). If you do stop reading.

  Anyway if you do not please install it using https://docs.haskellstack.org/en/stable/README/

  Then inside /yourhome/yourrepo/simpletexteditor run

    stack setup
    
    stack build
    
    stack exec simpletexteditor
    
    tack test
