OPTION EXPLICIT
OPTION DEFAULT NONE
OPTION BASE 0

CONST VERSION$ = "0.1"
CONST DQ$ = CHR$(34) 'Double quote
CONST PAGE_NUM_LINES% = 40
CONST ENABLE_PAGING% = 1 'Page every 40 lines

DIM recursionLevel% = 0
DIM errCode% = 0
DIM strToSearch$, fspec$
DIM startDir$ = CWD$
DIM pageCounter%=0
DIM progressCounter%=0

PRINT
PRINT "xFind Text Search V"+VERSION$+" by Epsilon"
PRINT

IF NOT parseCmdLine%(MM.CMDLINE$, strToSearch$, fspec$) THEN
  usage
  GOTO endProg
ENDIF

IF fspec$="" THEN
  fspec$="."
ENDIF

PRINT "Searching for: "+strToSearch$
PRINT "In: "+fspec$
 
'File spec or directory given?
DIM fileToScan$ = DIR$(fspec$, FILE)

IF fileToScan$<>"" THEN
  'If a subdirectory is specified, cd into there first
  DIM bd$=baseDir$(fspec$)
  IF bd$<>"" THEN
    CHDIR bd$
  ENDIF
  
  DO WHILE fileToScan$ <> ""
    scanFile strToSearch$, fileToScan$
    fileToScan$ = DIR$()
  LOOP
ELSEIF (fspec$=".") OR (DIR$(fspec$, DIR) <> "") THEN
  scanDir(strToSearch$, fspec$)
ENDIF

PRINT @(1) "Done."

endProg:
CHDIR startDir$

IF errCode% <> 0 THEN
  PRINT @(1) "errCode=" errCode%
ENDIF

ON ERROR SKIP 1
CLOSE #1
END

'This function starts an iteration over all sub directories in the current directory.
FUNCTION listDirs$()
  'It can be so easy...
  listDirs$ = DIR$("*", DIR)
END FUNCTION

'This function starts an iteration over all files in the current directory.
FUNCTION listFiles$()
  listFiles$ = DIR$("*.*", FILE)
END FUNCTION

'Extract the base directory portion of a filespec. E.g df/*.INC -> df
FUNCTION baseDir$(fspec$)
  LOCAL dividerPos%=0, prevDividerPos%
  
  baseDir$=""
  
  DO
    prevDividerPos% = dividerPos%
    dividerPos% = INSTR(dividerPos%+1, fspec$, "/")
    IF dividerPos%=0 THEN
      dividerPos% = INSTR(dividerPos%+1, fspec$, "\")
    ENDIF
  LOOP UNTIL dividerPos%=0
  
  IF prevDividerPos%<>0 THEN
    baseDir$ = (LEFT$(fspec$, prevDividerPos%))
  ENDIF
END FUNCTION

SUB scanFile(strToSearch$, filename$)
  'PRINT SPACE$(recursionLevel%*2) "Processing file " filename$
  
  OPEN filename$ FOR INPUT AS #1

  LOCAL line$
  LOCAL lineNbr%=1
  LOCAL k$
  
  'Contents
  DO WHILE NOT EOF(#1)
    ON ERROR SKIP 1
    LINE INPUT #1, line$
    IF MM.ERRNO = 0 THEN
      IF INSTR(UCASE$(line$), UCASE$(strToSearch$)) THEN
        PRINT @(1) CWD$+"/"filename$+" "+STR$(lineNbr%)+": "+line$
        INC pageCounter%
        IF ENABLE_PAGING% AND pageCounter%>=PAGE_NUM_LINES% THEN
          pageCounter%=0
          COLOR RGB(YELLOW)
          PRINT @(1) "Press Q to quit, any other key to continue."
          COLOR RGB(WHITE)
          DO 
            k$ = INKEY$
          LOOP UNTIL k$<>""
          IF UCASE$(k$)="Q" THEN
            GOTO endProg
          ENDIF
        ENDIF
      ELSE
        IF progressCounter% MOD 100 = 0 THEN
          PRINT @(1) "["+STR$(progressCounter%)+"]";
        ENDIF
        INC progressCounter%
      ENDIF
    ENDIF
    ON ERROR CLEAR
    INC lineNbr%
  LOOP

  CLOSE #1
END SUB

'This subroutine processes the contents of given directory
SUB scanDir(strToSearch$, dirToProcess$)
  recursionLevel% = recursionLevel% + 1

  LOCAL dirToProcess_l$ = dirToProcess$

  PRINT @(1) SPACE$(recursionLevel%*2) "Processing dir " dirToProcess_l$
  
  CHDIR dirToProcess_l$

  'Process the files
  LOCAL fileToProcess$ = listFiles$()

  DO WHILE fileToProcess$ <> ""
    scanFile strToSearch$, fileToProcess$
    fileToProcess$ = DIR$()
    IF errCode% <> 0 THEN
      GOTO EndOfProg
    ENDIF
  LOOP

  'Process the subdirs  
  LOCAL subDir$ = listDirs$()

  'DIR$/nextDir$ can't handle recursion in this while loop so we have to build a subDir list  
  LOCAL numSubDirs% = 0

  'First calculate how many subdirs there are in this directory
  DO WHILE subDir$ <> ""
    numSubDirs% = numSubDirs% + 1
    subDir$ = DIR$()
    IF errCode% <> 0 THEN
      GOTO EndOfProg
    ENDIF
  LOOP

  IF numSubDirs% >= 1 THEN
    'Note: The size of this array is too big by 1 entry
    LOCAL subDirList$(numSubDirs%)

    subDir$ = listDirs$()
    LOCAL listIdx% = 0

    DO WHILE subDir$ <> ""
      subDirList$(listIdx%) = subDir$
      subDir$ = DIR$()
      IF errCode% <> 0 THEN
        GOTO EndOfProg
      ENDIF
      listIdx% = listIdx% + 1
    LOOP  

    'Now we recurse. For some reason this doesn't work with a while loop, 
    'but with a for loop it works just fine.
    FOR listIdx%=0 TO numSubDirs%-1
      scanDir strToSearch$, subDirList$(listIdx%)
    NEXT listIdx%
  ENDIF

  CHDIR ".."
  recursionLevel% = recursionLevel% - 1
END SUB

'Return 1 if OK, 0 otherwise.
FUNCTION parseCmdLine%(cmdLine$, strToSearch$, fspec$)
  LOCAL curPos%=1
  LOCAL inWhiteSpace%=1
  LOCAL endPos%
  
  strToSearch$=""
  fspec$=""
  
  DO WHILE (curPos%<=LEN(cmdLine$)) AND ((strToSearch$="") OR (fspec$=""))
    IF inWhiteSpace% THEN
      IF MID$(cmdLine$, curPos%, 1) <> " " THEN
        inWhiteSpace% = 0
      ELSE
        INC curPos%
      ENDIF
    ELSE 'not in_whitespace. 
      IF strToSearch$="" THEN 'We don't have a string to search yet:
        'Should be double quote:
        IF MID$(cmdLine$, curPos%, 1) <> DQ$ THEN
          PRINT "Search string start quote not found."
          parseCmdLine%=0
          EXIT FUNCTION
        ENDIF
                
        INC curPos%
  
        'Find matching quote
        endPos% = INSTR(curPos%, cmdLine$, DQ$)
        IF endPos%=0 THEN
          PRINT "Search string end quote not found."
          parseCmdLine%=0
          EXIT FUNCTION
        ENDIF
        
        strToSearch$ = MID$(cmdLine$, curPos%, endPos%-curPos%)
        curPos% = endPos%+1
        
        'Should be whitespace, or EOL:
        IF (curPos% <= LEN(cmdLine$)) AND (MID$(cmdLine$, curPos%, 1) <> " ") THEN
          PRINT "Expected whitespace after closing quote."
          parseCmdLine%=0
          EXIT FUNCTION
        ENDIF
        
        inWhiteSpace%=1
        
      ELSE 'Looking for a fileSpec
        'Is there a whitespace after the current text block?
        endPos% = INSTR(curPos%, cmdLine$, " ")
        IF endPos%=0 THEN 'interprete up until the end of the line as fspec
          fspec$=MID$(cmdLine$, curPos%)
        ELSE 'interpret up to whitespace as fspec
          fspec$=MID$(cmdLine$, endPos%-curPos%)
        ENDIF
        
        IF fspec$="" THEN
          PRINT "Filespec not found."
          parseCmdLine%=0
          EXIT FUNCTION          
        ENDIF
        
        curPos%=endPos%
      ENDIF
    ENDIF 
  LOOP
  
  IF strToSearch$="" THEN
    PRINT "No string to search argument found."
    parseCmdLine%=0
    EXIT FUNCTION          
  ENDIF
        
  parseCmdLine% = 1
END FUNCTION

SUB usage
  PRINT
  PRINT "Usage:"
  PRINT
  PRINT "*xFind "+DQ$+"<text string>"+DQ$+" <filespec or dir>"
  PRINT
  PRINT "Examples:
  PRINT
  PRINT "Search for text "+DQ$+"SUB foo"+DQ$+" in all .INC files in current directory:" 
  PRINT "*xFind "+DQ$+"SUB foo"+DQ$+" *.INC"
  PRINT 
  PRINT "Search for text "+DQ$+"foo$()"+DQ$+" in directory "+DQ$+"bar"+DQ$+" (and subdirectories):" 
  PRINT "*xFind "+DQ$+"foo$()"+DQ$+" bar"
  PRINT
  PRINT "Search is case insensitive."
  PRINT
END SUB
