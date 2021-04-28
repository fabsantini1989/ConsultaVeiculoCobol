       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONSCAR.
      *AUTHOR. FABIANO SANTINI MARQUES.
      **************************************
      * CONSULTA DE VEICULOS *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCAR ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS PLACA
                    FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCAR
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCAR.DAT".
       01 REGCAR.
          03 PLACA              PIC X(07).
          03 MARCA              PIC X(12).
          03 MODELO             PIC X(14).
	      03 ANO                PIC 9(04).
          03 PROPR              PIC X(38).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-SEL             PIC 9(01) VALUE ZEROS.
       01 W-CONT            PIC 9(06) VALUE ZEROS.
       01 W-OPCAO           PIC X(01) VALUE SPACES.
       01 ST-ERRO           PIC X(02) VALUE "00".
       01 W-ACT             PIC 9(02) VALUE ZEROS.
       01 MENS              PIC X(50) VALUE SPACES.
       01 LIMPA             PIC X(55) VALUE SPACES.
       01 SOLIC             PIC X(20) VALUE SPACES.
       01 CONLIN            PIC 9(03) VALUE 000.
       01 PLACAENTR         PIC 9(09) VALUE ZEROS.
      *-----------------------------------------------------------------
      *
      *************************
      * REPORT SECTION *
      *************************
      *
       01 DETALHE.
          03 DETPLACA        PIC X(07) VALUE SPACES.
          03 FILLER          PIC X(01) VALUE SPACES.
          03 DETMARCA        PIC X(12) VALUE SPACES.
          03 FILLER          PIC X(01) VALUE SPACES.
          03 DETMODELO       PIC X(14) VALUE SPACES.
          03 FILLER          PIC X(01) VALUE SPACES.
          03 DETANO          PIC 9(04) VALUE ZEROS.
          03 FILLER          PIC X(02) VALUE SPACES.
          03 DETPROPR        PIC X(38) VALUE SPACES.
      *
      ******************
      * ROTINA DE TELA *
      ******************
      *	 
       SCREEN SECTION.
       01  TELACAR2.
           05  BLANK SCREEN.	   
           05  LINE 01  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 01  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 02  COLUMN 01 
               VALUE  "------------------------------ CADASTRO".
           05  LINE 02  COLUMN 41 
               VALUE  "VEICULAR -------------------------------".
           05  LINE 03  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 03  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 04  COLUMN 01 
               VALUE  " PLACA     MARCA         MODELO      ANO".
           05  LINE 04  COLUMN 41 
               VALUE  "   PROPRIETARIO".
           05  LINE 05  COLUMN 01 
               VALUE  "------- ------------ -------------- ----".
           05  LINE 05  COLUMN 41 
               VALUE  "- --------------------------------------".
           05  LINE 15  COLUMN 41 
               VALUE  "                  ________".
           05  LINE 16  COLUMN 41 
               VALUE  "                 //   ll \\".
           05  LINE 17  COLUMN 41 
               VALUE  "               //     ll  \\".
           05  LINE 18  COLUMN 41 
               VALUE  "          (---ll------ll---ll----o".
           05  LINE 19  COLUMN 41 
               VALUE  "          )   __             __   \".
           05  LINE 20  COLUMN 41 
               VALUE  "          l__/  \___________/  \__l".
           05  LINE 21  COLUMN 41 
               VALUE  "             \__/           \__/".
           05  LINE 22  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 22  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 23  COLUMN 01 
               VALUE  "MENSAGEM:".
           05  TPLACAENTR
               LINE 06  COLUMN 01  PIC X(07)
               USING  PLACAENTR.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP1.
           OPEN INPUT  CADCAR
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE CADCAR NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO CADCAR"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.
      *
       INC-001.
           MOVE ZEROS TO PLACAENTR
           DISPLAY  TELACAR2.	  
       INC-001A.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA"
           ACCEPT TPLACAENTR
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE CADCAR
                      GO TO ROT-FIM.
           IF W-ACT > 02
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE PLACAENTR TO PLACA
           START CADCAR KEY IS NOT LESS PLACA INVALID KEY
                 MOVE "*** PLACA NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001A.
           GO TO INC-RD2.
       INC-RD2.
           DISPLAY (23, 13) LIMPA
           READ CADCAR NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADCAR"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.
       ROT-MONTAR.
           MOVE PLACA    TO DETPLACA
           MOVE MARCA    TO DETMARCA
           MOVE MODELO   TO DETMODELO
           MOVE ANO      TO DETANO
           MOVE PROPR    TO DETPROPR

           COMPUTE LIN = CONLIN + 5
           DISPLAY (LIN, 01) DETALHE
           ADD 1 TO CONLIN
           IF CONLIN < 16
                  GO TO INC-RD2.
      *
       ROT-SOL.
           DISPLAY (23, 13) " *** CONTINUA CONSULTA : . ***"
           MOVE "S" TO W-OPCAO
           ACCEPT  (23, 38) W-OPCAO  WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 0 TO CONLIN
                  DISPLAY TELACAR2
                  GO TO INC-RD2.
           IF W-OPCAO NOT = "N"
                  MOVE "*** DIGITE S=SIM   N=NAO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-SOL.
           MOVE 0 TO CONLIN.
           GO TO INC-001.	
      *
       NOV-SOL.
           DISPLAY (23, 13) " *** NOVA CONSULTA : . ***"
           MOVE "S" TO W-OPCAO
           ACCEPT  (23, 34) W-OPCAO WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 01 TO CONLIN
                  GO TO INC-001.
           IF W-OPCAO NOT = "N"
                  MOVE "*** DIGITE S=SIM   N=NAO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO NOV-SOL.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *	
       ROT-FIM.
           DISPLAY (01 01) ERASE.
           CLOSE CADCAR.
       ROT-FIM2.
           EXIT PROGRAM.
       ROT-FIM3.
           STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *	  
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (23, 13) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 1000
              GO TO ROT-MENS2
           ELSE
              DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-MENS.
      *

      *---------------------*** FIM DE PROGRAMA ***--------------------*	  