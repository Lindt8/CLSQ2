      PROGRAM CLSQ
C     A PROGRAM FOR THE ANALYSIS OF DECAY CURVES                                
C     COPYRIGHT 1984 BROOKHAVEN NATIONAL LABORATORY.                            
C     PRIMARY ORIGINATOR, J. B. CUMMING.                                        
C     VERSION 1.00  07/19/84  FOR PDP-11/23
C     REVISED TREATMENT OF PARENT-DAUGHTER PAIRS.                               
C     WITH REVISED RJT ROUTINE. - CAUSES OLD ROUTINE TO BE USED.
       CHARACTER*24 INFN(30), OUTF
      COMMON B(10,10),EL(10,10)                                                 
      COMMON T(201),F(201),SFSQ(201),NAME(40),H(10),D(10),              
     1A(200,10),APF(10),FCALC(200),       SF(200),SIGMAH(10),X(10),             
     2SIGMAX(10),SIGMAD(10),DELTAD(10,9),DDELTA(10,9),EXPDTO(10),               
     3XEOB(10),SXEOB(10),UNITK(10),JUNIT(10),FKCS(5),SFKCS(5)                   
      COMMON HL(10),LUNIT(10),CEF(10),DL(10),FACT1(10),UNITL(10)                
      COMMON FACT2(10)                                                          
      DIMENSION BADT(200),BADFS(200),BINVRS(10,10),RATIO(200),V(200)            
      EQUIVALENCE(BADT(1),SF(1)),(BADFS(1),V(1)),(BINVRS(1,1),B(1,1))           
      EQUIVALENCE (RATIO(1),B(1,1)),(V(1),DELTAD(1,1))                          
      INTEGER HR,DAY,YR,YES*4,NO*4                                                     
      BYTE DAT(20)
      DATA HR,DAY,YR,NO,YES/1HH,1HD,1HY,4H NON,4H    /

      print *,'1 CLSQ V1.0-1 FOR PDP 11/23'
C 1000 FORMAT('1 CLSQ V1.0-1 FOR PDP 11/23')
      print *,'ENTER INPUT FILE SPECIFICATIONS-'
C 1001 FORMAT('$ ENTER INPUT FILE SPECIFICATIONS-')
      read 1002, INFN(I)
 1002 FORMAT(24A)
      CLOSE(UNIT=1)
      OPEN(UNIT = 1, FILE = INFN(I), STATUS = 'UNKNOWN')
      PRINT *,'NAME OF OUTPUT FILE'
      READ 1002, OUTF
      CLOSE(UNIT=6)
c      OPEN(UNIT=6,FILE='TT1:',STATUS='NEW')
       OPEN(UNIT = 6, FILE = OUTF, STATUS = 'UNKNOWN')
C      CALL TIME(DAT(1))
      DAT(10)=1H  
C      CALL DATE(DAT(11))
C     ENTRANCE TO PROGRAM                                                       
 1    LT1=0                                                                     
      TBOM=0.0
      READ(1,4,END=999)NAME                                                             
 4    FORMAT(40A2)                                                              
    7 READ(1,8)NC,NV,NCNV,CNV,BGD,SBGD,IN,IT,BLOCK,SCOFF,RJT,KCS,YIELD
    8 FORMAT(2I3,I1,F4.2,2F6.2,2I2,3F3.0,I2,F6.3)                               
      DO22I=1,NC                                                                
      READ(1,9)H(I),JUNIT(I),HL(I),LUNIT(I),CEF(I),TBOMT                        
 9    FORMAT(F7.0,A1,F7.0,A1,F6.3,F6.2)                                         
      IF(TBOMT.NE.0.0) TBOM=TBOMT                                               
C     HALF LIFE INPUT CONVERSION.  H,D, OR Y ARE RECOGNIZED AS SUCH.            
C     OTHER CHARACTERS ARE TAKEN TO MEAN M.                                     
      UNITK(I)=0.69314718                                                       
      IF(JUNIT(I).EQ.HR) UNITK(I)=0.11552453E-1                                 
      IF(JUNIT(I).EQ.DAY) UNITK(I)=0.48135221E-3                                
      IF(JUNIT(I).EQ.YR) UNITK(I)=0.13178987E-5                                 
      D(I)=UNITK(I)/H(I)                                                        
      IF(HL(I).EQ.0.) GO TO 21                                                  
      UNITL(I)=0.69314718                                                       
      IF(LUNIT(I).EQ.HR) UNITL(I)=0.11552453E-1                                 
      IF(LUNIT(I).EQ.DAY) UNITL(I)=0.48135221E-3                                
      IF(LUNIT(I).EQ.YR) UNITL(I)=0.13178987E-5                                 
      DL(I)=UNITL(I)/HL(I)                                                      
      DFACT=DL(I)/(DL(I)-D(I))                                                  
      FACT1(I)=CEF(I)+DFACT                                                     
      FACT2(I)=-DFACT                                                           
      IF(TBOM.EQ.0.0) GO TO 20                                                  
      EXP1=EXP(-D(I)*TBOM)                                                      
      EXP2=EXP(-DL(I)*TBOM)                                                     
      GFACT=((1.0-EXP2)-DFACT*(EXP1-EXP2))/(1.0-EXP1)                           
      FACT2(I)=FACT2(I)+GFACT                                                   
 20   FACT2(I)=FACT2(I)/FACT1(I)                                                
      GO TO 22                                                                  
 21   FACT2(I)=0.0                                                              
 22   CONTINUE                                                                  
      NP=0                                                                      
C     INPUT OPTION                                                              
      IF(IN.EQ.0) GO TO 31                                                      
      CALL DATAIN(NP,BGD,IN,DAT)                                                    
      GOTO35                                                                    
 31   NP=NP+1                                                                   
      READ(1,33)T(NP),F(NP),SF(NP)                                              
   33 FORMAT(3E10.3)                                                            
      SFSQ(NP)=SF(NP)**2                                                        
      IF(SF(NP).NE.0.) GO TO 31                                                 
C     TIME NORMALIZATION                                                        
 35   TO=T(NP)                                                                  
      NP=NP-1                                                                   
      IF(NP.NE.0) GO TO 34                                                      
      WRITE(6,37)                                                               
 37   FORMAT(1H0'NO DATA POINTS FOUND, GO TO NEXT PROBLEM')                     
      GO TO 1                                                                   
 34   DO 36I=1,NP                                                               
      T(I)=T(I)-TO                                                              
 36   CONTINUE                                                                  
      TO=T(1)                                                                   
      IF(BLOCK.EQ.0.) GO TO 39                                                  
C     DEAD TIME CORRECTION                                                      
      DEADT=BLOCK*(1.67E-8)                                                     
      DO38I=1,NP                                                                
      F(I)=F(I)/(1.-DEADT*F(I))                                                 
 38   CONTINUE                                                                  
 39   IF(BGD.LE.0.) GO TO 42                                                    
      SBGDSQ=SBGD**2                                                            
      DO41I=1,NP                                                                
      F(I)=F(I)-BGD                                                             
 41   SFSQ(I)=SFSQ(I)+SBGDSQ                                                    
 42   IF(YIELD.EQ.0.) GO TO 421                                                 
      YLDSQ=YIELD**2                                                            
      DO411I=1,NP                                                               
      F(I)=F(I)/YIELD                                                           
      SFSQ(I)=SFSQ(I)/YLDSQ                                                     
 411  CONTINUE                                                                  
C     SUBROUTINE FOR SIGMA CUTOFF                                               
 421  IF(SCOFF.EQ.0.) GO TO 44                                                  
      SCOFSQ=SCOFF**2/10000.                                                    
      DO43I=1,NP                                                                
      TSFSQ=SCOFSQ*F(I)**2                                                      
      IF(SFSQ(I).LT.TSFSQ) SFSQ(I)=TSFSQ                                        
 43   CONTINUE                                                                  
C     KNOWN COMPONENT SUBTRACTION (KCS) ROUTINE                                 
 44   IF(KCS.EQ.0) GO TO 47                                                     
      READ(1,45)(FKCS(I),SFKCS(I),I=1,KCS)                                      
 45   FORMAT(2E13.5)                                                            
      DO46J=1,KCS                                                               
      ND=NC-KCS+J                                                               
      DO46I=1,NP                                                                
      FACTOR=EXP ((TO-T(I))*D(ND))                                              
      F(I)=F(I)-FKCS(J)*FACTOR                                                  
 46   SFSQ(I)=SFSQ(I)+(SFKCS(J)*FACTOR)**2                                      
      NC=NC-KCS                                                                 
C                                                                               
C     INITIALIZATION AND RETURNS                                                
 47   NITER=-1                                                                  
      IF(NV.EQ.0) GO TO 51                                                      
      LT1=1                                                                     
      NVTEMP=NV                                                                 
      NV=0                                                                      
C     RETURN AFTER FIXED HALF LIFE PASS                                         
 51   NT=NC+NV                                                                  
      LV=NC+1                                                                   
C                                                                               
C     CALCULATION OF A MATRIX                                                   
C                  ALSO RETURN FOR NEXT ITERATION                               
 55   NITER=NITER+1                                                             
      DO65I=1,NP                                                                
      DO61J=1,NC                                                                
      DT=-D(J)*(T(I)-TO)                                                        
      IF(DT.GT.-34.5) GO TO 60                                                  
      A(I,J)=0.0                                                                
      GO TO 611                                                                 
 60   A(I,J)=EXP (DT)                                                           
 611  IF(HL(J).EQ.0.) GO TO 61                                                  
      IF(FACT2(J).EQ.0.0) GO TO 61                                              
      DLT=-DL(J)*T(I)                                                           
      IF(DLT.LT.-34.5) GO TO 61                                                 
      A(I,J)=A(I,J)+FACT2(J)*EXP(DLT)*EXP(D(J)*TO)                              
 61   CONTINUE                                                                  
      IF(NV.LE.0) GO TO 65                                                      
      DO64J=LV,NT                                                               
      K=J-NC                                                                    
      A(I,J)=(T(I)-TO)*A(I,K)                                                   
 64   CONTINUE                                                                  
 65   CONTINUE                                                                  
C                                                                               
C     CONSTRUCT MATRIX APF                                                      
C                  ALSO RETURN AFTER DATA REJECTION                             
 77   DO80I=1,NT                                                                
      APF(I)=0.0                                                                
      DO80K=1,NP                                                                
      APF(I)=APF(I)+A(K,I)*F(K)/SFSQ(K)                                         
 80   CONTINUE                                                                  
C                                                                               
C     CONSTRUCT MATRIX B                                                        
      DO88I=1,NT                                                                
      DO88J=1,NT                                                                
      B(I,J)=0.0                                                                
      DO88K=1,NP                                                                
      B(I,J)=B(I,J)+A(K,I)*A(K,J)/SFSQ(K)                                       
 88   CONTINUE                                                                  
C                                                                               
C     B MATRIX INVERSION                                                        
      CALL BNSINV(NT,DETERM,0.,ITEST)                                           
C     B MATRIX IS NOW REPLACED BY ITS INVERSE, BINVRS.                          
      IF(ITEST.NE.0) GO TO 211                                                  
C                                                                               
C     SOLUTION OF NORMAL EQUATIONS                                              
      DO108I=1,NT                                                               
      X(I)=0.0                                                                  
      DO108J=1,NT                                                               
      X(I)=X(I)+BINVRS(I,J)*APF(J)                                              
 108  CONTINUE                                                                  
C                                                                               
C     ENTRANCE TO CALCULATIONS FOR OUTPUT(127) OR ITERATION TESTS(301)          
      IF(NV.EQ.0) GO TO 127                                                     
C                                                                               
C     TESTS FOR COMPLETION OF ITERATION                                         
  301 NON=NO                                                                    
      DO306I=1,NV                                                               
      L=I+NC                                                                    
      DELTAD(I,NITER)=-X(L)/X(I)                                                
 305  IF(ABS (DELTAD(I,NITER)).LT.0.5*ABS (D(I))) GO TO 306                     
C     CHANGE IN THE DECAY CONSTANT CANNOT EXCEED HALF ITS VALUE                 
      DELTAD(I,NITER)=0.75*DELTAD(I,NITER)                                      
      GO TO 305                                                                 
 306  D(I)=D(I)+DELTAD(I,NITER)                                                 
      IF(NITER.LE.2) GO TO 55                                                   
      IF(NCNV.GT.0) GO TO 311                                                   
      DO310I=1,NV                                                               
      IF(ABS (DELTAD(I,NITER)).LE.ABS (DELTAD(I,NITER-1))) GO TO 310            
      IF(ABS (DELTAD(I,NITER-1)).GT.ABS (DELTAD(I,NITER-2))) GO TO 312          
 310  CONTINUE                                                                  
  311 NON=YES                                                                   
 312  DO314I=1,NV                                                               
      L=I+NC                                                                    
      SIGMAD(I)=ABS (DELTAD(I,NITER))*SQRT(BINVRS(I,I)/X(I)**2+BINVRS(L,        
     1L)/X(L)**2-2.*BINVRS(I,L)/(X(I)*X(L)))                                    
 314  CONTINUE                                                                  
      IF(NON.EQ.NO) GO TO 127                                                   
      IF(NITER.GE.9) GO TO 127                                                  
      DO319I=1,NV                                                               
      IF(ABS (DELTAD(I,NITER)).LT.CNV*SIGMAD(I)) GO TO 319                      
C     NOT YET DONE. TRY AGAIN IF NITER LT 9.                                    
      IF(NITER.LT.9) GO TO 55                                                   
      NON=NO                                                                    
      GO TO 127                                                                 
 319  CONTINUE                                                                  
C                                                                               
C     CALCULATIONS FOR OUTPUT DATA                                              
 127  DO130I=1,NC                                                               
      SIGMAH(I)=0.0                                                             
 130  EXPDTO(I)=EXP(D(I)*TO)                                                    
C     HALF LIFE OUTPUT CONVERSION                                               
      IF(NV.EQ.0) GO TO 211                                                     
      DO145I=1,NV                                                               
      H(I)=UNITK(I)/D(I)                                                        
 145  SIGMAH(I)=H(I)*SIGMAD(I)/D(I)                                             
C                                                                               
C     PRINT OUT RESULTS                                                         
 211  WRITE (6,2) DAT
 2    FORMAT('1 PDP 11/23 CLSQ V1.0-1',7X,20A1)
      WRITE (6,5) NAME
    5 FORMAT(1H0,40A2)                                                          
  210 IF(ITEST.EQ.0) GO TO 212                                                  
      WRITE (6,511)
 511  FORMAT(51H0MATRIX INVERSION HAS FAILED. MORE DATA ARE NEEDED.)            
      GO TO 1                                                                   
 212  WRITE (6,213) NP,NC,NV,CNV,BGD,SBGD,BLOCK,SCOFF,RJT,KCS,YIELD
  213 FORMAT(//4H NP=,I3,4H NC=,I2,4H NV=,I1,5H CNV=,F4.2,5H BGD=,F6.2,6        
     1H SBGD=,F5.2,7H BLOCK=,F4.1,7H SCOFF=,F3.1,5H RJT=,F4.1,5H KCS=,I2        
     2,7H YIELD=,F6.3)                                                          
      IF(NV.EQ.0) GO TO 2331                                                    
      WRITE (6,217) NITER,NON
 217  FORMAT(//25H    ITERATIONS PERFORMED=,I2,A4,10HCONVERGENT)                
      WRITE (6,222)
 222  FORMAT(//78H              1ST COMP      2ND COMP      3RD COMP            
     1  4TH COMP      5TH COMP)                                                 
      WRITE (6,227) (D(I),I=1,NV)
 227  FORMAT(10H    D     ,5E14.6)                                              
      DO231J=1,NITER                                                            
      WRITE (6,230) J,(DELTAD(I,J),I=1,NV)
 230  FORMAT(7H DELTA(,I2,1H),5E14.6)                                           
 231  CONTINUE                                                                  
      WRITE (6,233) (SIGMAD(I),I=1,NV)
 233  FORMAT(10H  SIGMA   ,5E14.6)                                              
C     CONVERSION TO END TIME.                                                   
 2331 DO2332I=1,NC                                                              
      XEOB(I)=X(I)*EXPDTO(I)                                                    
      SXEOB(I)=BINVRS(I,I)                                                      
      IF(I.GT.NV) GO TO 2332                                                    
      L=I+NC                                                                    
      SXEOB(I)=SXEOB(I)+2.*TO*BINVRS(I,L)+TO**2*BINVRS(L,L)                     
 2332 SXEOB(I)=SQRT(SXEOB(I))*EXPDTO(I)                                         
 234  WRITE (6,235)
 235  FORMAT(//68H          HALF LIFE  SIGMA H   CPM AT END     SIGMA           
     1  DECAY FACTOR)                                                           
      DO 266 I=1,NC                                                             
      WRITE (6,238) I,H(I),JUNIT(I),SIGMAH(I),
     1JUNIT(I),XEOB(I),SXEOB(I),EXPDTO(I)                                       
  238 FORMAT(6H COMP(,I2,2H) ,F8.3,A1,F8.3,A1,3E13.4)                           
      IF(HL(I).EQ.0.0) GO TO 266                                                
      WRITE (6,264) HL(I),LUNIT(I),CEF(I),TBOM,FACT1(I)
 264  FORMAT(15H          WITH ,F8.3,A1,17H DAUGHTER. C1/C2=,F6.3,7H, LB        
     1OM=,F6.2,10HM, FACTOR=,F6.3)                                              
 266  CONTINUE                                                                  
      IF(KCS.EQ.0) GO TO 239                                                    
      I=NC                                                                      
      J=0                                                                       
 2382 I=I+1                                                                     
      J=J+1                                                                     
      WRITE (6,2383) I,H(I),JUNIT(I),FKCS(J),SFKCS(J)
 2383 FORMAT(6H COMP(,I2,2H) ,F8.3,A1,9X,2E13.5,17H  KNOWN COMPONENT)           
      IF(KCS.GT.J) GO TO 2382                                                   
 239  VPV=0.0                                                                   
      DO135I=1,NP                                                               
      FCALC(I)=0.0                                                              
      DO131J=1,NC                                                               
      FCALC(I)=FCALC(I)+A(I,J)*X(J)                                             
 131  CONTINUE                                                                  
      V(I)=F(I)-FCALC(I)                                                        
      SF(I)=SQRT (SFSQ(I))                                                      
      RATIO(I)=V(I)/SF(I)                                                       
      VPV=VPV+V(I)**2/SFSQ(I)                                                   
 135  CONTINUE                                                                  
      DF=NP-NT                                                                  
      IF(DF.EQ.0.) DF=1.                                                        
      FIT=SQRT (VPV/DF)                                                         
      WRITE (6,240) FIT
 240  FORMAT(//8H    FIT=,F7.3//)                                               
      WRITE (6,242)
 242  FORMAT(77H       T(I)         F(I)       FCALC(I)       V(I)              
     1 SIGMAF(I)   RATIO(I))                                                    
      WRITE (6,244) (T(I),F(I),FCALC(I),V(I),SF(I),RATIO(I),I=1,NP)
  244 FORMAT(1H ,5E13.4,F9.2)                                                   
      IF(RJT.EQ.0.) GO TO 2457                                                  
      IF(NV.NE.0) GO TO 2457                                                    
C                                                                               
C     DATA REJECTION SUBROUTINE                                                 
      I=0                                                                       
      NRJT=0                                                                    
 2442 I=I+1                                                                     
 2443 IF(I.GT.NP) GO TO 2449                                                    
      IF(ABS (RATIO(I)).LE.ABS (RJT)) GO TO 2442                                
      IF(RJT.LT.0.) GO TO 5010                                                  
      IF(I.EQ.NP) GO TO 5001                                                    
      IF(RATIO(I)*RATIO(I+1))5021,5021,2442                                     
 5021 IF(I.EQ.1) GO TO 5010                                                     
 5001 IF(RATIO(I)*RATIO(I-1))5010,5010,2442                                     
 5010 NRJT=NRJT+1                                                               
      BADT(NRJT)=T(I)                                                           
      BADFS(NRJT)=F(I)                                                          
      NP=NP-1                                                                   
C     MOVE UP REMAINING POINTS                                                  
 2446 DO2448J=I,NP                                                              
      T(J)=T(J+1)                                                               
      F(J)=F(J+1)                                                               
      SFSQ(J)=SFSQ(J+1)                                                         
      RATIO(J)=RATIO(J+1)                                                       
      DO2448K=1,NT                                                              
      A(J,K)=A(J+1,K)                                                           
 2448 CONTINUE                                                                  
      GOTO2443                                                                  
 2449 IF(NRJT.NE.0) GO TO 2452                                                  
      WRITE (6,2451)
 2451 FORMAT(20H0ALL DATA POINTS OK.)                                           
      GOTO2457                                                                  
 2452 WRITE (6,2453) NRJT
 2453 FORMAT(22H0DATA POINTS REJECTED(,I3,18H) ARE GIVEN BELOW.,/22H            
     1BAD T        BAD F)                                                       
      WRITE (6,2455) (BADT(I),BADFS(I),I=1,NRJT)
 2455 FORMAT(1H ,2E12.4)                                                        
      WRITE (6,2456)
 2456 FORMAT(41H0REPEAT CALCULATION WITHOUT THESE POINTS.)
      GOTO77                                                                    
 2457 IF(LT1.NE.1) GO TO 1                                                      
      LT1=0                                                                     
      NV=NVTEMP                                                                 
      GOTO51                                                                    
C 999  CALL EXIT
 999     END
      SUBROUTINE DATAIN(NP,BGD,IN,DAT)                                              
C     REVISED 12/04/75 TO ACCEPT CARDS FROM INTRAL AND SORTIF.                  
      COMMON B(10,10),EL(10,10)                                                 
      COMMON T(201),F(201),SFSQ(201),NAME(40)                           
      BYTE DAT(20)
      CHARACTER*20 IDENT,TYPE*8
      IF(IN.EQ.1) GO TO 1                                                       
      WRITE (6,2) DAT
    2 FORMAT('1 PDP 11/23 CLSQ V1.0-1',7X,20A1)
      WRITE (6,5) NAME
    5 FORMAT(1H0,40A2)                                                          
      WRITE (6,3)
    3 FORMAT('0 INPUT DATA FROM CARDS'/'0  DAY  HR  MIN    COUNT   DELTA        
     1T   BGND  SBGND  SIGPCT  TYPE-FWHM  ENERGY       ID1       ID2            
     1   CPM')                                                                  
    1 NP=NP+1                                                                   
    4 READ(1,18) TD,TH,TM,COUNT,DELTAT,BGND,SBGND,SIGPCT,TYPE,EP,IDENT        
   18 FORMAT(F5.0,F3.0,F6.2,F7.0,F6.0,2F6.2,F4.1,A,F8.1,1X,A)               
      IF(DELTAT.NE.0.) CPM=COUNT/DELTAT                                         
      IF(DELTAT.EQ.0.) CPM=0.                                                   
      IF(IN.LT.2) GO TO 7
      WRITE (6,6) TD,TH,TM,COUNT,DELTAT,BGND,SBGND,SIGPCT,
     1TYPE,EP,IDENT,CPM                                                       
    6 FORMAT(1H ,F5.0,F4.0,F6.2,F9.0,F8.1,2F7.2,F8.1,1X,A,2X,F8.1,2X,          
     1A,F10.2)                                                               
 7    T(NP)=TM+60.*TH+1440.*TD+DELTAT/2.                                        
      IF(DELTAT.EQ.0.) RETURN                                                   
      F(NP)=COUNT/DELTAT                                                        
      IF(SIGPCT.EQ.0.) GO TO 20                                                 
      SFSQ(NP)=((SIGPCT/100.)*F(NP))**2                                         
      GO TO 21                                                                  
   20 SFSQ(NP)=(COUNT+1.)/DELTAT**2                                             
   21 IF(BGD.GE.0.) GO TO 1                                                     
      F(NP)=F(NP)-BGND                                                          
      SFSQ(NP)=SFSQ(NP)+SBGND**2                                                
      GO TO 1                                                                   
      END                                                                       
      SUBROUTINE BNSINV(N,DETERM,EPSIL,ITEST)                                   
C     MATRIX INVERSION SUBROUTINE                                       BNSINV03
C     A IS ORIGINAL MATRIX (REPLACED BY A-INVERSE AT END OF PROGRAM)    BNSINV05
C     EL IS A 10 BY 10 ARRAY USED ONLY FOR WORKING STORAGE              BNSINV06
C     N  IS DIMENSION OF MATRIX                                         BNSINV07
C     DETERM IS LOCATION OF DETERMINANT                                 BNSINV08
C     EPSIL IS USED TO TEST WHETHER MATRIX IS SINGULAR.                 BNSINV09
C           IF,AFTER TRIANGULARIZING THE MATRIX, THE RATIO OF THE       BNSINV10
C           SMALLEST TO LARGEST DIAGONAL ELEMENT IS LESS THEN EPSIL     BNSINV11
C           THE MATRIX IS CONSIDERED SINGULAR. IF ONE SETS EPSIL=0.     BNSINV12
C           THE ROUTINE WILL USE EPSIL=1.E-07                           BNSINV13
C     ITEST IS AN ERROR INDICATOR.                                      BNSINV14
C           IF ITEST = 0  ALL WENT WELL                                 BNSINV15
C           IF ITEST = 1  MATRIX WAS FOUND TO BE SINGULAR               BNSINV16
C           IF ITEST =-1  DIVIDE CHECK OCCURRED                                 
      COMMON A(10,10),EL(10,10)                                                 
      IF(N.GT.1) GO TO 9                                                        
C     SPECIAL CASE FOR N=1                                                      
      A(1,1)=1.0/A(1,1)                                                         
      GOTO125                                                                   
C     SET EL = IDENTITY MATRIX                                          BNSINV22
    9 DO30J=1,N                                                         BNSINV23
      DO30I=1,N                                                         BNSINV24
      IF(I.EQ.J) EL(I,J)=1.0                                            BNSINV25
      IF(I.NE.J) EL(I,J)=0.0                                            BNSINV26
   30 CONTINUE                                                          BNSINV27
C     TRIANGULARIZE A, FORM EL                                          BNSINV35
      N1=N-1                                                            BNSINV36
      M=2                                                               BNSINV37
      DO50J=1,N1                                                        BNSINV38
      DO45I=M,N                                                         BNSINV39
      IF(A(I,J).EQ.0.) GO TO 45                                         BNSINV40
      D=SQRT (A(J,J)**2+A(I,J)**2)                                              
      C=A(J,J)/D                                                        BNSINV42
      S=A(I,J)/D                                                        BNSINV43
      DO39K=J,N                                                         BNSINV44
      D=C*A(J,K)+S*A(I,K)                                               BNSINV45
      A(I,K)=C*A(I,K)-S*A(J,K)                                          BNSINV46
      A(J,K)=D                                                          BNSINV47
   39 CONTINUE                                                          BNSINV48
      DO40K=1,N                                                         BNSINV49
      D=C*EL(J,K)+S*EL(I,K)                                             BNSINV50
      EL(I,K)=C*EL(I,K)-S*EL(J,K)                                       BNSINV51
      EL(J,K)=D                                                         BNSINV52
   40 CONTINUE                                                          BNSINV53
   45 CONTINUE                                                          BNSINV54
   50 M=M+1                                                             BNSINV55
C     CALCULATE DETERMINANT                                             BNSINV56
      DETERP=A(1,1)                                                     BNSINV57
      DO52I=2,N                                                         BNSINV58
   52 DETERP=DETERP*A(I,I)                                              BNSINV59
      DETERM=DETERP                                                     BNSINV60
C     IS MATRIX SINGULAR                                                BNSINV61
      D=A(1,1)                                                                  
      E=A(1,1)                                                          BNSINV63
      DO58I=2,N                                                         BNSINV64
      IF(ABS (D).LT.ABS (A(I,I))) D=A(I,I)                              BNSINV65
      IF(ABS (E).GT.ABS (A(I,I))) E=A(I,I)                                      
   58 CONTINUE                                                          BNSINV69
      EPSILP=EPSIL                                                      BNSINV70
      IF(EPSILP.EQ.0.) EPSILP=1.E-07                                            
      IF(D.EQ.0.) GO TO 130                                             BNSINV73
      IF(ABS (E/D).LT.ABS (EPSILP)) GO TO 130                           BNSINV74
C     INVERT TRIANGULAR MATRIX                                          BNSINV75
      J=N                                                               BNSINV76
      DO100J1=1,N                                                       BNSINV77
      LGT=1                                                             BNSINV78
      I=J                                                               BNSINV79
      DO90I1=1,J                                                        BNSINV80
      IF(LGT.EQ.0) GO TO 75                                             BNSINV81
      LGT=0                                                                     
      IF(A(I,I).EQ.0.0)GOTO140                                          BNSINV82
      A(I,J)=1.0/A(I,I)                                                 BNSINV83
      GOTO90                                                            BNSINV84
   75 KS=I+1                                                            BNSINV85
      D=0.                                                              BNSINV86
      DO80K=KS,J                                                        BNSINV87
   80 D=D+A(I,K)*A(K,J)                                                 BNSINV88
      IF(A(I,I).EQ.0.0)GOTO140                                          BNSINV89
      A(I,J)=-D/A(I,I)                                                  BNSINV90
   90 I=I-1                                                             BNSINV91
  100 J=J-1                                                             BNSINV92
C     PREMULTIPLY EL BY INVERTED TRIANGULAR MATRIX                      BNSINV93
      M=1                                                               BNSINV94
      DO120I=1,N                                                        BNSINV95
      DO118J=1,N                                                        BNSINV96
      D=0.                                                              BN0INV97
      DO107K=M,N                                                        BNSINV98
  107 D=D+A(I,K)*EL(K,J)                                                BNSINV99
      EL(I,J)=D                                                         BNSINV00
  118 CONTINUE                                                          BNSINV01
  120 M=M+1                                                             BNSINV02
C     RECOPY EL TO A                                                            
  123 DO124I=1,N                                                        BNSINV04
      DO124J=1,N                                                        BNSINV05
  124 A(I,J)=EL(I,J)                                                    BNSINV06
  125 ITEST=0                                                           BNSINV07
  126 RETURN                                                            BNSINV10
  130 ITEST=1                                                           BNSINV11
      GOTO126                                                           BNSINV12
  140 ITEST=-1                                                          BNSINV13
      GOTO126                                                           BNSINV17
      END                                                               BNSINV18
