      SUBROUTINE EncBeam(val,nx,ny,naok,
     &     rawdata,nhitdata,hitdet,nhitdet,ndet,ndata)
c Modified by abey @ 2016.10.11 for MS03
c-----------------------------------------------------------------------
c ANALYZER 2 : Encbeam
c-----------------------------------------------------------------------
c  for raw data histgrams
c  ID=1 R3 CAMAC #02 C008 ch 0, 1
c  ID=2 R3 CAMAC #02 C008 ch 2, 3
c  ID=3 R3 CAMAC #02 C008 ch 4, 5
c  ID=4 R3 CAMAC #02 C008 ch 6, 7
c  ID=5 R3 CAMAC #04 RPH-022-1 ch 0, 1
c  ID=6 R3 CAMAC #04 RPH-022-1 ch 2, 3
c  ID=7 R3 CAMAC #04 RPH-022-1 ch 4, 5
c  ID=8 R3 CAMAC #04 RPH-022-1 ch 6, 7
c  ID=9 R3 CAMAC #04 RPH-022-1 ch 8, 9
c  ID=10 R3 CAMAC #06 RPH-022-2 ch 0, 1
c  ID=11 R3 CAMAC #14 K3780 ch 0, 1
c  ID=12 R3 CAMAC #14 K3780 ch 2, 3
c  ID=13 R3 CAMAC #14 K3780 ch 4, 5
c  ID=14 R3 CAMAC #14 K3780 ch 6, 7
c  ID=15 R3 CAMAC #16 K3780 ch 0, 1
c  ID=16 R3 CAMAC #16 K3780 ch 2, 3
c  ID=17 R3 CAMAC #16 K3780 ch 4, 5
c  ID=18 R3 CAMAC #16 K3780 ch 6, 7
c  ID=19 R3 CAMAC #08 C008 ch 0, 1
c  ID=20 R3 CAMAC #08 C008 ch 2, 3
c  ID=21 R3 CAMAC #08 C008 ch 4, 5
c  ID=22 R3 CAMAC #08 C008 ch 6, 7
c  ID=23 R3 CAMAC #20 2249W ch 0, 1
c  ID=24 R3 CAMAC #12 RPC-170 ch 0, 1
c  ID=25 R3 CAMAC #12 RPC-170 ch 2, 3
c  ID=26 R3 CAMAC #12 RPC-170 ch 4, 5
c  ID=27 R3 CAMAC #12 RPC-170 ch 6, 7
c  ID=28 BigRIPS CAMAC F2 Pla L 
c  ID=29 BigRIPS CAMAC F2 Pla R
c  ID=30 BigRIPS CAMAC F3 Pla L 
c  ID=31 BigRIPS CAMAC F3 Pla R
c  ID=32 BigRIPS CAMAC F3 Pla CFD-T(L and R) 
c  ID=33 F3 CAMAC #01 AD811 F3 Si
c  ID=34 F3 CAMAC #03 AD811 F2 IC 1, 2
c  ID=35 F3 CAMAC #03 AD811 F2 IC 3, dummy 
c  ID=36 BigRIPS CAMAC F7 Pla LE-T (L and R)
c  ID=37 BigRIPS CAMAC F7 Pla CFD-T (L and R)
c  ID=38 BigRIPS CAMAC F7 IC1,2
c  ID=39 BigRIPS CAMAC F7 IC3,4
c  ID=40 BigRIPS CAMAC F7 IC5,6
c  ID=41 RF
c  ID=42 FH10 VME TDC ch 11, 12 (LE for T)
c  ID=43 FH10 VME TDC ch 141, 142 (TE for Q)
c  ID=44 FH10 VME TDC ch 13, 14 (LE for T)
c  ID=45 FH10 VME TDC ch 143, 144 (TE for Q)
c  ID=46 F3 VME MADC ch 0, 1 (F3IC1, IC2)
c  ID=47 F3 VME MADC ch 2, 3 (F3IC3, IC4)
c  ID=48 F3 VME MADC ch 4, 5 (F3IC5, IC6)

c  for view true histgrams
c-----------------------------------------------------------------------
c  ID=90 PPAC X, Y (F3, F4, F5, F6, FH9, FH10, S0)
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   F31aX F31aY F31bX F31bY F32aX F32aY F32bX F32bY
c
c  W# :  11   12    13    14    15    16    17    18    19    20
c             F51aX F51aY F51bX F51bY F52aX F52aY F52bX F52bY
c
c  W# :  21   22    23    24    25    26    27    28    29    30
c             F4X   F4Y   F6X   F6Y   
c
c  W# :  31   32    33    34    35    36    37    38    39    40
c             FH9X  FH9Y  FHXX  FHXY  S0X   S0Y
c
c-----------------------------------------------------------------------
c  ID=101 RF
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   RF          RF
c             (ch)        (ns)
c-----------------------------------------------------------------------
c  ID=121 F2 plastic
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   F2LT        F2RT        F2LQ        F2RQ  
c             (ch)        (ch)        (ch)        (ch)  
c  W# :  11   12    13    14    15    16    17    18    19    20
c             F2LT        F2RT        F2LQ        F2RQ
c             (ns)        (ns)        (ch)       (ch)
c  W# :  21   22    23    24    25    26    27    28    29    30
c             F2Tsum                              F2Qavr  
c
c-----------------------------------------------------------------------
c  ID=122 F2 IC (Not use)
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   IC1         IC2         IC3
c             (ch)        (ch)        (ch)
c  W# :  11   12    13    14    15    16    17    18    19    20
c             IC1(ped)    IC2(ped)    IC3(ped)
c             (ch)        (ch)        (ch)
c  W# :  21   22    23    24    25    26    27    28    29    30
c             ICavr                   dEcal       
c             (ch)                    (MeV)       
c-----------------------------------------------------------------------
c  ID=131 F3 plastic
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   F3LT  F3RT        F3LQ  F3RQ        F3LTC F3RTC
c             (ch)  (ch)        (ch)  (ch)        (ch)  (ch)
c  W# :  11   12    13    14    15    16    17    18    19    20
c             F3LT  F3RT        F3LQ  F3RQ        F3LTC F3RTC
c             (ns)  (ns)        (ch)  (ch)        (ns)  (ns)
c  W# :  21   22    23    24    25    26    27    28    29    30
c             Tsum                    Qavr        CTsum 
c             (ns)                    (ch)        (ns)
c-----------------------------------------------------------------------
c  ID=132 F3 IC 
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   IC1   IC2   IC3   IC4   IC5   IC6
c             (ch)  (ch)  (ch)  (ch)  (ch)  (ch)
c  W# :  11   12    13    14    15    16    17    18    19    20
c             IC1   IC2   IC3   IC4   IC5   IC6
c             (ch)        (ch)        (ch)
c  W# :  21   22    23    24    25    26    27    28    29    30
c             ICavr       dEcal       
c             (ch)        (MeV)       
c-----------------------------------------------------------------------
c  ID=133 F3 Si
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   dE1         dE2         dE1ped      dE2ped
c             (ch)        (ch)        (ch)        (ch)
c  W# :  11   12    13    14    15    16    17    18    19    20
c             dE1         dE2         Siavr       Sical
c             (MeV)       (MeV)       (ch)        (MeV)
c  W# :  21   22    23    24    25    26    27    28    29    30
c
c
c-----------------------------------------------------------------------
c  ID=134 F3 reconstruction
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   F31X  F31Y  F32X  F32Y 
c
c  W# :  11   12    13    14    15    16    17    18    19    20
c             F3X         F3Y
c
c  W# :  21   22    23    24    25    26    27    28    29    30
c             F3A         F3B
c
c-----------------------------------------------------------------------
c  ID=141 F4 PPAC
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   F4X         F4Y
c
c-----------------------------------------------------------------------
c  ID=151 F5 reconstruction
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   F51X  F51Y  F52X  F52Y 
c
c  W# :  11   12    13    14    15    16    17    18    19    20
c             F5X         F5Y
c
c  W# :  21   22    23    24    25    26    27    28    29    30
c             F5A         F5B
c
c-----------------------------------------------------------------------
c  ID=161 F6 PPAC
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   F6X         F6Y
c
c-----------------------------------------------------------------------
c  ID=171 F7 Pla
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   F7LT  F7RT                          F7LTC F7RTC
c             (raw) (raw)                         (raw) (raw)
c  W# :  11   12    13    14    15    16    17    18    19    20
c             F7LT  F7RT                          F7LTC F7RTC
c             (ns)  (ns)                          (ns)  (ns)
c  W# :  21   22    23    24    25    26    27    28    29    30
c             Tsum                                CTsum 
c             (ns)                                (ns)
c-----------------------------------------------------------------------
c  ID=172 F7 IC
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   IC1   IC2   IC3   IC4   IC5   IC6
c
c  W# :  11   12    13    14    15    16    17    18    19    20
c             ICavr       ICcal
c             
c-----------------------------------------------------------------------
c  ID=191 FH9 PPAC
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   FH9X        FH9Y
c
c-----------------------------------------------------------------------
c  ID=201 FH10 plastic TOF
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   LTcal RTcal       LQL   RQL   LQT   RQT   LT(R3)RT(R3)
c
c  W# :  11   12    13    14    15    16    17    18    19    20
c             T(VME)LQ    RQ    Qavr              T(R3)
c
c-----------------------------------------------------------------------
c  ID=211 S0 reconstruction
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   FH10aX       FH10aY     FH10bX      FH10bY  
c
c  W# :  11   12    13    14    15    16    17    18    19    20
c             S0X         S0Y
c
c  W# :  21   22    23    24    25    26    27    28    29    30
c             S0A         S0B
c
c-----------------------------------------------------------------------
c  ID=212 S0 TOF
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   MCPQL             MCPQT             MCPT
c
c  W# :  11   12    13    14    15    16    17    18    19    20
c                               MCPQ              
c
c-----------------------------------------------------------------------
c  ID=213 S1 plastic
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   S1Traw      S1Tcal
c
c-----------------------------------------------------------------------
c  ID=214 S2 PPAC position
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   X1    Y1    X2    Y2    X1ped Y1ped X2ped Y2ped
c
c  W# :  11   12    13    14    15    16    17    18    19    20
c             X1-X2 X1+X2 Y1-Y2 Y1-Y2       Xrat  Yrat 
c
c  W# :  21   22    23    24    25    26    27    28    29    30
c             S2X         S2Y         S2Xgeo      S2Ygeo
c
c-----------------------------------------------------------------------
c  ID=301 ILC1 PPAC and reconstruction
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   X1    Y1    X2    Y2    AT
c             (ch)  (ch)  (ch)  (ch)  (ch)
c  W# :  11   12    13    14    15    16    17    18    19    20
c             X1    Y1    X2    Y2    AT          Txsum Tysum
c             (ns)  (ns)  (ns)  (ns)  (ns)
c  W# :  21   22    23    24    25    26    27    28    29    30
c             dTX(X2-X1)  dTY(Y1-Y2)  dTXcal      dTYcal 
c
c  W# :  31   32    33    34    35    36    37    38    39    40
c             Xns2mm      Yns2mm      Xint        Yint
c
c  W# :  41   42    43    44    45    46    47    48    49    50
c             Xgeo        Ygeo        ILC1A       ILC1B
c             (mm)        (mm)        (mrad)      (mrad)
c-----------------------------------------------------------------------
c  ID=302 ILC2 T-shape plastic
c  W# :  1    2      3      4      5      6      7      8      9      10
c        ID   LT     RUT    RDT           LQ     RUQ    RDQ
c             (ns)   (ns)   (ns)         
c  W# :  11   12     13     14     15     16     17     18     19     20
c        ID                               LQcal  RUQcal RDQcal
c             
c  W# :  21   22     23     24     25     26     27     28     29     30
c        ID   RTsum  Tsum                 RQavr  Qavr
c
c-----------------------------------------------------------------------
c  ID=303 ILC2 PPAC position
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   X1    Y1    X2    Y2    AT
c             (ch)  (ch)  (ch)  (ch)  (ch)
c  W# :  11   12    13    14    15    16    17    18    19    20
c             X1    Y1    X2    Y2    AT          Txsum Tysum
c             (ns)  (ns)  (ns)  (ns)  (ns)
c  W# :  21   22    23    24    25    26    27    28    29    30
c             dTX(X2-X1)  dTY(Y1-Y2)  dTXcal      dTYcal 
c
c  W# :  31   32    33    34    35    36    37    38    39    40
c             Xns2mm      Yns2mm      Xint        Yint
c
c  W# :  41   42    43    44    45    46    47    48    49    50
c             Xgeo        Ygeo        
c             (mm)        (mm)        
c-----------------------------------------------------------------------
c  ID=304 R-MD plastic TOF
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   MD1T  MD2T  MD3T  MD4T  KLT   KRT               KT
c             (ch)  (ch)  (ch)  (ch)  (ns)  (ns)             (ns)
c  W# :  11   12    13    14    15    16    17    18    19    20
c             MD1Q  MD2Q  MD3Q  MD4Q  KLQ   KRQ               KQ
c             (ch)  (ch)  (ch)  (ch)  (ch)  (ch)             (ch)
c  W# :  21   22    23    24    25    26    27    28    29    30
c             MD1T  MD2T  MD3T  MD4T
c             (ns)  (ns)  (ns)  (ns)
c-----------------------------------------------------------------------
c  ID=305 ELC plastic
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   UTcal       DTcal       UQ          DQ
c
c  W# :  11   12    13    14    15    16    17    18    19    20
c                                     UQcal       DQcal
c             
c  W# :  21   22    23    24    25    26    27    28    29    30
c             Tsum                    Qavr 
c             
c-----------------------------------------------------------------------
c  ID=306 ELC IC (Saitama)
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   IC1   IC2   IC3   IC4
c
c  W# :  11   12    13    14    15    16    17    18    19    20
c             IC1p  IC2p  IC3p  IC4p
c             
c  W# :  21   22    23    24    25    26    27    28    29    30
c                                     ICavr       ICcal               
c
c-----------------------------------------------------------------------
c  ID=307 NaI
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   Qraw        Qp          Qcal        
c
c-----------------------------------------------------------------------
c  ID=308 Schottky
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   T_us  T_ms
c
c-----------------------------------------------------------------------
c  ID=400 For PI
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   TOF23       TOF37       TOF3S0      TOF3ILC2
c             
c  W# :  11   12    13    14    15    16    17    18    19     20
c        ID   TOF23(off)  TOF37(off)  TOF3S0(off) TOF3ILC2(off)
c
c  W# :  21   22    23    24    25    26    27    28    29     30
c        ID   F3dE(IC)                dE(Si1)     dE(Si2)      
c
c-----------------------------------------------------------------------
c  ID=410 For Mass
c  W# :  1    2     3     4     5     6     7     8     9     10
c        ID   
c
c-----------------------------------------------------------------------
c  encbeam.f
c
c     ihit_min                  : the number of required data (ihit_min0)
c     hitdet(1:nhitdet)         : hit detector id
c     nhitdata(1:ndet)          : the number of data for each id 
c     nhitdet                   : the number of hit detector
c     ndet                      : the number of total detectors
c     ndata                     : the length of rawdata array
      IMPLICIT NONE
      INCLUDE 'analyslogic.fh'
      INCLUDE 'commonprm.fh'

      INTEGER nx, ny, ndet, ndata, naok
      INTEGER rawdata(ndata,ndet)
      INTEGER nhitdata(ndet)
      INTEGER hitdet(ndet)
      INTEGER nhitdet
      INTEGER i,j

      REAL    val(nx,ny)

ccccc RF ccccccc
      REAL RF

ccccc F2 ccccccc
      REAL F2Tsum, F2Qavr, F2dE

ccccc F3 ccccccc
      REAL f3eX, f3eY
      REAL f3dX, f3dY
      REAL f3dZ, f3Z

      REAL F3Tsum, F3Qavr, F3TCsum

      REAL F3dE1, F3dE2, F3dE

ccccc F5 ccccccc
      REAL F5eX, F5eY
      REAL F5dX, F5dY
      REAL F5dZ, F5Z
      REAL F5x

ccccc F7 ccccccc
      REAL F7Tsum, F7TCsum
      REAL F7dE

ccccc FH10 ccccccc
c      REAL s0eX, s0eY
      REAL s0dX, s0dY
      REAL s0dZx, s0dZy
      REAL s0Zx, s0Zy

      REAL FH10Tsum_FH10, FH10Tsum_R3
      REAL S0Tsum_FH10, S0Tsum_R3
      REAL T_F3S0

ccccc ILC1 ccccc

      REAL s2eX, s2eY
      REAL s2dX, s2dY
      REAL s2dZx, s2dZy
      REAL s2Zx, s2Zy

ccccc ILC2 ccccc
      REAL ILC2Tsum

ccccc in ring ccccc
      REAL KickTsum

ccccc ELC  ccccc
      REAL ELCTsum
      REAL ELCdE

cccc dispersion correction factor cccc

      REAL cf46dp
      REAL cf6il1dp
      REAL cf6il2dp



CCCCCC  TOF DEFINITATION    CCCC
      REAL T_S0_ILC2
      REAL T_ILC2_KICKER
      REAL T_S0_KICKER
      REAL T_S0_ELC
      REAL T_KICKER_ELC
   
cccc    MASS CALIBRATION 20161115 ZHUANG cccc
    
      
 
      REAL CC,CC2,MU,MU2,M_E
      !75CU,76ZN,77GA,78GE,79AS;REF,78GE
      REAL MOQ1,MOQ2,MOQ3,MOQ4,MOQ5,MOQ0
      REAL BETA,BETA1,BETA2,BETA3,BETA4,BETA5 
      REAL EM1,EM2,EM3,EM4,EM5,EM0
      !REAL TRV00,TRV01,TRV10,TRTR11,TR2
      REAL DELTA_MOQ1,DELTA_MOQ2,DELTA_MOQ3
      REAL DELTA_MOQ4,DELTA_MOQ5
      REAL DPOP,BRHO,BRHO1,BRHO2
      REAL BE
      REAL M1,M2,M3,M4,M5,M0
      REAL DELTA_M1,DELTA_M2,DELTA_M3,DELTA_M4,DELTA_M5
      REAL TRV_78GE
      REAL TRV_77GA
      REAL TRV_76ZN
      REAL TR_77Ga1
      REAL TR_76Zn1
      REAL BETA_77Ga1
      REAL BETA_76ZN1
      REAL BETAC_77Ga1
      REAL BETAC_76ZN1
      REAL MOQ_77GA1
      REAL MOQ_76ZN1
      REAL ME_77GA1
      REAL ME_76ZN1
      REAL DELT_MOQ_77GA1
      REAL DELT_MOQ_76ZN1


c      CHARACTER ihitchara*4
      INTEGER ihit_min0

      INTEGER ihit, id

      SAVE ihit_min0
c      save naiprev

      IF (InitENCflag(2)) THEN
c         CALL LOADDALIPRM
c         CALL GETENV('IHIT_MIN0',ihitchara)
c         READ(ihitchara,*) ihit_min0
c         IF (ihit_min0.EQ.1) THEN
c            WRITE(*,*) 'ENCDALI-M : IHIT_MIN0 = ', ihit_min0
c         ELSE
c            ihit_min0 = 3
c            WRITE(*,*) 'ENCDALI-M : IHIT_MIN0 -> ', ihit_min0
c         ENDIF
         InitENCFlag(2) = .FALSE.
c         DO i=1,186,1
c            write(*,*)i,ped(i)+gain(i)*3840.
c            write(*,*)i,(100.-ped(i))/gain(i),
c     &           int((100.-ped(i))/gain(i)/2.)
c            write(*,*)i, int((100.-ped(i))/gain(i)/2.)
c         ENDDO
      ENDIF

      naok = 0

      DO ihit = 1,nhitdet
         id = hitdet(ihit)

c         IF (id.GT.nch_dali) CYCLE
c         IF (nhitdata(id).LT.ihit_min0) CYCLE ! ihit_min
c         IF (rawdata(3,id).GT.1) CYCLE

         naok = naok + 1

         val(1,naok) = id

         val(2,naok) = rawdata(1,id)
         val(3,naok) = rawdata(2,id)

c         write(*,*) id,naok

c        for FH10 Plastic T and Q
         do j=42,46
            if(id.eq.j) then
            do i=1,2
               rawdata(i,j) = rawdata(i,j) - fh10tref + 20000.
            enddo
         endif
         enddo

c         write(*,*) id, naok,fh10tref

ccccccccccccccccccccccccccccccccc

         
c         IF (val(2,naok).GT.0. .AND. val(3,naok).GT.0.) THEN
c            mult = mult + 1
c         ENDIF

      ENDDO

cccccccccccccccccccccccccccccccccc
ccc all ppac from encppac Part ccc
cccccccccccccccccccccccccccccccccc

      ID = 90
      naok = naok + 1
      val(1,naok) = id
      val(2,naok) = f31ax
      val(3,naok) = f31ay
      val(4,naok) = f31bx
      val(5,naok) = f31by

      val(6,naok) = f32ax
      val(7,naok) = f32ay
      val(8,naok) = f32bx
      val(9,naok) = f32by

      val(12,naok) = f51ax
      val(13,naok) = f51ay
      val(14,naok) = f51bx
      val(15,naok) = f51by

      val(16,naok) = f52ax
      val(17,naok) = f52ay

      val(18,naok) = f52bx
      val(19,naok) = f52by

      val(22,naok) = f4x
      val(23,naok) = f4y

      val(24,naok) = f6x
      val(25,naok) = f6y

      val(32,naok) = fh9x
      val(33,naok) = fh9y

c      val(34,naok) = fh10x
c      val(35,naok) = fh10y
      val(34,naok) = fh10x * (-1.)
      val(35,naok) = fh10y

c      val(36,naok) = s0x
c      val(37,naok) = s0y
      val(36,naok) = s0x * (-1.) 
      val(37,naok) = s0y

ccccccccccccccccc
ccc PPAC Tsum ccc
ccccccccccccccccc

      val(42,naok) = f31atxsum
      val(43,naok) = f31atysum
      val(44,naok) = f31btxsum
      val(45,naok) = f31btysum

      val(46,naok) = f32atxsum
      val(47,naok) = f32atysum
      val(48,naok) = f32btxsum
      val(49,naok) = f32btysum

      val(52,naok) = f51atxsum
      val(53,naok) = f51atysum
      val(54,naok) = f51btxsum
      val(55,naok) = f51btysum

      val(56,naok) = f52atxsum
      val(57,naok) = f52atysum
      val(58,naok) = f52btxsum
      val(59,naok) = f52btysum

      val(62,naok) = f4txsum
      val(63,naok) = f4tysum

      val(64,naok) = f6txsum
      val(65,naok) = f6tysum

      val(74,naok) = fh10txsum
      val(75,naok) = fh10tysum

      val(76,naok) = s0txsum
      val(77,naok) = s0tysum

cccccccccccccc
ccc for RF ccc
cccccccccccccc

      ID = 101

      naok = naok + 1
      val(1,naok) = id

      val(2,naok) = rawdata(1,41)  ! RF raw
      val(4,naok) = val(2,naok) * 0.0274332  ! RF cal

      RF = val(4,naok)

cccccccccccccccccc
ccc for F2 Pla ccc
cccccccccccccccccc

      ID = 121

      naok = naok + 1
      val(1,naok) = id

      val(2,naok) = rawdata(2,28)  ! F2 Pla LT raw
      val(4,naok) = rawdata(2,29)  ! F2 Pla RT raw

      val(6,naok) = rawdata(1,28)  ! F2 Pla LQ raw
      val(8,naok) = rawdata(1,29)  ! F2 Pla RQ raw

      val(12,naok) = val(2,naok) * 0.0268680 ! F2 PLa LT cal
      val(14,naok) = val(4,naok) * 0.0271937 ! F2 PLa RT cal

      val(16,naok) = val(6,naok) - 0.0000 ! F2 PLa LQ ped
      val(18,naok) = val(8,naok) - 0.0000 ! F2 Pla RQ ped

      val(22,naok) = (val(12,naok) + val(14,naok)) * 0.5 ! F2 Tsum

      val(26,naok) = (val(16,naok) * val(18,naok)) **0.5 ! F2 Qavr

      F2Tsum = val(22,naok)

      F2Qavr = val(26,naok)

cccccccccccccccccc
ccc for F3 Pla ccc
cccccccccccccccccc

      ID = 131

      naok = naok + 1
      val(1,naok) = id

      val(2,naok) = rawdata(2,30)  ! F3 Pla LT raw
      val(3,naok) = rawdata(2,31)  ! F3 Pla RT raw
      val(5,naok) = rawdata(1,30)  ! F3 Pla LQ raw
      val(6,naok) = rawdata(1,31)  ! F3 Pla RQ raw
      val(8,naok) = rawdata(1,32) ! F3 Pla LT(CFD) raw 
      val(9,naok) = rawdata(2,32) ! F3 Pla RT(CFD) raw 

      val(12,naok) = val(2,naok) * 0.0271382 ! F3 PLa LT cal
      val(13,naok) = val(3,naok) * 0.0276006 ! F3 PLa RT cal
      val(15,naok) = val(5,naok) - 0.0000 ! F3 PLa LQ ped
      val(16,naok) = val(6,naok) - 0.0000 ! F3 Pla RQ ped
      val(18,naok) = val(8,naok) * 0.0257660 ! F3 PLa LTC cal
      val(19,naok) = val(9,naok) * 0.0275440 ! F3 Pla RTC cal

      val(22,naok) = (val(12,naok) + val(13,naok)) * 0.5 ! F3 Tsum
      val(26,naok) = (val(15,naok) * val(16,naok)) ** 0.5 ! F3 Qavr
      val(28,naok) = (val(18,naok) + val(19,naok)) * 0.5 ! F3 TCsum

      F3Tsum = val(22,naok)
      F3Qavr = val(26,naok)
      F3TCsum = val(28,naok)

ccccccccccccccccc
ccc for F3 IC ccc
ccccccccccccccccc

      ID = 132

      naok = naok + 1
      val(1,naok) = id

      val(2,naok) = rawdata(1,47)  ! F3 IC Ch1 raw
      val(3,naok) = rawdata(2,47)  ! F3 IC Ch2 raw
      val(4,naok) = rawdata(1,48)  ! F3 IC Ch3 raw
      val(5,naok) = rawdata(2,48)  ! F3 IC Ch4 raw
      val(6,naok) = rawdata(1,49)  ! F3 IC Ch5 raw
      val(7,naok) = rawdata(2,49)  ! F3 IC Ch6 raw

      val(12,naok) = val(2,naok) - 0.0 ! F3 IC Ch1 ped.
      val(13,naok) = val(3,naok) - 0.0 ! F3 IC Ch2 ped.
      val(14,naok) = val(4,naok) - 0.0 ! F3 IC Ch2 ped.
      val(15,naok) = val(5,naok) - 0.0 ! F3 IC Ch2 ped.
      val(16,naok) = val(6,naok) - 0.0 ! F3 IC Ch2 ped.
      val(17,naok) = val(7,naok) - 0.0 ! F3 IC Ch3 ped.

      val(22,naok) = (val(12,naok)*val(13,naok)*val(14,naok)*
     &val(15,naok)*val(16,naok)*val(17,naok))**(1/6.)

      val(24,naok) = val(22,naok) * 1.0000 + 0.0000 ! F3 IC Calib.

      F3dE = val(24,naok)


ccccccccccccccccc
ccc for F3 Si ccc
ccccccccccccccccc

      ID = 133

      naok = naok + 1
      val(1,naok) = id

      val(2,naok) = rawdata(1,33)  ! F3 Si1 raw
      val(4,naok) = rawdata(2,33)  ! F3 Si2 raw

      val(6,naok) = val(2,naok) - 0.0 ! F3 Si1 ped.
      val(8,naok) = val(4,naok) - 0.0 ! F3 Si2 ped.

      val(12,naok) = val(6,naok) * 1.0 + 0.0 ! F3 Si1 calib.
      val(14,naok) = val(8,naok) * 1.0 + 0.0 ! F3 Si2 calib.

      val(16,naok) = (val(6,naok) * val(8,naok)) ** 0.5 ! F3 Siavr
      val(18,naok) = val(16,naok) * 1.0 + 0.0 ! F3 Si calib.

      F3dE1 = val(12,naok) 
      F3dE2 = val(14,naok)

ccccccccccccccccccc
ccc for F3 PPAC ccc
ccccccccccccccccccc

      ID = 134

      naok = naok + 1
      val(1,naok) = id

      val(2,naok) = (f31ax + f31bx) * 0.5  ! F3PPAC1 X
      val(3,naok) = (f31ay + f31by) * 0.5  ! F3PPAC1 Y
c      val(4,naok) = (f32ax + f32bx) * 0.5  ! F3PPAC2 X
c      val(5,naok) = (f32ay + f32by) * 0.5  ! F3PPAC2 Y
      val(4,naok) = f32bx  ! F3PPAC2 X                        
      val(5,naok) = f32by  ! F3PPAC2 Y 
      f3eX = val(4,naok)
      f3eY = val(5,naok)
      f3dX = val(4,naok) - val(2,naok)
      f3dY = val(5,naok) - val(3,naok)
      f3dZ = 890.
      f3Z = -890. ! Distance from PPAC2 to F3-focal plane

      val(12,naok) = f3eX + (f3dX/f3dZ)*f3Z ! X @ F3
      val(14,naok) = f3eY + (f3dY/f3dZ)*f3Z ! Y @ F3

      val(22,naok) = (f3dX/f3dZ)*1000. ! angle for X @ F3                
      val(24,naok) = (f3dY/f3dZ)*1000. ! angle for Y @ F3    

c      val(22,naok) = atan(f3dX/f3dZ)*1000. ! angle for X @ F3
c      val(24,naok) = atan(f3dY/f3dZ)*1000. ! angle for Y @ F3

ccccccccccccccccccc
ccc for F4 PPAC ccc
ccccccccccccccccccc

      ID = 141

      naok = naok + 1
      val(1,naok) = id

      val(2,naok) = f4x  ! F4PPAC X
      val(4,naok) = f4y  ! F4PPAC Y


ccccccccccccccccccc
ccc for F5 PPAC ccc
ccccccccccccccccccc

      ID = 151

      naok = naok + 1
      val(1,naok) = id

      val(2,naok) = (f51ax + f51bx) * 0.5  ! F5PPAC1 X
      val(3,naok) = (f51ay + f51by) * 0.5  ! F5PPAC1 Y
      val(4,naok) = (f52ax + f52bx) * 0.5  ! F5PPAC2 X
      val(5,naok) = (f52ay + f52by) * 0.5  ! F5PPAC2 Y
 
      F5eX = val(4,naok)
      F5eY = val(5,naok)
      F5dX = val(4,naok) - val(2,naok)
      F5dY = val(5,naok) - val(3,naok)
      F5dZ = 650.
      F5Z = -250. ! Distance from PPAC2 to F5-focal plane for X

      val(12,naok) = F5eX + (F5dX/F5dZ)*F5Z ! X @ F5
      val(14,naok) = F5eY + (F5dY/F5dZ)*F5Z ! Y @ F5

      val(22,naok) = atan(F5dX/F5dZ)*1000. ! angle for X @ F5
      val(24,naok) = atan(F5dY/F5dZ)*1000. ! angle for Y @ F5

      F5x = val(12,naok)
ccccccccccccccccccc
ccc for F6 PPAC ccc
ccccccccccccccccccc

      ID = 161

      naok = naok + 1
      val(1,naok) = id

      val(2,naok) = f6x  ! F6PPAC X
      val(4,naok) = f6y  ! F6PPAC Y
      val(6,naok) = f6x/73.753 + 0.0 ! transfer F6X to dp/p
      DPOP=val(6,naok)
cccccccccccccccccc
ccc for F7 Pla ccc
cccccccccccccccccc

      ID = 171

      naok = naok + 1
      val(1,naok) = id

      val(2,naok) = rawdata(1,36)  ! F7 Pla LT raw
      val(3,naok) = rawdata(2,36)  ! F7 Pla RT raw
      val(8,naok) = rawdata(1,37)  ! F7 Pla CFDLT raw
      val(9,naok) = rawdata(2,37)  ! F7 Pla CFDRT raw

      val(12,naok) = val(2,naok) * 0.0270701 ! F7 PLa LT cal
      val(13,naok) = val(3,naok) * 0.0277871 ! F7 PLa RT cal
      val(18,naok) = val(8,naok) * 0.026656 ! F7 PLa LTC cal
      val(19,naok) = val(9,naok) * 0.027739 ! F7 Pla RTC cal

      val(22,naok) = (val(12,naok) + val(13,naok)) * 0.5 ! F7 Tsum
      val(28,naok) = (val(18,naok) + val(19,naok)) * 0.5 ! F7 TCsum

      F7Tsum = val(22,naok)
      F7TCsum = val(28,naok)

ccccccccccccccccc
ccc for F7 IC ccc
ccccccccccccccccc

      ID = 172

      naok = naok + 1
      val(1,naok) = id

      val(2,naok) = rawdata(1,38)  ! F7 IC1
      val(3,naok) = rawdata(2,38)  ! F7 IC2
      val(4,naok) = rawdata(1,39)  ! F7 IC3
      val(5,naok) = rawdata(2,39)  ! F7 IC4
      val(6,naok) = rawdata(1,40)  ! F7 IC5
      val(7,naok) = rawdata(2,40)  ! F7 IC6

      val(12,naok) = (val(2,naok)*val(3,naok)*val(4,naok)*
     & val(5,naok)*val(6,naok)*val(7,naok))**(1/6.) ! F7ICavr
      val(14,naok) = val(12,naok) * 1.0000 + 0.0 ! F7IC cal

      F7dE = val(14,naok)

cccccccccccccccc
ccc FH10 Pla ccc
cccccccccccccccc

      ID =191

      naok = naok + 1
      val(1,naok) = id

      val(2,naok) = fh9x
      val(4,naok) = fh9y

cccccccccccccccc
ccc FH10 Pla ccc
cccccccccccccccc

      ID = 201

      naok = naok + 1
      val(1,naok) = id

      val(2,naok) = rawdata(1,42) * 0.09766 ! FH10 LT ch2ns
      val(3,naok) = rawdata(2,42) * 0.09766 ! FH10 RT ch2ns

      val(5,naok) = rawdata(1,43) * 0.09766 ! FH10 LQ ch2ns (Lead)
      val(6,naok) = rawdata(2,43) * 0.09766 ! FH10 RQ ch2ns (Lead)
     
      val(7,naok) = rawdata(1,44) * 0.09766 ! FH10 LQ ch2ns (Trail)
      val(8,naok) = rawdata(2,44) * 0.09766 ! FH10 RQ ch2ns (Trail)

      val(9,naok) = FH10_LT ! in nsec DAQ at B3F --> R3
      val(10,naok) = FH10_RT ! in nsec DAQ at B3F --> R3

      val(12,naok) = (val(2,naok) + val(3,naok)) * 0.5 ! FH10 Tsum @ FH10 DAQ

      val(13,naok) = val(7,naok) - val(5,naok) ! FH10 LQ diff
      val(14,naok) = val(8,naok) - val(6,naok) ! FH10 RQ diff

      val(15,naok) = (val(13,naok) * val(14,naok)) ** 0.5 ! FH10 Qsum @ FH10 DAQ 

      val(18,naok) = (val(9,naok) + val(10,naok)) * 0.5 ! FH10 Tsum @ R3 DAQ 

      FH10Tsum_FH10 = val(12,naok)

      FH10Tsum_R3 = val(18,naok)

ccccccccccccccccccccc
ccc for FH10 PPAC ccc
ccccccccccccccccccccc

      ID = 211

      naok = naok + 1
      val(1,naok) = id

      val(2,naok) = fh10x * (-1.)
      val(4,naok) = fh10y

      val(6,naok) = s0x * (-1.)
      val(8,naok) = s0y
      
      s0dX = val(6,naok) - val(2,naok)
      s0dY = val(8,naok) - val(4,naok)
      s0dZx = 500.
      s0dZy = 500.
      s0Zx =  1268.3 ! Distance from PPAC2 to E-MCP C-foil for X
      s0Zy =  1276.9 ! Distance from PPAC2 to E-MCP C-foil for Y
c      s0Zx =  1321.7 ! Distance from PPAC2 to s0-focal plane for X
c      s0Zy =  1313.1 ! Distance from PPAC2 to s0-focal plane for Y

      val(12,naok) = s0X + (s0dX/s0dZx)*s0Zx ! X @ S0
      val(14,naok) = s0Y + (s0dY/s0dZy)*s0Zy ! Y @ S0

      val(22,naok) = atan(s0dX/s0dZx)*1000. ! angle for X @ S0
      val(24,naok) = atan(s0dY/s0dZy)*1000. ! angle for Y @ S0

cccccccccccccc
ccc S0 MCP ccc
cccccccccccccc

      ID = 212

      naok = naok + 1
      val(1,naok) = id

      val(2,naok) = rawdata(1,45) * 0.09766 ! S0 E-MCP QTC Leading ch2ns
c      val(3,naok) = rawdata(2,44) * 0.09766 ! S0 BT ch2ns

      val(5,naok) = rawdata(1,46) * 0.09766 ! S0 E-MCP QTC Trailing ch2ns
c      val(6,naok) = rawdata(2,45) * 0.09766 ! S0 BQ ch2ns

      val(8,naok) = S0_FT ! in nsec DAQ at B3F --> R3
c      val(9,naok) = S0_BT ! in nsec DAQ at B3F --> R3

c      val(12,naok) = (val(2,naok) + val(3,naok)) * 0.5 ! FH10 Tsum 

      If(val(2,naok).gt.-1000 .and. val(5,naok).gt.-1000) then
         val(15,naok) = (val(5,naok) - val(2,naok)) ! S0 E-MCP Qsum 
      Else 
         val(15,naok) = -100.
      EndIf
c      val(18,naok) = (val(18,naok) + val(19,naok)) * 0.5 ! FH10 Tsum @ R3 DAQ 

c      S0Tsum_FH10 = val(2,naok)

      S0Tsum_R3 = val(8,naok)

cccccccccc
ccc S1 ccc
cccccccccc

      ID = 213

      naok = naok + 1
      val(1,naok) = id

      val(2,naok) = rawdata(1,26) ! S1 raw
      val(4,naok) = val(2,naok) * 0.14331 ! S1 in nsec

cccccccccccccccc
ccc S2 PPAC  ccc
cccccccccccccccc

      ID = 214

      naok = naok + 1
      val(1,naok) = id
      val(2,naok) = rawdata(1,1) ! X1
      val(3,naok) = rawdata(2,1) ! Y1
      val(4,naok) = rawdata(1,2) ! X2
      val(5,naok) = rawdata(2,2) ! Y2

      val(6,naok) = val(2,naok) - 44.0 ! X1 - ped.
      val(7,naok) = val(3,naok) - 59.0 ! Y1 - ped.
      val(8,naok) = val(4,naok) - 47.0 ! X2 - ped.
      val(9,naok) = val(5,naok) - 42.0 ! Y2 - ped.

      val(12,naok) = val(6,naok) - val(8,naok) ! X1 - X2
      val(13,naok) = val(6,naok) + val(8,naok) ! X1 + X2
      val(14,naok) = val(7,naok) - val(9,naok) ! Y1 - Y2
      val(15,naok) = val(7,naok) + val(9,naok) ! Y2 + Y2

      val(17,naok) = val(12,naok) / val(13,naok)
      val(18,naok) = val(14,naok) / val(15,naok)

      val(22,naok) = (val(17,naok) * (-52.189 )- 0.1292)*(-1) ! S2 PPAC X calib
      val(24,naok) = val(18,naok) * (-51.879) - 0.0942 ! S2 PPAC Y calib 

      if((val(6,naok).gt.20. .and. val(6,naok).lt.4000.).and.
     & (val(8,naok).gt.20. .and. val(8,naok).lt.4000.)) then
         val(26,naok) = val(22,naok) + 0.000 ! S2 PPAC X geo offset 
      else
         val(26,naok) = -1000.
      endif

      if((val(6,naok).gt.20. .and. val(6,naok).lt.4000.).and.
     &(val(8,naok).gt.20. .and. val(8,naok).lt.4000.)) then
         val(28,naok) = val(24,naok) + 0.000 ! S2 PPAC Y geo offset
      else
         val(28,naok) = -1000.
      endif
      s2eX = val(26,naok)
      s2eY = val(28,naok)

cccccccccccccccccccccccccccccccccccccc
ccc for dispersion correction part ccc
cccccccccccccccccccccccccccccccccccccc

      ID = 215

      naok = naok + 1
      val(1,naok) = id

      cf46dp = -3.726 ! (-75.3 / 18.3 mm) <-- input fitting result F4 X vs F6 X
c      cf46dp = -1.557 ! (-75.3 / 18.3 mm) <-- input fitting result F4 X vs F6 X
c      cf46dp = -4.0000 ! (-75.3 / 18.3 mm) <-- input fitting result F4 X vs F6 X
      val(2,naok) = f6x - cf46dp * f4x

      cf6il1dp = -0.6000 ! (-47 / 75.3 mm) <-- input fitting result F6 X vs ILC1 X
      val(4,naok) = ilc1x - cf6il1dp * f6x

      cf6il2dp = 0.9500 ! (-72.5 / 75.3 mm) <-- input fitting result F4 X vs ILC2 X
      val(6,naok) = ilc2x - cf6il2dp * f6x


ccccccccccccccccccccc   R3 Part  cccccccccccccccccccc

cccccccccccccccccccccccc
ccc ILC1 PPAC Calib. ccc
cccccccccccccccccccccccc

      ID = 301

      naok = naok + 1
      val(1,naok) = id
      val(2,naok) = rawdata(1,11) ! X1
      val(3,naok) = rawdata(2,11) ! Y1
      val(4,naok) = rawdata(1,12) ! X2
      val(5,naok) = rawdata(2,12) ! Y2
      val(6,naok) = rawdata(1,13) ! Anode

      val(12,naok) = val(2,naok) * 0.04819 ! X1 ch2ns
      val(13,naok) = val(3,naok) * 0.04851 ! Y1 ch2ns
      val(14,naok) = val(4,naok) * 0.04801 ! X2 ch2ns
      val(15,naok) = val(5,naok) * 0.04794 ! Y2 ch2ns
      val(16,naok) = val(6,naok) * 0.04789 ! AT ch2ns

      val(18,naok) = val(12,naok) + val(14,naok) ! Txsum
      val(19,naok) = val(13,naok) + val(15,naok) ! Tysum

      val(22,naok) = val(14,naok) - val(12,naok) ! dTX (X2-X1)
      val(24,naok) = val(13,naok) - val(15,naok) ! dTY (Y1-Y2)

      val(26,naok) = val(22,naok) - 1.612 ! dTX outline calib.
      val(28,naok) = val(24,naok) + 1.660 ! dTY outline calib.

      val(32,naok) = val(26,naok) * 1.234 / 2. ! dTX ns2mm
      val(34,naok) = val(28,naok) * 1.253 / 2. ! dTY ns2mm

      val(36,naok) = val(32,naok) - 0.18 ! X internal offset
      val(38,naok) = val(34,naok) + 0.57 ! Y internal offset

      if((val(18,naok).gt.0).and.(val(18,naok).lt.200))then
         val(42,naok) = val(36,naok) + 2.0000 ! X geometrical offset 
      else
         val(42,naok) = -100. ! TXsum gateout
      endif

      if((val(19,naok).gt.0).and.(val(19,naok).lt.200))then
         val(44,naok) = val(38,naok) + 0.0000 ! Y geometrical offset
      else
         val(44,naok) = -100. ! TYsum gateout
      endif

      s2dX = val(42,naok) - s2eX
      s2dY = val(44,naok) - s2eY
      s2dZx = 2770.
      s2dZy = 2700.
      s2Zx =  1. ! Distance from PPAC2 to s0-focal plane for X
      s2Zy =  1. ! Distance from PPAC2 to s0-focal plane for Y

      val(46,naok) = atan(s2dX/s2dZx)*1000. ! angle for X @ ILC1
      val(48,naok) = atan(s2dY/s2dZy)*1000. ! angle for Y @ ICL1


cccccccccccccccccc
ccc ILC2 T-Pla ccc
cccccccccccccccccc

      ID = 302

      naok = naok + 1
      val(1,naok) = id
      val(2,naok) = ILC2_LT ! LT in nsec TC842
      val(3,naok) = ILC2_RUT ! RUT in nsec TC842
      val(4,naok) = ILC2_RDT ! RDT in nsec TC842

      val(6,naok) = rawdata(1,5) ! LQ 
      val(7,naok) = rawdata(2,5) ! RUQ
      val(8,naok) = rawdata(1,6) ! RDQ

      val(16,naok) = val(6,naok) - 0.0000 ! LQ - ped. 
      val(17,naok) = val(7,naok) - 0.0000 ! RUQ - ped.
      val(18,naok) = val(8,naok) - 0.0000 ! RDQ - ped.

      val(22,naok) = (val(3,naok) + val(4,naok)) * 0.5 ! RTsum 
      val(23,naok) = (val(2,naok) + val(22,naok)) * 0.5 ! Tsum 

      val(26,naok) = (val(17,naok) * val(18,naok)) ** 0.5 ! RQavr 
      val(27,naok) = (val(16,naok) * val(26,naok)) ** 0.5 ! Qavr

      ILC2Tsum = val(23,naok) 

cccccccccccccccccccccccc
ccc ILC2 PPAC Calib. ccc
cccccccccccccccccccccccc

      ID = 303

      naok = naok + 1
      val(1,naok) = id
      val(2,naok) = rawdata(1,15) ! X1
      val(3,naok) = rawdata(2,15) ! Y1
      val(4,naok) = rawdata(1,16) ! X2
      val(5,naok) = rawdata(2,16) ! Y2
      val(6,naok) = rawdata(1,17) ! Anode

      val(12,naok) = val(2,naok) * 0.04860 ! X1 ch2ns
      val(13,naok) = val(3,naok) * 0.04832 ! Y1 ch2ns
      val(14,naok) = val(4,naok) * 0.04868 ! X2 ch2ns
      val(15,naok) = val(5,naok) * 0.04877 ! Y2 ch2ns
      val(16,naok) = val(6,naok) * 0.04893 ! AT ch2ns

      val(18,naok) = val(12,naok) + val(14,naok) ! Txsum
      val(19,naok) = val(13,naok) + val(15,naok) ! Tysum

      val(22,naok) = val(14,naok) - val(12,naok) ! dTX (X2-X1)
      val(24,naok) = val(13,naok) - val(15,naok) ! dTY (Y1-Y2)

      val(26,naok) = val(22,naok) - 2.654 ! dTX outline calib.
      val(28,naok) = val(24,naok) + 14.88 ! dTY outline calib.

      val(32,naok) = val(26,naok) * 1.245 / 2. ! dTX ns2mm
      val(34,naok) = val(28,naok) * 1.239 / 2. ! dTY ns2m

      val(36,naok) = val(32,naok) - 0.35  ! X internal offset
      val(38,naok) = val(34,naok) - 0.79  ! Y internal offset

      if((val(18,naok).gt.0).and.(val(18,naok).lt.200))then
         val(42,naok) = val(36,naok) + 0.5000 ! X geometrical offset 
      else
         val(42,naok) = -100.
      endif

      if((val(19,naok).gt.0).and.(val(19,naok).lt.200))then
         val(44,naok) = val(38,naok) + 0.0000 ! Y geometrical offset
      else
         val(44,naok) = -100.
      endif

cccccccccccccccc
ccc R-MD Pla ccc
cccccccccccccccc

      ID = 304

      naok = naok + 1
      val(1,naok) = id

      val(2,naok) = rawdata(1,24) ! R-MD1 T
      val(3,naok) = rawdata(2,24) ! R-MD2 T
      val(4,naok) = rawdata(1,25) ! R-MD3 T
      val(5,naok) = rawdata(2,25) ! R-MD4 T
      val(6,naok) = Kick_LT ! Kicker Pla in nsec TC842
      val(7,naok) = Kick_RT ! Kicker Pla in nsec TC842

      val(10,naok) = (val(6,naok) + val(7,naok)) * 0.5 ! Kicker-Pla Tsum
      
      KickTsum = val(10,naok) ! KT

      val(12,naok) = rawdata(2,6) ! R-MD1 Q
      val(13,naok) = rawdata(1,7) ! R-MD2 Q
      val(14,naok) = rawdata(1,8) ! R-MD3 Q
      val(15,naok) = rawdata(2,8) ! R-MD4 Q
      val(16,naok) = rawdata(1,9) ! kick-LQ
      val(17,naok) = rawdata(2,9) ! kick-RQ

      val(20,naok) = (val(16,naok) * val(17,naok)) ** 0.5 ! Kicker-Pla Qavr

      val(22,naok) = val(2,naok) * 0.05505 ! R-MD1 Tcal
      val(23,naok) = val(3,naok) * 0.05597 ! R-MD2 Tcal
      val(24,naok) = val(4,naok) * 0.05604 ! R-MD3 Tcal
      val(25,naok) = val(5,naok) * 0.05615 ! R-MD4 Tcal

      val(35,naok) = S0_BT
ccccccccccccccc
ccc ELC Pla ccc
ccccccccccccccc

      ID = 305

      naok = naok + 1
      val(1,naok) = id

      val(2,naok) = ELC_UT  ! TU in nsec 
      val(4,naok) = ELC_DT  ! TD in nsec 

      val(6,naok) = rawdata(1,10) ! QUraw 
      val(8,naok) = rawdata(2,10) ! QDraw

      val(16,naok) = val(6,naok) - 0.0000 ! QU - ped.
      val(18,naok) = val(8,naok) - 0.0000 ! QD - ped.

      val(22,naok) = (val(2,naok) + val(4,naok))*0.5 ! ELC_Tsum

      val(24,naok) = val(22,naok) - 700000. ! ELC_Tsum - 700usec

      val(26,naok) = (val(16,naok) * val(18,naok))**0.5 ! ELC_Qavr

      ELCTsum = val(22,naok)

cccccccccccccc
ccc ELC IC ccc
cccccccccccccc

      ID = 306

      naok = naok + 1
      val(1,naok) = id
      val(2,naok) = rawdata(1,19) ! ELC IC ch 1
      val(3,naok) = rawdata(2,19) ! ELC IC ch 2
      val(4,naok) = rawdata(1,20) ! ELC IC ch 3
      val(5,naok) = rawdata(2,20) ! ELC IC ch 4

      val(12,naok) = val(2,naok) - 44.0 ! ELC IC ch 1 - ped.
      val(13,naok) = val(3,naok) - 52.0 ! ELC IC ch 2 - ped.
      val(14,naok) = val(4,naok) - 49.0 ! ELC IC ch 3 - ped.
      val(15,naok) = val(5,naok) - 52.0 ! ELC IC ch 4 - ped.

      val(26,naok) = (val(12,naok)*val(13,naok)*
     & val(14,naok)*val(15,naok)) ** (1/4.) ! ELC IC avr
      val(28,naok) = val(26,naok) * 1.0000 + 0.0 ! ELC IC in MeV

      ELCdE = val(28,naok)


cccccccccccc
ccc NaI  ccc
cccccccccccc

      ID = 307

      naok = naok + 1
      val(1,naok) = id

      val(2,naok) = rawdata(1,23) ! NaI Q

      val(4,naok) = val(2,naok) - 0.0000 ! NaI Q - ped.

      val(6,naok) = val(4,naok) * 1.000 - 0.00 ! TotalE calib.

c      val(8,naok) = NaI_T ! in nsec


cccccccccccccccc
ccc Schottky ccc
cccccccccccccccc

      ID = 308

      naok = naok + 1
      val(1,naok) = id
      val(2,naok) = Schottky1 ! RSA Trig. in usec
      val(3,naok) = Schottky2 ! RSA Trig. in msec


cccccccccccccccccccc
ccc for PI calc. ccc
cccccccccccccccccccc
      Id = 400

      naok = naok + 1
      val(1,naok) = id

      val(2,naok) = F3Tsum - F2Tsum ! TOF23
      val(4,naok) = F7Tsum - F3Tsum ! TOF37
      val(6,naok) = S0Tsum_R3 - F3Tsum ! TOF3X (E-MCP)
      val(7,naok) = FH10Tsum_R3 - F3Tsum ! TOF3X (FH10Pla)
      val(8,naok) = ILC2Tsum - F3Tsum ! TOF3ILC2

      val(12,naok) = val(2,naok) + 90.0000 ! TOF23 offset in
      val(14,naok) = val(4,naok) + 0.0000 ! TOF37 offset in
      val(16,naok) = val(6,naok) + 0.0000 ! TOF3S0 offset in
      val(17,naok) = val(7,naok) + 0.0000 ! TOF3X offset in
      val(18,naok) = val(8,naok) + 0.0000 ! TOF3ILC2 offset in

      val(22,naok) = F3dE ! F3dE IC

      val(26,naok) = F3dE1 ! F3dE Si1
      val(28,naok) = F3dE2 ! F3dE Si1
c      val(30,naok) = F3dE ! F3dE Si1+Si2

      val(32,naok) = F7dE ! F7dE IC

      val(42,naok) = val(12,naok) - 1.0000 * f6x ! TOF23 cor by F6X
      val(46,naok) = val(16,naok) - (-0.047338) * f6x ! TOF3S0 cor by F6X
      val(48,naok) = val(18,naok) - 1.0000 * f6x ! TOF3ILC2 cor by F6X
      val(50,naok) = val(4,naok) +  0.04848 * F5x ! TOF37 cor by F5X
C      val(50,naok) = val(4,naok) + 0.023729 * F5x ! TOF37 cor by F5X
      val(51,naok) = val(32,naok) + 4.2915 * F5x ! F7IC cor by F5X
c     val(51,naok) = RF - 0.06751 * F5x ! RF cor by F5X
C      val(52,naok) = val(22,naok) - 1.0000 * f6x ! F3dE(IC) cor by F6X
      val(52,naok) = val(22,naok) - (-0.5595) * f6x ! F3dE(IC) cor by F6X

      val(56,naok) = val(26,naok) - 1.0000 * f6x ! F3dE1(Si1) cor by F6X
      val(58,naok) = val(28,naok) - 1.0000 * f6x ! F3dE2(Si2) cor by F6X
      
      T_F3S0=val(6,naok)

cccccccccccccccccc
c for Mass calc. c
cccccccccccccccccc
      ID = 410
      
      naok = naok + 1
      val(1,naok) = id

      val(2,naok) = ILC2Tsum - S0Tsum_R3 + 0.0000 ! between S0-ILC2 TOF w/ offset
      val(4,naok) = KickTsum - ILC2Tsum + 0.0000 ! ILC2-KICKER TOF w/ offset
      val(5,naok) = KickTsum - S0Tsum_R3 + 0.0000 ! S0-KICKER TOF w/ offset

      val(6,naok) = ELCTsum - S0Tsum_R3  ! S0-ELC TOF w/ offset  MOST IMPO
      val(7,naok) = ELCTsum - KickTsum  ! KICKER-ELC TOF w/ offset 
      val(8,naok) = val(6,naok) - 700000. !S0-ELC TOF w/ offset  -700usec

      val(10,naok) = val(6,naok) + 694.3  - 700000. ! TOF ring offset -700usec
      val(11,naok) = DPOP
      val(12,naok) = val(6,naok) + 694.3 ! TOF ring offset


      T_S0_ILC2=val(2,naok)

      T_ILC2_KICKER=val(4,naok) 

      T_S0_KICKER=val(5,naok)
     
      T_KICKER_ELC=val(7,naok)  
      
      T_S0_ELC=val(12,naok)

c      val(8,naok) = val(6,naok) - 0.0000 ! TOF - out of ring ??
cccccccccccccccccc
c ADDED IN 20161115 for Mass calc. c
C   ZHUANG    C
cccccccccccccccccc
      !F6X IS THE OFFSET OF F6@X BECASUSE OF MOMENTUM DISPERSION , DPOP IS DP/P
        !ID = 411
      
      !naok = naok + 1
      !val(1,naok) = id
     
      !DOUBLE KICKER RUN257 258 .   78GE 33.45 ;77GA 45.93
       !75CU,76ZN,77GA,78GE,79AS;REF,78GE
       !MOQ1,MOQ2,MOQ3,MOQ4,MOQ5,MOQ0
       !BETA,BETA1,BETA2,BETA3,BETA4,BETA5 
       CC=299.792458   !10**6m/s 
       CC2= 299.792458*299.792458
       MU = 931.4940610  !MEV
       MU2 = 931.4940610*931.4940610
       !75CU,76ZN,77GA,78GE,79AS;REF,78GE
       MOQ1=2.583643529   !75CU
       MOQ2=2.530557008   !75CU,76ZN
       MOQ3=2.481038881   !75CU,76ZN,77GA
       MOQ4=2.434542495   !75CU,76ZN,77GA,78GE
       MOQ5=2.390997318   !75CU,76ZN,77GA,78GE,79AS
       MOQ0=2.434542495   !75CU,76ZN,77GA,78GE,79AS;REF,78GE
       
    
      

      !<name>D1</name> <brho>6.7203</brho> <name>D2</name><brho>6.0859</brho>
      !D3 4.8577 , D4 4.8480 ,D5 4.8480,D6 0.0000 ,D7 4.8500 ,D6 0.000
      !D7 4.8500 , D8 4.8500
      BRHO1=4.8480   !@F6
      BRHO= BRHO1*(1+DPOP*0.01)
      !write(*,*)brho
      
      val(16,naok)=BRHO
      BRHO2=BRHO*BRHO
      BETA1=1.0/SQRT((MOQ1*MOQ1*MU2/(BRHO2*CC2))+1.0)
      BETA2=1.0/SQRT((MOQ2*MOQ2*MU2/(BRHO2*CC2))+1.0)
      BETA3=1.0/SQRT((MOQ3*MOQ3*MU2/(BRHO2*CC2))+1.0)
      BETA4=1.0/SQRT((MOQ4*MOQ4*MU2/(BRHO2*CC2))+1.0)
      BETA5=1.0/SQRT((MOQ5*MOQ5*MU2/(BRHO2*CC2))+1.0) 
      !BETA0= BETA1
      val(19,naok)=BETA1
      val(20,naok)=BETA2
      val(21,naok)=BETA3
      val(22,naok)=BETA4
      val(23,naok)=BETA5
      ! write(*,*)beta1
      !revolution number and time 
      !NUCLEI     TOFXELC   DOUBLEKIKER            TOFRING         MCP           N
      !78Ge      700752.3+694.3      33.446 +694.3   700718.9    373.415(20)    1878
       !         701446.9            728.09
      !77GA      700822.6+694.3     45.933          700776.7    378.243(30)    1853
      !          701515.8           740.28
      !76ZN      701228.06             57.030+694.3              383.645(20)    1826
      !                               751.26



      !DISPERSION:73.753 MM/%  DPOP=f6x / 73.753 + 0.0 ! F6X to dp/p

      val(26,naok)=(T_S0_ELC-728.09)/1878.  !revolution time of 78Ge event by event
      val(27,naok)=(T_S0_ELC-740.28)/1853.  !revolution time of 77Ga
      val(28,naok)=(T_S0_ELC-751.26)/1826.  !revolution time of 76Zn
      TRV_78GE=val(26,naok)
      TRV_77GA=val(27,naok)
      TRV_76ZN=val(28,naok)
      
      !ratio of T1/T0, 78GE AS REF
      TR_77Ga1=TRV_77GA/373.1197
      TR_76Zn1=TRV_76ZN/373.1197
       val(34,naok)=TR_77Ga1
       val(35,naok)=TR_76Zn1
      !BETA= 286.58517/(T_F3S0 + 322.16136)
      !BETA = 283.373 /(T_F3S0 + 316.187) ! TOF calib
      !Beta = 291.856/(tof3X + 332.367)
      BETA= 286.73229/(T_F3S0 + 322.40746)
      val(30,naok)=BETA
      BETA_77Ga1=BETA*0.999994928
      BETA_76ZN1=BETA*0.999995618
       val(32,naok)=BETA_77Ga1
       val(33,naok)=BETA_76ZN1
      BETAC_77Ga1=sqrt((1-BETA_77GA1**2)/(1-(TR_77GA1*BETA_77GA1)**2))

      BETAC_76ZN1=sqrt((1-BETA_76ZN1**2)/(1-(TR_76ZN1*BETA_76ZN1)**2))
      val(36,naok)=BETAC_77Ga1
      val(37,naok)=BETAC_76ZN1
      
      !CAL MASS TURN
      val(39,naok)=TR_77Ga1*MOQ4*BETAC_77Ga1
      val(40,naok)=TR_76Zn1*MOQ4*BETAC_76ZN1
            MOQ_77GA1=val(39,naok)
            MOQ_76ZN1=val(40,naok)
            DELT_MOQ_77GA1=(val(39,naok)-MOQ3)/MOQ3
            DELT_MOQ_76ZN1=(val(40,naok)-MOQ2)/MOQ2
                  val(41,naok)=DELT_MOQ_77GA1
                  val(42,naok)=DELT_MOQ_76ZN1
       !REVOLUTION TIME cor
      val(43,naok)=TRV_77GA*BETAC_77Ga1
      val(44,naok)=TRV_76ZN*BETAC_76ZN1



      !TOF RING cor
      val(45,naok)=T_S0_ELC*BETAC_77Ga1
      val(46,naok)=T_S0_ELC*BETAC_77Ga1


      !MASS EXCESS(KEV)
       !75CU,76ZN,77GA,78GE,79AS;REF,78GE
       M_E=510.998928
       EM1=45232.21139 !ev
       EM2=49061.56713
       EM3=53074.77229
       EM4=57274.52501
       EM5=61663.50747
       EM0=57274.52501

       !val(47,naok)=MOQ_77GA
       !val(48,naok)=MU*

       val(50,naok)=MU*(MOQ_77GA1*31.-77.)*1000.0-EM3/1000.0+31.*M_E
       val(51,naok)=MU*(MOQ_76ZN1*31.-77.)*1000.0-EM2/1000.0+30.*M_E
       ME_77GA1=val(50,naok)
       ME_76ZN1=val(51,naok)
       
      

     
      







       



      RETURN
      END

c ======================================
