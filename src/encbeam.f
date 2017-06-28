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

      REAL F3Tsum, F3Qavr, F3TCsum, F3Tminus

      REAL F3dE1, F3dE2, F3dE

      REAL f3x, f3y
      REAL f3a, f3b

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
      REAL T_F3S0,T_F2F3
      REAL T_F3S0_1
      REAL T_F3S0_c
      REAL T_F3S0_c1

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
    
      
 
      REAL CC,CC2,MU,MU2,M_E,LC
      !75CU,76ZN,77GA,78GE,79AS;REF,78GE;77Ge
      REAL MOQ1,MOQ2,MOQ3,MOQ4,MOQ5,MOQ0,MOQ6
      REAL BETA,BETA_0,BETA1,BETA2,BETA3,BETA4,BETA5,BETA6
      REAL BETAc,BETA1c,BETA2c,BETA3c,BETA4c,BETA5c,BETA6c
      REAL beta_br
      REAL EM1,EM2,EM3,EM4,EM5,EM0,EM6
      !REAL TRV00,TRV01,TRV10,TRTR11,TR2
      REAL DELTA_MOQ1,DELTA_MOQ2,DELTA_MOQ3
      REAL DELTA_MOQ4,DELTA_MOQ5,DELTA_MOQ6
      REAL DPOP,BRHO,BRHO1,BRHO2,DPOP2
      REAL BE
      REAL M1,M2,M3,M4,M5,M0,M6
      REAL DELTA_M1,DELTA_M2,DELTA_M3
      REAL DELTA_M4,DELTA_M5,DELTA_M6

      REAL TRV_78GE
      REAL TRV_77GA
      REAL TRV_76ZN
      REAL TRV_75CU
      REAL TRV_79AS
      REAL TRV_77GE

      REAL TR_77Ga1
      REAL TR_76Zn1
      REAL TR_75Cu1
      REAL TR_78Ge1
      REAL TR_79As1
      REAL TR_77Ge1

      !REAL TR_77Ga2
      !REAL TR_76Zn2
      !REAL TR_75Cu2
      !REAL TR_78Ge2
      !REAL TR_79As2
      !REAL TR_77Ge2

      REAL BETA_77Ga1
      REAL BETA_76ZN1
      REAL BETA_75Cu1
      REAL BETA_78Ge1
      REAL BETA_79As1
      REAL BETA_77Ge1

      REAL BETAC_77Ga1
      REAL BETAC_76ZN1
      REAL BETAC_75Cu1
      REAL BETAC_78Ge1
      REAL BETAC_79As1
      REAL BETAC_77Ge1

      REAL BRHOC_77Ga1
      REAL BRHOC_76ZN1
      REAL BRHOC_75Cu1
      REAL BRHOC_78Ge1
      REAL BRHOC_79As1
      REAL BRHOC_77Ge1

      REAL MOQ_77GA1
      REAL MOQ_76ZN1
      REAL MOQ_75CU1
      REAL MOQ_79AS1
      REAL MOQ_77Ge1
      REAL MOQ_78Ge1

      REAL MOQ_77GA2
      REAL MOQ_76ZN2
      REAL MOQ_75CU2
      REAL MOQ_79AS2
      REAL MOQ_77Ge2
      REAL MOQ_78Ge2

      REAL ME_77GA1
      REAL ME_76ZN1
      REAL ME_75CU1
      REAL ME_79AS1
      REAL ME_77Ge1
      REAL ME_78Ge1

      REAL ME_77GA2
      REAL ME_76ZN2
      REAL ME_75CU2
      REAL ME_79AS2
      REAL ME_77Ge2
      REAL ME_78Ge2

      REAL DELT_MOQ_77GA1
      REAL DELT_MOQ_76ZN1
      REAL DELT_MOQ_75CU1
      REAL DELT_MOQ_79AS1
      REAL DELT_MOQ_77Ge1
      REAL DELT_MOQ_78Ge1

      REAL DELT_MOQ_77GA2
      REAL DELT_MOQ_76ZN2
      REAL DELT_MOQ_75CU2
      REAL DELT_MOQ_79AS2
      REAL DELT_MOQ_77Ge2
      REAL DELT_MOQ_78Ge2


      REAL gamma,gammat
      REAL gamma1,gamma2
      REAL gamma3,gamma4
      REAL gamma5,gamma6

      REAL gammat1,gammat2
      REAL gammat3,gammat4
      REAL gammat5,gammat6

      REAL gamma_75cu,gamma_76zn
      REAL gamma_77ga,gamma_78ge
      REAL gamma_79as,gamma_77ge

      REAL gammat_75cu,gammat_76zn
      REAL gammat_77ga,gammat_78ge
      REAL gammat_79as,gammat_77ge
      



      Real gammaz,z_t,z_t2
      Real z,z2,aoz_t,aoz
ccc      Real Z_t,Z,Z_t2,Z2,gammaz,aoz_t,aoz,Brho_t


      integer evtnum
      SAVE evtnum

      logical fileend,erflag
      integer evtwnum,blkc,trignum
      common  /anaevtstat/ fileend,erflag,evtwnum,blkc,trignum

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
   
      evtnum = 0
      
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
      val(30,naok) =val(22,naok)+0.5*(val(13,naok) - val(12,naok)) ! F3 Tsum + diff(R-L)/2
      val(32,naok) =0.5*(val(13,naok) - val(12,naok)) ! F3 T diff(R-L)/2
      F3Tsum = val(22,naok)
      F3Qavr = val(26,naok)
      F3TCsum = val(28,naok)
      F3Tminus = val(30,naok)

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


      F3x=val(12,naok)
      F3y=val(14,naok)
      F3a=val(16,naok)
      F3b=val(18,naok)




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
       !val(8,naok) =f6x-3.0763*F3x-0.0*F3a
      !val(9,naok) =f6x-3.0763*F3x-0.48084*F3a
      val(8,naok) =f6x-0.0*F3x-0.0*F3a
      val(9,naok) =f6x-0.0*F3x-0.0*F3a
      DPOP=val(6,naok)
      DPOP2=val(8,naok)/73.753 + 0.0



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

      val(3,naok) = F3Tminus - F2Tsum ! TOF23 +diff(R-L)/2
      val(5,naok) = F7Tsum - F3Tminus ! TOF37+diff(R-L)/2
      val(9,naok) = S0Tsum_R3 - F3Tminus ! TOF3X (E-MCP)+diff(R-L)/2
      val(10,naok) = FH10Tsum_R3 - F3Tminus ! TOF3X (FH10Pla)+diff(R-L)/2
      val(11,naok) = ILC2Tsum - F3Tminus ! TOF3ILC2+diff(R-L)/2

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
       !val(46,naok) = val(16,naok) - (-0.047338) * f6x ! TOF3S0 cor by F6X
      val(46,naok) = val(16,naok) - (-0.040321538) * f6x ! TOF3S0 cor by F6X
      val(48,naok) = val(18,naok) - 1.0000 * f6x ! TOF3ILC2 cor by F6X
      val(50,naok) = val(4,naok) +  0.04848 * F5x ! TOF37 cor by F5X
C      val(50,naok) = val(4,naok) + 0.023729 * F5x ! TOF37 cor by F5X
      val(51,naok) = val(32,naok) + 4.2915 * F5x ! F7IC cor by F5X
c     val(51,naok) = RF - 0.06751 * F5x ! RF cor by F5X
C      val(52,naok) = val(22,naok) - 1.0000 * f6x ! F3dE(IC) cor by F6X
       !val(52,naok) = val(22,naok) - (-0.5595) * f6x ! F3dE(IC) cor by F6X
      val(52,naok) = val(22,naok) - (-0.56951) * f6x ! F3dE(IC) cor by F6X
      val(56,naok) = val(26,naok) - 1.0000 * f6x ! F3dE1(Si1) cor by F6X
      val(58,naok) = val(28,naok) - 1.0000 * f6x ! F3dE2(Si2) cor by F6X
      val(60,naok) = val(22,naok)-val(16,naok)*(-0.90423)-202.204! F3dE(IC) cor by TOF3S0 
      T_F2F3 = val(12,naok)
      T_F3S0=val(16,naok)
      T_F3S0_1=val(9,naok)
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
C  BETA CORRECTION   C
C   ZHUANG    C
cccccccccccccccccc
      !F6X IS THE OFFSET OF F6@X BECASUSE OF MOMENTUM DISPERSION , 
      !DPOP IS DP/P f6x/73.753
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
       MOQ6=2.403314274   !77Ge 2.403314274
       MOQ0=2.434542495   !75CU,76ZN,77GA,78GE,79AS;REF,78GE
       
    
      

      !<name>D1</name> <brho>6.7203</brho> <name>D2</name><brho>6.0859</brho>
      !D3 4.8577 , D4 4.8480 ,D5 4.8480,D6 0.0000 ,D7 4.8500 ,D6 0.000
      !D7 4.8500 , D8 4.8500
      BRHO1=4.8480   !@F6
      lc=60.3507
      BRHO= BRHO1*(1+DPOP*0.01)
      BRHO= BRHO1*(1+DPOP2*0.01)
      val(16,naok)=BRHO
      BRHO2=BRHO*BRHO
      BETA1=1.0/SQRT((MOQ1*MOQ1*MU2/(BRHO2*CC2))+1.0)  !75CU
      BETA2=1.0/SQRT((MOQ2*MOQ2*MU2/(BRHO2*CC2))+1.0)  !76ZN
      BETA3=1.0/SQRT((MOQ3*MOQ3*MU2/(BRHO2*CC2))+1.0)  !77GA
      BETA4=1.0/SQRT((MOQ4*MOQ4*MU2/(BRHO2*CC2))+1.0)  !78GE
      BETA5=1.0/SQRT((MOQ5*MOQ5*MU2/(BRHO2*CC2))+1.0)  !79AS
      BETA6=1.0/SQRT((MOQ6*MOQ6*MU2/(BRHO2*CC2))+1.0)  !77Ge
      !beta center brho
      BETA1c=1.0/SQRT((MOQ1*MOQ1*MU2/(BRHO1*BRHO1*CC2))+1.0)  !75CU
      BETA2c=1.0/SQRT((MOQ2*MOQ2*MU2/(BRHO1*BRHO1*CC2))+1.0)  !76ZN
      BETA3c=1.0/SQRT((MOQ3*MOQ3*MU2/(BRHO1*BRHO1*CC2))+1.0)  !77GA
      BETA4c=1.0/SQRT((MOQ4*MOQ4*MU2/(BRHO1*BRHO1*CC2))+1.0)  !78GE
      BETA5c=1.0/SQRT((MOQ5*MOQ5*MU2/(BRHO1*BRHO1*CC2))+1.0)  !79AS
      BETA6c=1.0/SQRT((MOQ6*MOQ6*MU2/(BRHO1*BRHO1*CC2))+1.0)  !77Ge

      !gammat
      gammat1=SQRT(1.0/(MOQ1*MOQ1*MU2/(BRHO1*BRHO1*CC2))+1.0)
      gammat2=SQRT(1.0/(MOQ2*MOQ2*MU2/(BRHO1*BRHO1*CC2))+1.0)
      gammat3=SQRT(1.0/(MOQ3*MOQ3*MU2/(BRHO1*BRHO1*CC2))+1.0)
      gammat4=SQRT(1.0/(MOQ4*MOQ4*MU2/(BRHO1*BRHO1*CC2))+1.0)
      gammat5=SQRT(1.0/(MOQ5*MOQ5*MU2/(BRHO1*BRHO1*CC2))+1.0) 
      gammat6=SQRT(1.0/(MOQ6*MOQ6*MU2/(BRHO1*BRHO1*CC2))+1.0) 
      !gamma
      gamma1=SQRT(1.0/(MOQ1*MOQ1*MU2/(BRHO2*CC2))+1.0)
      gamma2=SQRT(1.0/(MOQ2*MOQ2*MU2/(BRHO2*CC2))+1.0)
      gamma3=SQRT(1.0/(MOQ3*MOQ3*MU2/(BRHO2*CC2))+1.0)
      gamma4=SQRT(1.0/(MOQ4*MOQ4*MU2/(BRHO2*CC2))+1.0)
      gamma5=SQRT(1.0/(MOQ5*MOQ5*MU2/(BRHO2*CC2))+1.0) 
      gamma6=SQRT(1.0/(MOQ6*MOQ6*MU2/(BRHO2*CC2))+1.0) 

      !gammat ref 78Ge
      gammat=1.0/SQRT((MOQ4*MOQ4*MU2/BRHO1*BRHO1*CC2)+1.0)
      !BETA0= BETA1
      val(13,naok)=gamma1
      val(14,naok)=gamma2
      val(15,naok)=gamma3
      val(9,naok)=gamma4
      val(17,naok)=gamma5
      val(18,naok)=gamma6

      val(19,naok)=BETA1
      val(20,naok)=BETA2
      val(21,naok)=BETA3
      val(22,naok)=BETA4
      val(23,naok)=BETA5
      val(24,naok)=BETA6
      !revolution number and time 
      !NUCLEI     TOFXELC   DOUBLEKIKER            TOFRING         MCP           N
      !78Ge      700752.3+694.3      33.446 +694.3   700718.9    373.415(20)    1878
       !         701446.9            728.09
      !77GA      700822.6+694.3     45.933          700776.7    378.243(30)    1853
      !          701515.8           740.28
      !76ZN      701228.06             57.030+694.3              383.645(20)    1826
      !                               751.26



      !DISPERSION:73.753 MM/%  DPOP=f6x / 73.753 + 0.0 ! F6X to dp/p  DX=73.753mm/%
      !revolution time 
      val(26,naok)=(T_S0_ELC-728.09)/1878.  !revolution time of 78Ge event by event
      val(27,naok)=(T_S0_ELC-740.28)/1853.  !revolution time of 77Ga
      val(28,naok)=(T_S0_ELC-751.26)/1826.  !revolution time of 76Zn
      val(29,naok)=(T_S0_ELC-759.7445)/1799.  !revolution time of 75Cu
      val(31,naok)=(T_S0_ELC-718.6715)/1902.  !revolution time of 79As
      TRV_78GE=val(26,naok)
      TRV_77GA=val(27,naok)
      TRV_76ZN=val(28,naok)
      TRV_75CU=val(29,naok)
      TRV_79AS=val(31,naok)
      
      !ratio of T1/T0, 78GE AS REF
      TR_77Ga1=TRV_77GA/373.1197
      TR_76Zn1=TRV_76ZN/373.1197
      TR_75Cu1=TRV_75Cu/373.1197
      TR_79As1=TRV_79As/373.1197
      TR_78GE1=TRV_78GE/373.1197
      TR_77GE1=TRV_77GE/373.1197   !369.692436
      
       val(34,naok)=TR_77Ga1
       val(35,naok)=TR_76Zn1
       val(60,naok)=TR_75Cu1
       val(61,naok)=TR_79As1
       val(62,naok)=TR_78GE1
      !BETA= 286.58517/(T_F3S0 + 322.16136)
      !BETA = 283.373 /(T_F3S0 + 316.187) ! TOF calib
      !Beta = 291.856/(tof3X + 332.367)
      !I used for beta cal the in 2016(below)
      ! 3point for mass in ring(not Bro corrected not center)
      !BETA= 286.73229/(T_F3S0 + 322.40746)
      
cFinal set of parameters            Asymptotic Standard Error
c five point cal for Ni74
c=======================            ==========================
c a               = 282.508          +/- 0.462        (0.1635%)
cb               = -314.649         +/- 0.868        (0.2759%)
      

      BETA= 282.508 /(T_F3S0 + 314.649)
      BETA_0=287.136/(T_F3S0+323.247)
      val(30,naok)=BETA
      !velocity<--->beta
      BETA_77Ga1=BETA*0.999994928
      BETA_76ZN1=BETA*0.999995618
      BETA_75Cu1=BETA_0*0.999995938
      BETA_78Ge1=BETA*0.999995923
      BETA_79As1=BETA_0*0.999995425
      BETA_77Ge1=BETA*0.999994123
       val(32,naok)=BETA_77Ga1
       val(33,naok)=BETA_76ZN1
       val(63,naok)=BETA_75CU1
       val(64,naok)=BETA_79AS1
       val(65,naok)=BETA_77Ge1
       val(66,naok)=BETA_78Ge1

      !beta correction factor
      BETAC_77Ga1=sqrt((1.-BETA_77GA1**2)/(1.-(TR_77GA1*BETA_77GA1)
     &**2))

      BETAC_76ZN1=sqrt((1.-BETA_76ZN1**2)/(1.-(TR_76ZN1*BETA_76ZN1)
     &**2))

      BETAC_75CU1=sqrt((1.-BETA_75CU1**2)/(1.-(TR_75CU1*BETA_75CU1)
     &**2))

      BETAC_79AS1=sqrt((1.-BETA_79AS1**2)/(1.-(TR_79AS1*BETA_79AS1)
     &**2))     
     
      BETAC_77GE1=sqrt((1.-BETA_77GE1**2)/(1.-(TR_77GE1*BETA_77GE1)
     &**2))

      BETAC_78GE1=sqrt((1.-BETA_78GE1**2)/(1.-(TR_78GE1*BETA_78GE1)
     &**2))

      val(36,naok)=BETAC_77Ga1
      val(37,naok)=BETAC_76ZN1
      val(70,naok)=BETAC_75CU1
      val(71,naok)=BETAC_79AS1
      val(72,naok)=BETAC_77Ge1
      val(73,naok)=BETAC_78Ge1


      !!!circumference of different ions(in mm)
      val(91,naok)=val(32,naok)*val(27,naok)*cc
      val(92,naok)=val(33,naok)*val(28,naok)*cc
      val(93,naok)=val(63,naok)*val(29,naok)*cc
      val(94,naok)=val(64,naok)*val(31,naok)*cc
      !val(94,naok)=val(65,naok)
      val(96,naok)=val(66,naok)*val(26,naok)*cc
       !val(32,naok)=BETA_77Ga1
       !val(33,naok)=BETA_76ZN1
       !val(63,naok)=BETA_75CU1
       !val(64,naok)=BETA_79AS1
       !val(65,naok)=BETA_77Ge1
       !val(66,naok)=BETA_78Ge1
       !TRV_78GE=val(26,naok)
       !TRV_77GA=val(27,naok)
       !TRV_76ZN=val(28,naok)
       !TRV_75CU=val(29,naok)
       !TRV_79AS=val(31,naok)

      
      !CAL MASS  ===>>>m/q
      val(39,naok)=TR_77Ga1*MOQ4*BETAC_77Ga1
      val(40,naok)=TR_76Zn1*MOQ4*BETAC_76ZN1
      val(75,naok)=TR_75CU1*MOQ4*BETAC_75CU1
      val(76,naok)=TR_79AS1*MOQ4*BETAC_79AS1
      val(77,naok)=TR_77Ge1*MOQ4*BETAC_77Ge1
      val(78,naok)=TR_78Ge1*MOQ4*BETAC_78Ge1
            MOQ_77GA1=val(39,naok)
            MOQ_76ZN1=val(40,naok)
            MOQ_75CU1=val(75,naok)
            MOQ_79AS1=val(76,naok)
            MOQ_77Ge1=val(77,naok)
            MOQ_78Ge1=val(78,naok)
            DELT_MOQ_77GA1=(val(39,naok)-MOQ3)/MOQ3
            DELT_MOQ_76ZN1=(val(40,naok)-MOQ2)/MOQ2
            DELT_MOQ_75CU1=(val(75,naok)-MOQ1)/MOQ1
            DELT_MOQ_79AS1=(val(76,naok)-MOQ5)/MOQ5
            DELT_MOQ_77Ge1=(val(77,naok)-MOQ6)/MOQ6
            DELT_MOQ_78Ge1=(val(78,naok)-MOQ4)/MOQ4
         val(41,naok)=DELT_MOQ_77GA1
         val(42,naok)=DELT_MOQ_76ZN1
         val(81,naok)=DELT_MOQ_75CU1
         val(82,naok)=DELT_MOQ_79AS1
         val(83,naok)=DELT_MOQ_77GE1
         val(84,naok)=DELT_MOQ_78GE1


       !REVOLUTION TIME cor
      val(43,naok)=TRV_77GA*BETAC_77Ga1
      val(44,naok)=TRV_76ZN*BETAC_76ZN1
      val(56,naok)=TRV_75Cu*BETAC_75Cu1
      val(57,naok)=TRV_79As*BETAC_79As1
      val(58,naok)=TRV_77Ge*BETAC_77Ge1
      val(59,naok)=TRV_78Ge*BETAC_78Ge1

      !TOF RING cor
      val(45,naok)=T_S0_ELC*BETAC_77Ga1
      val(46,naok)=T_S0_ELC*BETAC_76Zn1
      val(47,naok)=T_S0_ELC*BETAC_75Cu1
      val(48,naok)=T_S0_ELC*BETAC_79As1
      val(87,naok)=T_S0_ELC*BETAC_77GE1
      val(88,naok)=T_S0_ELC*BETAC_78GE1

      !MASS EXCESS(KEV)
       !75CU,76ZN,77GA,78GE,79AS;REF,78GE
       M_E=510.998928
       EM1=45232.21139 !ev
       EM2=49061.56713
       EM3=53074.77229
       EM4=57274.52501
       EM5=61663.50747
       EM6=57274.52501
       EM0=57274.52501

       !val(47,naok)=MOQ_77GA
       !val(48,naok)=MU*

       val(50,naok)=MU*(MOQ_77GA1*31-77)*1000.0-EM3/1000.0+31.*M_E
       val(51,naok)=MU*(MOQ_76ZN1*30-76)*1000.0-EM2/1000.0+30.*M_E
       val(53,naok)=MU*(MOQ_75CU1*29-75)*1000.0-EM1/1000.0+29.*M_E
       val(54,naok)=MU*(MOQ_79AS1*33-79)*1000.0-EM5/1000.0+33.*M_E
       val(91,naok)=MU*(MOQ_77GE1*32-77)*1000.0-EM6/1000.0+32.*M_E
       val(92,naok)=MU*(MOQ_78GE1*32-78)*1000.0-EM4/1000.0+32.*M_E

       ME_77GA1=val(50,naok)
       ME_76ZN1=val(51,naok)
       ME_75CU1=val(53,naok)
       ME_79AS1=val(54,naok)
       ME_77GE1=val(91,naok)
       ME_78GE1=val(92,naok)
       
      

     
      
cccccccccccccccccc
c ADDED IN 20170301 for Mass calc. c
C   ZHUANG    C
C  BRHO TOF  C
cccccccccccccccccc


ccccccccccccccccccccccc
ccc
c MAking gate circle
c
c
cccREAL T_F3S0,F3dE,T_F2F3
ccccccccccccccccccccccc


ccccccF3S0tof mean  F3IC mean

ccccccF2F3tof mean  F3IC mean
c
ccc 

      ID = 700
      
      naok = naok +1 
      
      val(1,naok) = id

ccc   241.640.778743565.540.037800 74Ni


      val(2,naok) = sqrt(((T_F3S0-241.64)/0.77874)**2+
     &((F3dE-3565.5)/40.037)**2)
      val(3,naok) = sqrt(((T_F2F3-51.78)/0.16838)**2+
     &((F3dE-3561.1)/38.85)**2)


c     209.210.776114344.636.29480as not correct
c     51.780.1018436039.55780as9220
      val(4,naok) = sqrt(((T_F3S0-209.21)/0.77611)**2+
     &((F3dE-4344.6)/36.294)**2)
      val(5,naok) = sqrt(((T_F2F3-51.78)/0.1018)**2
     &+((F3dE-4360)/39.557)**2)
c     202.270.631464282.332.01680179As
c     51.210.12314284.335.5579As43600
      val(6,naok) = sqrt(((T_F3S0-202.27)/0.63146)**2+
     &((F3dE-4282.3)/32.016)**2)
      val(7,naok) = sqrt(((T_F2F3-51.21)/0.1231)**2+
     &((F3dE-4284.3)/35.55)**2)
c     216.610.73574228.438.58880579Ge
c     52.3680.109684233.636.14479Ge31214
      val(8,naok) = sqrt(((T_F3S0-216.61)/0.7357)**2+
     &((F3dE-4228.4)/38.588)**2)
      val(9,naok) = sqrt(((T_F2F3-52.368)/0.10968)**2+
     &((F3dE-4233.6)/36.144)**2)
c     208.760.649644137.937.73280478Ge
c     51.770.1224139.735.7378Ge1225614
      val(10,naok) = sqrt(((T_F3S0-208.76)/0.64964)**2+
     &((F3dE-4137.9)/37.732)**2)
      val(11,naok) = sqrt(((T_F2F3-51.77)/0.122)**2+
     &((F3dE-4139.7)/35.73)**2)
c     200.970.627084049.438.30380377Ge
c     51.1680.12204404534.77977Ge371438
      val(12,naok) = sqrt(((T_F3S0-200.97)/0.62708)**2+
     &((F3dE-4049.4)/38.303)**2)
      val(13,naok) = sqrt(((T_F2F3-51.168)/0.12204)**2+
     &((F3dE-4045.0)/34.779)**2)
c     193.690.56244046.630.89180276Ge not exist contamination

c      val(14,naok) = sqrt(((T_F3S0-193.69)/0.5624)**2+
c     &((F3dE-4046.6)/30.891)**2)
c      val(15,naok) = sqrt(((T_F2F3-51.78)/0.16838)**2+
c     &((F3dE-3565.5)/40.037)**2)
c     79ga
c22   4.160.688944089.639.52480878Ga
c     53.0690.12264093.239.84778Ga20387
      val(16,naok) = sqrt(((T_F3S0-224.16)/0.68894)**2+
     &((F3dE-4089.6)/39.524)**2)
      val(17,naok) = sqrt(((T_F2F3-53.069)/0.1226)**2+
     &((F3dE-4093.2)/39.847)**2)
c     215.860.676033991.939.15580777Ga
c     52.3920.121123992.139.46377Ga 807250
      val(18,naok) = sqrt(((T_F3S0-215.86)/0.67603)**2+
     &((F3dE-3991.9)/39.155)**2)
      val(19,naok) = sqrt(((T_F2F3-52.392)/0.12112)**2+
     &((F3dE-3992.1)/39.463)**2)
c     207.760.663393900.940.21380676ga
c     51.7530.126783897.337.31876ga 280120
      val(20,naok) = sqrt(((T_F3S0-207.76)/0.66339)**2+
     &((F3dE-3900.9)/40.213)**2)
      val(21,naok) = sqrt(((T_F2F3-51.753)/0.12678)**2+
     &((F3dE-3897.3)/37.318)**2)
c     75ga
c     232.220.695673943.941.2881177zn
c     53.7440.109513948.338.9430.38787848677zn24344
      val(22,naok) = sqrt(((T_F3S0-232.22)/0.69567)**
     &2+((F3dE-3943.9)/41.28)**2)
      val(23,naok) = sqrt(((T_F2F3-53.744)/0.10951)**2+
     &((F3dE-3948.3)/37.318)**2)
c22   3.680.710193847.240.50581076Zn
c     53.080.139153847.938.64176Zn667987
      val(24,naok) = sqrt(((T_F3S0-223.68)/0.71019)**2+
     &((F3dE-3847.2)/40.505)**2)
      val(25,naok) = sqrt(((T_F2F3-53.08)/0.13915)**2+
     &((F3dE-3847.9)/38.641)**2)
c     215.210.668243750.739.03280975Zn
c     52.3860.13095375038.31975Zn64871
      val(26,naok) = sqrt(((T_F3S0-215.21)/0.66824)**2+
     &((F3dE-3750.7)/39.032)**2)
      val(27,naok) = sqrt(((T_F2F3-52.386)/0.13095)**2+
     &((F3dE-3750)/38.319)**2)
c     74zn
c     
c     241.220.711553809.139.29281476cu
c     54.4970.143333807.738.66576cu 1690

      val(28,naok) = sqrt(((T_F3S0-241.22)/0.71155)**2+
     &((F3dE-3809.1)/39.292)**2)
      val(29,naok) = sqrt(((T_F2F3-54.497)/0.14333)**2+
     &((F3dE-3807.7)/38.665)**2)
c     232.230.725373705.239.10381375Cu
c     53.8080.135073703.937.34475Cu74668
      val(30,naok) = sqrt(((T_F3S0-232.23)/0.72537)**2+
     &((F3dE-3705.2)/39.103)**2)
      val(31,naok) = sqrt(((T_F2F3-53.808)/0.13507)**2+
     &((F3dE-3703.9)/37.344)**2)
c22   3.510.778133608.740.86881274cu
      val(32,naok) = sqrt(((T_F3S0-223.51)/0.77813)**2+
     &((F3dE-3608.7)/40.868)**2)
      val(33,naok) = sqrt(((T_F2F3-51.78)/0.16838)**2+
     &((F3dE-3565.5)/40.037)**2)
c     73cu
c     
c     
c     75ni
c     241.640.778743565.540.03780074Ni
c     54.5680.168383561.138.84974Ni3389

c     232.510.8396 3470.741.51773ni
c     53.8610.13743347035.45773ni 2679
      val(34,naok) = sqrt(((T_F3S0-232.51)/0.8396)**2+
     &((F3dE-3470.7)/41.517)**2)
      val(35,naok) = sqrt(((T_F2F3-53.861)/0.13743)**2+
     &((F3dE-3470)/35.457)**2)
c22   4.220.783373392.245.24772ni
c     53.2130.150563372.242.81972ni 523
      val(36,naok) = sqrt(((T_F3S0-224.22)/0.78337)**2+
     &((F3dE-3392.2)/45.247)**2)
      val(37,naok) = sqrt(((T_F2F3-53.213)/0.15056)**2+
     &((F3dE-3372.2)/42.819)**2)




cccccccccccccccccccccccc
c TOF cor F6x old
c gate on
c  F6x corr 20170414
C TOF corr2
cccccccccccccccccccccccc

ccc
ccc 74Ni 3389c
ccc mean 240.77 +-0.012411 ns
      ID = 800
      
      naok = naok +1 

      val(1,naok) = id
cf6x cor
      val(2,naok) = T_F3S0-(-0.49391E-01*f6x+ (-0.22638E-05)*f6x**2)
cf6x cor2
      val(3,naok) = T_F3S0-(-4.83322E-02*f6x+ (-1.25164E-04)*f6x**2)
cf3a cor      
      val(4,naok) = val(2,naok)*1.
cccccc 74Ni
c new
c$$$$$$
c corr f6x by F3x and F3a
c$$$$$$
cf6x by F3x
      val(10,naok) = F6x-(3.0763*f3x)
cf6x by F3a
      val(11,naok) = F6x-(0.7248*f3a)
      val(12,naok) = F6x-((0.72345)*f3a + (0.00063883)*f3a**2)
cf6x by F3x and F3a(first order)
      val(13,naok) = F6x-(3.0763*f3x)-(0.7248*f3a)
cf6x by F3x and F3a(second order) !!!!use 
      val(14,naok) = F6x-(3.0763*f3x)-(0.72345*f3a + (0.00063883)
     &*f3a**2)

c$$$$$$
c corr f6x by TOF_F3S0
cccc
      val(50,naok) = F6x-((1/(-0.049392))*(T_F3S0-240.77))


c new
c$$$$$$
c TOF F3s0 by F3x, F3a, f6x cor2
c$$$$$$
cf3x cor 
      val(16,naok) = T_F3S0-(-0.11102*f3x)
cf3a cor      
      val(18,naok) = T_F3S0-(-0.025403*f3a)
c f3x and f3a cor      !!!use 
      val(20,naok) = T_F3S0-(-0.11102*f3x+(-0.025403)*f3a)
cF3s0 F6x cor 1st order -0.049392	240.77
      val(21,naok) = T_F3S0-(-0.049392*f6x)
cF3s0 F6x cor old 2nd order old
      val(22,naok) = T_F3S0-(-0.49391E-01*f6x+
     &(-0.22638E-05)*f6x**2)

ccccccccccccc
c F3s0 by F3x, F3a, f6x cor2 
cF3s0 F6x cor 1st order (F6x old)
      val(24,naok) = val(20,naok)-(-0.049392*f6x)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(F3x 1st F3a 1st)  cor  1st order 
      val(25,naok) = val(20,naok)-(-0.049392*val(13,naok))
cF3s0 F6x cor(F3x 1st F3a 2nd)  1st order !use !!!use !!!final
      val(26,naok) = val(20,naok)-(-0.049392*val(14,naok))
cF3s0 F6x cor(F3x 1st only)  1st order !use !!!use !!!final2
      val(27,naok) = val(20,naok)-(-0.049392*val(10,naok))

cccccccc
c F3s0 by F3x, F3a, f6x cor2
cF3s0 F6x cor  2nd order (F6x old)
      val(28,naok) = val(20,naok)-(-0.49391E-01*f6x+ 
     &(-0.22638E-05)*f6x**2)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(F3x 1st F3a 1st)  cor  2nd order 
      val(30,naok) = val(20,naok)-(-0.49391E-01*val(13,naok)+ 
     &(-0.22638E-05)*val(13,naok)**2)
cF3s0 F6x cor(F3x 1st F3a 2nd)  2nd order !use
      val(32,naok) = val(20,naok)-(-0.49391E-01*val(14,naok)+ 
     &(-0.22638E-05)*val(14,naok)**2)

cF3s0 F6x cor(F3x 1st only)  2nd order !use
      val(34,naok) = val(20,naok)-(-0.49391E-01*val(10,naok)+ 
     &(-0.22638E-05)*val(10,naok)**2)

ccccccccccccccccccccccccccccccc

ccc
ccc 79As  43600c
ccc   202.29 +-  0.18403E-02 ns
      ID = 801
      
      naok = naok +1 

      val(1,naok) = id
cf6x cor
      val(2,naok) = T_F3S0-(-0.44422E-01*f6x+(-0.74401E-04)*f6x**2)
cf6x cor2
      val(3,naok) = T_F3S0-(-4.31E-02*f6x+(-1.01E-04)*f6x**2)
cf3a cor      
      val(4,naok) = val(2,naok)*1


cccccc  79As 
c new
c$$$$$$
c corr f6x by F3x and F3a
c$$$$$$
cf6x by F3x
      val(10,naok) = F6x-(2.539*f3x)
cf6x by F3a
      val(11,naok) = F6x-(0.37363*f3a)
      val(12,naok) = F6x-((0.36106)*f3a + (-0.007772)*f3a**2)
cf6x by F3x and F3a(first order)
      val(13,naok) = F6x-(2.539*f3x)-(0.37363*f3a)
cf6x by F3x and F3a(second order) !!!use 
      val(14,naok) = F6x-(2.539*f3x)-(0.36106*f3a + (-0.007772)
     &*f3a**2)

c$$$$$$
c corr f6x by TOF_F3S0
cccc -0.041782	202.29
      val(50,naok) = F6x-((1/(-0.041782))*(T_F3S0-202.29))

c new
c$$$$$$
c TOF F3s0 by F3x, F3a, f6x cor2
c$$$$$$
cf3x cor 
      val(16,naok) = T_F3S0-(-0.095286*f3x)
cf3a cor      
      val(18,naok) = T_F3S0-(-0.030789*f3a)
c f3x and f3a cor     !!!use 
      val(20,naok) = T_F3S0-(-0.095286*f3x+(-0.030789)*f3a)
cF3s0 F6x cor 1st order 
      val(21,naok) = T_F3S0-(-0.041782*f6x)
cF3s0 F6x cor old 2nd order old
      val(22,naok) = T_F3S0-(-0.04179*f6x+
     $(-0.000006481)*f6x**2)
ccccccc

ccccccccccccc
c F3s0 by F3x, F3a, f6x cor2   1st order
ccccccccccccc
cF3s0 F6x cor 1st order (F6x old)
      val(24,naok) = val(20,naok)-(-0.041782*f6x)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(cor F3x 1st F3a 1st)  cor  1st order 
      val(25,naok) = val(20,naok)-(-0.041782*val(13,naok))
cF3s0 F6x ( corF3x 1st F3a 2nd) cor 1st order !use !!!use !!!final
      val(26,naok) = val(20,naok)-(-0.041782*val(14,naok))
cF3s0 F6x cor(F3x 1st only)  1st order !use !!!use !!!final2
      val(27,naok) = val(20,naok)-(-0.041782*val(10,naok))

cccccccccccc
c F3s0 by F3x, F3a, f6x cor2  2nd order
cccccccccccc
cF3s0 F6x cor  2nd order (F6x old)
      val(28,naok) = val(20,naok)-(-0.04179*f6x+ 
     &(-0.000006481)*f6x**2)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(F3x 1st F3a 1st)  cor  2nd order 
      val(30,naok) = val(20,naok)-(-0.04179*val(13,naok)+ 
     &(-0.000006481)*val(13,naok)**2)
cF3s0 F6x cor(F3x 1st F3a 2nd)  2nd order !use
      val(32,naok) = val(20,naok)-(-0.04179*val(14,naok)+ 
     &(-0.000006481)*val(14,naok)**2)
cF3s0 F6x cor(F3x 1st only)  2nd order !use
      val(34,naok) = val(20,naok)-(-0.04179*val(10,naok)+ 
     &(-0.000006481)*val(10,naok)**2)

ccccccccccccccccccccccccccccccc

ccc
ccc 76Ge  12011c  not used 
ccc   196.68       0.22388E-01 ns
      ID = 802
      
      naok = naok +1 

      val(1,naok) = id
cf6x cor
      val(2,naok) = T_F3S0-(-0.44177E-01*f6x+(0.56774E-05)*f6x**2)
cf6x cor2
      val(3,naok) = T_F3S0-(-0.44177E-01*f6x+(0.56774E-05)*f6x**2)
cf3a cor      
      val(4,naok) = val(2,naok)

ccccccccccccccccccccccccccccccc

ccc
ccc 77Ge   371438c
ccc   203.73       0.42111E-02 ns
      ID = 803
      
      naok = naok +1 

      val(1,naok) = id
cf6x cor
      val(2,naok) = T_F3S0-(-0.47778E-01*f6x+(0.40069E-04)*f6x**2)
cf6x cor2
      val(3,naok) = T_F3S0-(-3.96183E-02*f6x+(-2.18680E-05)*f6x**2)
cf3a cor      
      val(4,naok) = val(2,naok)



cccccc  77Ge 
c new
c$$$$$$
c corr f6x by F3x and F3a
c$$$$$$
cf6x by F3x
      val(10,naok) = F6x-(2.4231*f3x)
cf6x by F3a
      val(11,naok) = F6x-(-0.15428*f3a)
      val(12,naok) = F6x-((0.013484)*f3a + (-0.042673)*f3a**2)
cf6x by F3x and F3a(first order)
      val(13,naok) = F6x-(2.4231*f3x)-(-0.15428*f3a)
cf6x by F3x and F3a(second order) !!!use 
      val(14,naok) = F6x-(2.4231*f3x)-((0.013484)*f3a
     &+ (-0.042673)*f3a**2)

c$$$$$$
c corr f6x by TOF_F3S0
cccc  -0.042079	203.54
      val(50,naok) = F6x-((1/(-0.042079))*(T_F3S0-203.54))
cccc


c new
c$$$$$$
c TOF F3s0 by F3x, F3a, f6x cor2
c$$$$$$
cf3x cor 
      val(16,naok) = T_F3S0-(-0.068705*f3x)
cf3a cor      
      val(18,naok) = T_F3S0-(-0.014797*f3a)
c f3x and f3a cor     !!!use 
      val(20,naok) = T_F3S0-(-0.068705*f3x+(-0.014797)*f3a)
cF3s0 F6x cor 1st order 
      val(21,naok) = T_F3S0-(-0.042079*f6x)
cF3s0 F6x cor old 2nd order old
      val(22,naok) = T_F3S0-(-0.045547*f6x+
     $(2.79E-05)*f6x**2)
ccccccc

ccccccccccccc
c F3s0 by F3x, F3a, f6x cor2   1st order
ccccccccccccc
cF3s0 F6x cor 1st order (F6x old)
      val(24,naok) = val(20,naok)-(-0.042079*f6x)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(cor F3x 1st F3a 1st)  cor  1st order 
      val(25,naok) = val(20,naok)-(-0.042079*val(13,naok))
cF3s0 F6x ( corF3x 1st F3a 2nd) cor 1st order !use !!!use !!!final
      val(26,naok) = val(20,naok)-(-0.042079*val(14,naok))
cF3s0 F6x cor(F3x 1st only)  1st order !use !!!use !!!final2
      val(27,naok) = val(20,naok)-(-0.042079*val(10,naok))
cccccccccccc
c F3s0 by F3x, F3a, f6x cor2  2nd order
cccccccccccc
cF3s0 F6x cor  2nd order (F6x old)
      val(28,naok) = val(20,naok)-(-0.045547*f6x+ 
     &(2.79E-05)*f6x**2)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(F3x 1st F3a 1st)  cor  2nd order 
      val(30,naok) = val(20,naok)-(-0.045547*val(13,naok)+ 
     &(2.79E-05)*val(13,naok)**2)
cF3s0 F6x cor(F3x 1st F3a 2nd)  2nd order !use
      val(32,naok) = val(20,naok)-(-0.045547*val(14,naok)+ 
     &(2.79E-05)*val(14,naok)**2)
cF3s0 F6x cor(F3x 1st only)  2nd order !use
      val(34,naok) = val(20,naok)-(-0.045547*val(10,naok)+ 
     &(2.79E-05)*val(10,naok)**2)
ccccccccccccccccccccccccccccccc

ccc
ccc 78Ge 1225614c
ccc   208.89       0.26154E-03 ns
      ID = 804
      
      naok = naok +1 

      val(1,naok) = id
cf6x cor
      val(2,naok) = T_F3S0-(-0.46052E-01*f6x+(-0.57644E-06)*f6x**2)
cf6x cor2
      val(3,naok) = T_F3S0-(-4.46233E-02*f6x+(5.82026E-05)*f6x**2)
cf3a cor      
      val(4,naok) = val(2,naok)



cccccc  78Ge 
c new
c$$$$$$
c corr f6x by F3x and F3a
c$$$$$$
cf6x by F3x
      val(10,naok) = F6x-(2.7487*f3x)
cf6x by F3a
      val(11,naok) = F6x-(0.42846*f3a)
      val(12,naok) = F6x-((0.43385)*f3a + (-0.0083809)*f3a**2)
cf6x by F3x and F3a(first order)
      val(13,naok) = F6x-(2.7487*f3x)-(0.42846*f3a)
cf6x by F3x and F3a(second order) !!!use 
      val(14,naok) = F6x-(2.7487*f3x)-((0.43385)*f3a
     &+ (-0.0083809)*f3a**2)

c$$$$$$
c corr f6x by TOF_F3S0
cccc -0.044268	208.88
      val(50,naok) = F6x-((1/(-0.044268))*(T_F3S0-208.88))
cccc


c new
c$$$$$$
c TOF F3s0 by F3x, F3a, f6x cor2 
c$$$$$$
cf3x cor 
      val(16,naok) = T_F3S0-(-0.096729*f3x)
cf3a cor      
      val(18,naok) = T_F3S0-(-0.026809*f3a)
c f3x and f3a cor     !!!use 
      val(20,naok) = T_F3S0-(-0.096729*f3x+(-0.026809)*f3a)
cF3s0 F6x cor 1st order old f6x
      val(21,naok) = T_F3S0-(-0.044268*f6x)
cF3s0 F6x cor old 2nd order old f6x
      val(22,naok) = T_F3S0-(-0.04422*f6x+
     $(-1.52E-05)*f6x**2)
ccccccc

ccccccccccccc
c F3s0 by F3x, F3a, f6x cor2   1st order
ccccccccccccc
cF3s0 F6x cor 1st order (F6x old)
      val(24,naok) = val(20,naok)-(-0.044268*f6x)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(cor F3x 1st F3a 1st)  cor  1st order 
      val(25,naok) = val(20,naok)-(-0.044268*val(13,naok))
cF3s0 F6x ( corF3x 1st F3a 2nd) cor 1st order !use !!!use !!!final
      val(26,naok) = val(20,naok)-(-0.044268*val(14,naok))
cF3s0 F6x cor(F3x 1st only)  1st order !use !!!use !!!final2
      val(27,naok) = val(20,naok)-(-0.044268*val(10,naok))
cccccccccccc
c F3s0 by F3x, F3a, f6x cor2  2nd order
cccccccccccc
cF3s0 F6x cor  2nd order (F6x old)
      val(28,naok) = val(20,naok)-(-0.04422*f6x+ 
     &(-1.52E-05)*f6x**2)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(F3x 1st F3a 1st)  cor  2nd order 
      val(30,naok) = val(20,naok)-(-0.04422*val(13,naok)+ 
     &(-1.52E-05)*val(13,naok)**2)
cF3s0 F6x cor(F3x 1st F3a 2nd)  2nd order !use
      val(32,naok) = val(20,naok)-(-0.04422*val(14,naok)+ 
     &(-1.52E-05)*val(14,naok)**2)

cF3s0 F6x cor(F3x 1st only)  2nd order !use
      val(34,naok) = val(20,naok)-(-0.04422*val(10,naok)+ 
     &(-1.52E-05)*val(10,naok)**2)
ccccccccccccccccccccccccccccccc

ccc
ccc 79Ge  31214c
ccc  214.36       0.12932E-01
      ID = 805
      
      naok = naok +1 

      val(1,naok) = id
cf6x cor
      val(2,naok) = T_F3S0-(-0.36635E-01*f6x+0.66307E-04*f6x**2)
cf6x cor2
      val(3,naok) = T_F3S0-(-4.53261E-02*f6x+(-4.96100E-06)*f6x**2)
cf3a cor      
      val(4,naok) = val(2,naok)




cccccc  79Ge 
c new
c$$$$$$
c corr f6x by F3x and F3a
c$$$$$$
cf6x by F3x
      val(10,naok) = F6x-(2.9687*f3x)
cf6x by F3a
c1st
      val(11,naok) = F6x-(0.28091*f3a)
c2nd
      val(12,naok) = F6x-((0.37229)*f3a + (0.051536)*f3a**2)
cf6x by F3x and F3a(first order)
      val(13,naok) = F6x-(2.9687*f3x)-(0.28091*f3a)
cf6x by F3x and F3a(second order) !!!use 
      val(14,naok) = F6x-(2.9687*f3x)-((0.37229)*f3a
     &+ (0.051536)*f3a**2)

c$$$$$$
c corr f6x by TOF_F3S0
cccc -0.04287	214.24
      val(50,naok) = F6x-((1/(-0.04287))*(T_F3S0-214.24))
cccc

c new
c$$$$$$
c TOF F3s0 by F3x, F3a, f6x cor2  seperately
c$$$$$$
cf3x cor 
      val(16,naok) = T_F3S0-(-0.14682*f3x)
cf3a cor      
      val(18,naok) = T_F3S0-(-0.014657*f3a)
c f3x and f3a cor     !!!use 
      val(20,naok) = T_F3S0-(-0.14682*f3x+(-0.014657)*f3a)
cF3s0 F6x cor 1st order old f6x
      val(21,naok) = T_F3S0-(-0.04287*f6x)
cF3s0 F6x cor old 2nd order old f6x
      val(22,naok) = T_F3S0-(-0.034877*f6x+
     $(7.14E-05)*f6x**2)
ccccccc

ccccccccccccc
c F3s0 by F3x, F3a, f6x cor2   1st order
ccccccccccccc
cF3s0 F6x cor 1st order (F6x old)
      val(24,naok) = val(20,naok)-(-0.04287*f6x)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(cor F3x 1st F3a 1st)  cor  1st order 
      val(25,naok) = val(20,naok)-(-0.04287*val(13,naok))
cF3s0 F6x ( corF3x 1st F3a 2nd) cor 1st order !use !!!use !!!final
      val(26,naok) = val(20,naok)-(-0.04287*val(14,naok))
cF3s0 F6x cor(F3x 1st only)  1st order !use !!!use !!!final2
      val(27,naok) = val(20,naok)-(-0.04287*val(10,naok))
cccccccccccc
c F3s0 by F3x, F3a, f6x cor2  2nd order
cccccccccccc
cF3s0 F6x cor  2nd order (F6x old)
      val(28,naok) = val(20,naok)-(-0.034877*f6x+ 
     &(7.14E-05)*f6x**2)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(F3x 1st F3a 1st)  cor  2nd order 
      val(30,naok) = val(20,naok)-(-0.034877*val(13,naok)+ 
     &(7.14E-05)*val(13,naok)**2)
cF3s0 F6x cor(F3x 1st F3a 2nd)  2nd order !use
      val(32,naok) = val(20,naok)-(-0.034877*val(14,naok)+ 
     &(7.14E-05)*val(14,naok)**2)
cF3s0 F6x cor(F3x 1st only)  2nd order !use
      val(34,naok) = val(20,naok)-(-0.034877*val(10,naok)+ 
     &(7.14E-05)*val(10,naok)**2)

ccccccccccccccccccccccccccccccc

ccc
ccc 76Ga  280120c
ccc  210.77       0.47487E-02 ns

      ID = 806
      
      naok = naok +1 

      val(1,naok) = id
cf6x cor
      val(2,naok) = T_F3S0-( -0.52096E-01*f6x+ 0.53806E-04*f6x**2)
cf6x cor2
      val(3,naok) = T_F3S0-( -2.98350E-02*f6x+ (-1.11044E-04)*f6x**2)
cf3a cor      
      val(4,naok) = val(2,naok)

cccccc  76Ga
c new
c$$$$$$
c corr f6x by F3x and F3a
c$$$$$$
cf6x by F3x
      val(10,naok) = F6x-(2.6051*f3x)
cf6x by F3a
c1st
      val(11,naok) = F6x-(0.071929*f3a)
c2nd
      val(12,naok) = F6x-((0.2001)*f3a + (-0.05446)*f3a**2)
cf6x by F3x and F3a(first order)
      val(13,naok) = F6x-(2.6051*f3x)-(0.071929*f3a)
cf6x by F3x and F3a(second order) !!!use 
      val(14,naok) = F6x-(2.6051*f3x)-((0.2001)*f3a
     &+ (-0.05446)*f3a**2)

c$$$$$$
c corr f6x by TOF_F3S0
cccc -0.045162	210.56
      val(50,naok) = F6x-((1/(-0.045162))*(T_F3S0-210.56))
cccc


c new
c$$$$$$
c TOF F3s0 by F3x, F3a, f6x cor2  seperately
c$$$$$$
cf3x cor 
      val(16,naok) = T_F3S0-(-0.08913*f3x)
cf3a cor      
      val(18,naok) = T_F3S0-(-0.021219*f3a)
c f3x and f3a cor     !!!use 
      val(20,naok) = T_F3S0-(-0.08913*f3x+(-0.021219)*f3a)
cF3s0 F6x cor 1st order old f6x
      val(21,naok) = T_F3S0-(-0.045162*f6x)
cF3s0 F6x cor old 2nd order old f6x
      val(22,naok) = T_F3S0-(-0.051942*f6x+
     $(0.000055755)*f6x**2)
ccccccc

ccccccccccccc
c F3s0 by F3x, F3a, f6x cor2   1st order
ccccccccccccc
cF3s0 F6x cor 1st order (F6x old)
      val(24,naok) = val(20,naok)-(-0.045162*f6x)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(cor F3x 1st F3a 1st)  cor  1st order 
      val(25,naok) = val(20,naok)-(-0.045162*val(13,naok))
cF3s0 F6x ( corF3x 1st F3a 2nd) cor 1st order !use !!!use !!!final
      val(26,naok) = val(20,naok)-(-0.045162*val(14,naok))
cF3s0 F6x cor(F3x 1st only)  1st order !use !!!use !!!final2
      val(27,naok) = val(20,naok)-(-0.045162*val(10,naok))
cccccccccccc
c F3s0 by F3x, F3a, f6x cor2  2nd order
cccccccccccc
cF3s0 F6x cor  2nd order (F6x old)
      val(28,naok) = val(20,naok)-(-0.051942*f6x+ 
     &(0.000055755)*f6x**2)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(F3x 1st F3a 1st)  cor  2nd order 
      val(30,naok) = val(20,naok)-(-0.051942*val(13,naok)+ 
     &(0.000055755)*val(13,naok)**2)
cF3s0 F6x cor(F3x 1st F3a 2nd)  2nd order !use
      val(32,naok) = val(20,naok)-(-0.051942*val(14,naok)+ 
     &(0.000055755)*val(14,naok)**2)
cF3s0 F6x cor(F3x 1st only)  2nd order !use
      val(34,naok) = val(20,naok)-(-0.051942*val(10,naok)+ 
     &(0.000055755)*val(10,naok)**2)
ccccccccccccccccccccccccccccccc

ccc
ccc 77Ga  807250c
ccc  215.98       0.34696E-03ns
      ID = 807
      
      naok = naok +1 

      val(1,naok) = id
cf6x cor
      val(2,naok) = T_F3S0-( -0.47419E-01*f6x+(-0.24764E-04)*f6x**2)
cf6x cor2
      val(3,naok) = T_F3S0-( -4.58288E-02*f6x+(2.20430E-05)*f6x**2)
cf3a cor      
      val(4,naok) = val(2,naok)


cccccc  77Ga
c new
c$$$$$$
c corr f6x by F3x and F3a
c$$$$$$
cf6x by F3x
      val(10,naok) = F6x-(2.8735*f3x)
cf6x by F3a
c1st
      val(11,naok) = F6x-(0.51254*f3a)
c2nd
      val(12,naok) = F6x-((0.53164)*f3a + (-0.0072485)*f3a**2)
cf6x by F3x and F3a(first order)
      val(13,naok) = F6x-(2.8735*f3x)-(0.51254*f3a)
cf6x by F3x and F3a(second order) !!!use 
      val(14,naok) = F6x-(2.8735*f3x)-((0.53164)*f3a
     &+ (-0.0072485)*f3a**2)

c$$$$$$
c corr f6x by TOF_F3S0
cccc -0.04536	215.97
      val(50,naok) = F6x-((1/(-0.04536))*(T_F3S0-215.97))
cccc


c new
c$$$$$$
c TOF F3s0 by F3x, F3a, f6x cor2  seperately
c$$$$$$
cf3x cor 
      val(16,naok) = T_F3S0-(-0.10697*f3x)
cf3a cor      
      val(18,naok) = T_F3S0-(-0.028*f3a)
c f3x and f3a cor     !!!use 
      val(20,naok) = T_F3S0-(-0.10697*f3x+(-0.028)*f3a)
cF3s0 F6x cor 1st order old f6x
      val(21,naok) = T_F3S0-(-0.04536*f6x)
cF3s0 F6x cor old 2nd order old f6x
      val(22,naok) = T_F3S0-(-0.045253*f6x+
     $(-0.000032074)*f6x**2)
ccccccc

ccccccccccccc
c F3s0 by F3x, F3a, f6x cor2   1st order
ccccccccccccc
cF3s0 F6x cor 1st order (F6x old)
      val(24,naok) = val(20,naok)-(-0.04536*f6x)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(cor F3x 1st F3a 1st)  cor  1st order 
      val(25,naok) = val(20,naok)-(-0.04536*val(13,naok))
cF3s0 F6x ( corF3x 1st F3a 2nd) cor 1st order !use !!!use !!!final
      val(26,naok) = val(20,naok)-(-0.04536*val(14,naok))
cF3s0 F6x cor(F3x 1st only)  1st order !use !!!use !!!final2
      val(27,naok) = val(20,naok)-(-0.04536*val(10,naok))

cccccccccccc
c F3s0 by F3x, F3a, f6x cor2  2nd order
cccccccccccc
cF3s0 F6x cor  2nd order (F6x old)
      val(28,naok) = val(20,naok)-(-0.045253*f6x+ 
     &(-0.000032074)*f6x**2)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(F3x 1st F3a 1st)  cor  2nd order 
      val(30,naok) = val(20,naok)-(-0.045253*val(13,naok)+ 
     &(-0.000032074)*val(13,naok)**2)
cF3s0 F6x cor(F3x 1st F3a 2nd)  2nd order !use
      val(32,naok) = val(20,naok)-(-0.045253*val(14,naok)+ 
     &(-0.000032074)*val(14,naok)**2)
cF3s0 F6x cor(F3x 1st only)  2nd order !use
      val(34,naok) = val(20,naok)-(-0.045253*val(10,naok)+ 
     &(-0.000032074)*val(10,naok)**2)
ccccccccccccccccccccccccccccccc

ccc
ccc 78Ga  20387c
ccc   221.70       0.14368E-01ns
      ID = 808
      
      naok = naok +1 

      val(1,naok) = id
cf6x cor
      val(2,naok) = T_F3S0-(-0.39656E-01*f6x+0.53046E-04*f6x**2)
cf6x cor2
      val(3,naok) = T_F3S0-(-2.92520E-02*f6x+1.29146E-04*f6x**2)
cf3a cor      
      val(4,naok) = val(2,naok)



cccccc  78Ga
c new
c$$$$$$
c corr f6x by F3x and F3a
c$$$$$$
cf6x by F3x
      val(10,naok) = F6x-(3.6012*f3x)
cf6x by F3a
c1st
      val(11,naok) = F6x-(0.72808*f3a)
c2nd
      val(12,naok) = F6x-((0.56689)*f3a + (0.049689)*f3a**2)
cf6x by F3x and F3a(first order)
      val(13,naok) = F6x-(3.6012*f3x)-(0.72808*f3a)
cf6x by F3x and F3a(second order) !!!use 
      val(14,naok) = F6x-(3.6012*f3x)-((0.56689)*f3a
     &+ (0.049689)*f3a**2)

c$$$$$$
c corr f6x by TOF_F3S0
cccc-0.042877	221.7
      val(50,naok) = F6x-((1/(-0.042877))*(T_F3S0-221.7))
cccc


c new
c$$$$$$
c TOF F3s0 by F3x, F3a, f6x cor2  seperately
c$$$$$$
cf3x cor 
      val(16,naok) = T_F3S0-(-0.13563*f3x)
cf3a cor      
      val(18,naok) = T_F3S0-(-0.016509*f3a)
c f3x and f3a cor     !!!use 
      val(20,naok) = T_F3S0-(-0.13563*f3x+(-0.016509)*f3a)
cF3s0 F6x cor 1st order old f6x
      val(21,naok) = T_F3S0-(-0.042877*f6x)
cF3s0 F6x cor old 2nd order old f6x
      val(22,naok) = T_F3S0-(-0.030608*f6x+
     $(1.08E-04)*f6x**2)
ccccccc

ccccccccccccc
c F3s0 by F3x, F3a, f6x cor2   1st order
ccccccccccccc
cF3s0 F6x cor 1st order (F6x old)
      val(24,naok) = val(20,naok)-(-0.042877*f6x)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(cor F3x 1st F3a 1st)  cor  1st order 
      val(25,naok) = val(20,naok)-(-0.042877*val(13,naok))
cF3s0 F6x ( corF3x 1st F3a 2nd) cor 1st order !use !!!use !!!final
      val(26,naok) = val(20,naok)-(-0.042877*val(14,naok))
cF3s0 F6x cor(F3x 1st only)  1st order !use !!!use !!!final2
      val(27,naok) = val(20,naok)-(-0.042877*val(10,naok))
cccccccccccc
c F3s0 by F3x, F3a, f6x cor2  2nd order
cccccccccccc
cF3s0 F6x cor  2nd order (F6x old)
      val(28,naok) = val(20,naok)-(-0.030608*f6x+ 
     &(1.08E-04)*f6x**2)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(F3x 1st F3a 1st)  cor  2nd order 
      val(30,naok) = val(20,naok)-(-0.030608*val(13,naok)+ 
     &(1.08E-04)*val(13,naok)**2)
cF3s0 F6x cor(F3x 1st F3a 2nd)  2nd order !use
      val(32,naok) = val(20,naok)-(-0.030608*val(14,naok)+ 
     &(1.08E-04)*val(14,naok)**2)
cF3s0 F6x cor(F3x 1st only)  2nd order !use
      val(34,naok) = val(20,naok)-(-0.030608*val(10,naok)+ 
     &(1.08E-04)*val(10,naok)**2)
ccccccccccccccccccccccccccccccc

ccc
ccc 75Zn  64871c
ccc 218.48       0.87777E-02ns
      ID = 809
      
      naok = naok +1 

      val(1,naok) = id
cf6x cor
      val(2,naok) = T_F3S0-( -0.60166E-01*f6x+ 0.96603E-04 *f6x**2)
cf6x cor2
      val(3,naok) = T_F3S0-( -4.63280E-02*f6x+ (-1.17400E-06) *f6x**2)
cf3a cor      
      val(4,naok) = val(2,naok)



cccccc  75Zn
c new
c$$$$$$
c corr f6x by F3x and F3a
c$$$$$$
cf6x by F3x
      val(10,naok) = F6x-(2.8055*f3x)
cf6x by F3a
c1st
      val(11,naok) = F6x-(0.33395*f3a)
c2nd
      val(12,naok) = F6x-((0.34619)*f3a + (-0.046606)*f3a**2)
cf6x by F3x and F3a(first order)
      val(13,naok) = F6x-(2.8055*f3x)-(0.33395*f3a)
cf6x by F3x and F3a(second order) !!!use 
      val(14,naok) = F6x-(2.8055*f3x)-((0.34619)*f3a
     &+ (-0.046606)*f3a**2)
c$$$$$$
c corr f6x by TOF_F3S0
cccc  -0.047202	218.05
      val(50,naok) = F6x-((1/(-0.047202))*(T_F3S0-218.05))
cccc


c new
c$$$$$$
c TOF F3s0 by F3x, F3a, f6x cor2  seperately
c$$$$$$
cf3x cor 
      val(16,naok) = T_F3S0-(-0.10772*f3x)
cf3a cor      
      val(18,naok) = T_F3S0-(-0.025763*f3a)
c f3x and f3a cor     !!!use 
      val(20,naok) = T_F3S0-(-0.10772*f3x+(-0.025763)*f3a)
cF3s0 F6x cor 1st order old f6x
      val(21,naok) = T_F3S0-(-0.047202*f6x)
cF3s0 F6x cor old 2nd order old f6x
      val(22,naok) = T_F3S0-(-0.05721*f6x+
     $(0.000086519)*f6x**2)
ccccccc

ccccccccccccc
c F3s0 by F3x, F3a, f6x cor2   1st order
ccccccccccccc
cF3s0 F6x cor 1st order (F6x old)
      val(24,naok) = val(20,naok)-(-0.047202*f6x)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(cor F3x 1st F3a 1st)  cor  1st order 
      val(25,naok) = val(20,naok)-(-0.047202*val(13,naok))
cF3s0 F6x ( corF3x 1st F3a 2nd) cor 1st order !use !!!use !!!final
      val(26,naok) = val(20,naok)-(-0.047202*val(14,naok))
cF3s0 F6x cor(F3x 1st only)  1st order !use !!!use !!!final2
      val(27,naok) = val(20,naok)-(-0.047202*val(10,naok))
cccccccccccc
c F3s0 by F3x, F3a, f6x cor2  2nd order
cccccccccccc
cF3s0 F6x cor  2nd order (F6x old)
      val(28,naok) = val(20,naok)-(-0.05721*f6x+ 
     &(0.000086519)*f6x**2)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(F3x 1st F3a 1st)  cor  2nd order 
      val(30,naok) = val(20,naok)-(-0.05721*val(13,naok)+ 
     &(0.000086519)*val(13,naok)**2)
cF3s0 F6x cor(F3x 1st F3a 2nd)  2nd order !use
      val(32,naok) = val(20,naok)-(-0.05721*val(14,naok)+ 
     &(0.000086519)*val(14,naok)**2)
cF3s0 F6x cor(F3x 1st only)  2nd order !use
      val(34,naok) = val(20,naok)-(-0.05721*val(10,naok)+ 
     &(0.000086519)*val(10,naok)**2)
ccccccccccccccccccccccccccccccc

ccc
ccc 76Zn   667987c
ccc  223.62       0.37468E-03ns
      ID = 810
      
      naok = naok +1 

      val(1,naok) = id
cf6x cor
      val(2,naok) = T_F3S0-(-0.48461E-01*f6x+(-0.42001E-04)*f6x**2)
cf6x cor2
      val(3,naok) = T_F3S0-(-4.64382E-02*f6x+(1.01080E-05)*f6x**2)
cf3a cor      
      val(4,naok) = val(2,naok)



cccccc  76Zn
c new
c$$$$$$
c corr f6x by F3x and F3a
c$$$$$$
cf6x by F3x
      val(10,naok) = F6x-(2.9536*f3x)
cf6x by F3a
c1st
      val(11,naok) = F6x-(0.55934*f3a)
c2nd
      val(12,naok) = F6x-((0.57307)*f3a + (-0.0047252)*f3a**2)
cf6x by F3x and F3a(first order)
      val(13,naok) = F6x-(2.9536*f3x)-(0.55934*f3a)
cf6x by F3x and F3a(second order) !!!use 
      val(14,naok) = F6x-(2.9536*f3x)-((0.57307)*f3a
     &+ (-0.0047252)*f3a**2)

c$$$$$$
c corr f6x by TOF_F3S0
cccc -0.038792	223.64
      val(50,naok) = F6x-((1/(-0.038792))*(T_F3S0-223.64))
cccc

c new
c$$$$$$
c TOF F3s0 by F3x, F3a, f6x cor2  seperately
c$$$$$$
cf3x cor 
      val(16,naok) = T_F3S0-(-0.11401*f3x)
cf3a cor      
      val(18,naok) = T_F3S0-(-0.027121*f3a)
c f3x and f3a cor     !!!use 
      val(20,naok) = T_F3S0-(-0.11401*f3x+(-0.027121)*f3a)
cF3s0 F6x cor 1st order old f6x
      val(21,naok) = T_F3S0-(-0.038792*f6x)
cF3s0 F6x cor old 2nd order old f6x
      val(22,naok) = T_F3S0-(-0.046526*f6x+
     $(-0.000001553)*f6x**2)
ccccccc

ccccccccccccc
c F3s0 by F3x, F3a, f6x cor2   1st order
ccccccccccccc
cF3s0 F6x cor 1st order (F6x old)
      val(24,naok) = val(20,naok)-(-0.038792*f6x)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(cor F3x 1st F3a 1st)  cor  1st order 
      val(25,naok) = val(20,naok)-(-0.038792*val(13,naok))
cF3s0 F6x ( corF3x 1st F3a 2nd) cor 1st order !use !!!use !!!final
      val(26,naok) = val(20,naok)-(-0.038792*val(14,naok))
cF3s0 F6x cor(F3x 1st only)  1st order !use !!!use !!!final2
      val(27,naok) = val(20,naok)-(-0.038792*val(10,naok))

cccccccccccc
c F3s0 by F3x, F3a, f6x cor2  2nd order
cccccccccccc
cF3s0 F6x cor  2nd order (F6x old)
      val(28,naok) = val(20,naok)-(-0.046526*f6x+ 
     &(-0.000001553)*f6x**2)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(F3x 1st F3a 1st)  cor  2nd order 
      val(30,naok) = val(20,naok)-(-0.046526*val(13,naok)+ 
     &(-0.000001553)*val(13,naok)**2)
cF3s0 F6x cor(F3x 1st F3a 2nd)  2nd order !use
      val(32,naok) = val(20,naok)-(-0.046526*val(14,naok)+ 
     &(-0.000001553)*val(14,naok)**2)
cF3s0 F6x cor(F3x 1st only)  2nd order !use
      val(34,naok) = val(20,naok)-(-0.046526*val(10,naok)+ 
     &(-0.000001553)*val(10,naok)**2)
ccccccccccccccccccccccccccccccc

ccc
ccc 77Zn  24344c
ccc  229.66       0.29327E-02ns
      ID = 811
      
      naok = naok +1 

      val(1,naok) = id
cf6x cor
      val(2,naok) = T_F3S0-(-0.40901E-01*f6x+ 0.31911E-05*f6x**2)
cf6x cor2
      val(3,naok) = T_F3S0-(-3.09095E-02*f6x+ 1.08441E-04*f6x**2)
cf3a cor      
      val(4,naok) = val(2,naok)



cccccc  77Zn
c new
c$$$$$$
c corr f6x by F3x and F3a
c$$$$$$
cf6x by F3x
      val(10,naok) = F6x-(3.5189*f3x)
cf6x by F3a
c1st
      val(11,naok) = F6x-(0.94428*f3a)
c2nd
      val(12,naok) = F6x-((0.7083)*f3a + (0.049304)*f3a**2)
cf6x by F3x and F3a(first order)
      val(13,naok) = F6x-(3.5189*f3x)-(0.94428*f3a)
cf6x by F3x and F3a(second order) !!!use 
      val(14,naok) = F6x-(3.5189*f3x)-((0.7083)*f3a
     &+ (0.049304)*f3a**2)

c$$$$$$
c corr f6x by TOF_F3S0
cccc -0.042453	229.7
      val(50,naok) = F6x-((1/(-0.042453))*(T_F3S0-229.7))
cccc


c new
c$$$$$$
c TOF F3s0 by F3x, F3a, f6x cor2  seperately
c$$$$$$
cf3x cor 
      val(16,naok) = T_F3S0-(-0.12362*f3x)
cf3a cor      
      val(18,naok) = T_F3S0-(-0.026208*f3a)
c f3x and f3a cor     !!!use 
      val(20,naok) = T_F3S0-(-0.12362*f3x+(-0.026208)*f3a)
cF3s0 F6x cor 1st order old f6x
      val(21,naok) = T_F3S0-(-0.042453*f6x)
cF3s0 F6x cor old 2nd order old f6x
      val(22,naok) = T_F3S0-(-0.03268*f6x+
     $(0.000079168)*f6x**2)
ccccccc

ccccccccccccc
c F3s0 by F3x, F3a, f6x cor2   1st order
ccccccccccccc
cF3s0 F6x cor 1st order (F6x old)
      val(24,naok) = val(20,naok)-(-0.042453*f6x)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(cor F3x 1st F3a 1st)  cor  1st order 
      val(25,naok) = val(20,naok)-(-0.042453*val(13,naok))
cF3s0 F6x ( corF3x 1st F3a 2nd) cor 1st order !use !!!use !!!final
      val(26,naok) = val(20,naok)-(-0.042453*val(14,naok))
cF3s0 F6x cor(F3x 1st only)  1st order !use !!!use !!!final2
      val(27,naok) = val(20,naok)-(-0.042453*val(10,naok))

cccccccccccc
c F3s0 by F3x, F3a, f6x cor2  2nd order
cccccccccccc
cF3s0 F6x cor  2nd order (F6x old)
      val(28,naok) = val(20,naok)-(-0.03268*f6x+ 
     &(0.000079168)*f6x**2)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(F3x 1st F3a 1st)  cor  2nd order 
      val(30,naok) = val(20,naok)-(-0.03268*val(13,naok)+ 
     &(0.000079168)*val(13,naok)**2)
cF3s0 F6x cor(F3x 1st F3a 2nd)  2nd order !use
      val(32,naok) = val(20,naok)-(-0.03268*val(14,naok)+ 
     &(0.000079168)*val(14,naok)**2)
cF3s0 F6x cor(F3x 1st only)  2nd order !use
      val(34,naok) = val(20,naok)-(-0.03268*val(10,naok)+ 
     &(0.000079168)*val(10,naok)**2)
ccccccccccccccccccccccccccccccc

ccc
ccc 74Cu   34017c
ccc  226.02       0.15026E-01ns
      ID = 812
      
      naok = naok +1 

      val(1,naok) = id
cf6x cor
      val(2,naok) = T_F3S0-(-0.45263E-01*f6x+(-0.33470E-04)*f6x**2)
cf6x cor2
      val(3,naok) = T_F3S0-(-3.99570E-02*f6x+(-7.39880E-05)*f6x**2)

cf3a cor      
      val(4,naok) = val(2,naok)


cccccc  74Cu
c new
c$$$$$$
c corr f6x by F3x and F3a
c$$$$$$
cf6x by F3x
      val(10,naok) = F6x-(3.1196*f3x)
cf6x by F3a
c1st
      val(11,naok) = F6x-(0.51849*f3a)
c2nd
      val(12,naok) = F6x-((0.52355)*f3a + (-0.042995)*f3a**2)
cf6x by F3x and F3a(first order)
      val(13,naok) = F6x-(3.1196*f3x)-(0.51849*f3a)
cf6x by F3x and F3a(second order) !!!use 
      val(14,naok) = F6x-(3.1196*f3x)-((0.52355)*f3a
     &+ (-0.042995)*f3a**2)
c$$$$$$
c corr f6x by TOF_F3S0
cccc -0.04796	226.06
      val(50,naok) = F6x-((1/(-0.04796))*(T_F3S0-226.06))
cccc

c new
c$$$$$$
c TOF F3s0 by F3x, F3a, f6x cor2  seperately
c$$$$$$
cf3x cor 
      val(16,naok) = T_F3S0-(-0.13686*f3x)
cf3a cor      
      val(18,naok) = T_F3S0-(-0.033262*f3a)
c f3x and f3a cor     !!!use 
      val(20,naok) = T_F3S0-(-0.13686*f3x+(-0.033262)*f3a)
cF3s0 F6x cor 1st order old f6x
      val(21,naok) = T_F3S0-(-0.04796*f6x)
cF3s0 F6x cor old 2nd order old f6x
      val(22,naok) = T_F3S0-(-0.04653*f6x+
     $(-0.000013109)*f6x**2)

ccccccc

ccccccccccccc
c F3s0 by F3x, F3a, f6x cor2   1st order
ccccccccccccc
cF3s0 F6x cor 1st order (F6x old)
      val(24,naok) = val(20,naok)-(-0.04796*f6x)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(cor F3x 1st F3a 1st)  cor  1st order 
      val(25,naok) = val(20,naok)-(-0.04796*val(13,naok))
cF3s0 F6x ( corF3x 1st F3a 2nd) cor 1st order !use !!!use !!!final
      val(26,naok) = val(20,naok)-(-0.04796*val(14,naok))
cF3s0 F6x cor(F3x 1st only)  1st order !use !!!use !!!final2
      val(27,naok) = val(20,naok)-(-0.04796*val(10,naok))
cccccccccccc
c F3s0 by F3x, F3a, f6x cor2  2nd order
cccccccccccc
cF3s0 F6x cor  2nd order (F6x old)
      val(28,naok) = val(20,naok)-(-0.04653*f6x+ 
     &(-0.000013109)*f6x**2)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(F3x 1st F3a 1st)  cor  2nd order 
      val(30,naok) = val(20,naok)-(-0.04653*val(13,naok)+ 
     &(-0.000013109)*val(13,naok)**2)
cF3s0 F6x cor(F3x 1st F3a 2nd)  2nd order !use
      val(32,naok) = val(20,naok)-(-0.04653*val(14,naok)+ 
     &(-0.000013109)*val(14,naok)**2)
cF3s0 F6x cor(F3x 1st only)  2nd order !use
      val(34,naok) = val(20,naok)-(-0.04653*val(10,naok)+ 
     &(-0.000013109)*val(10,naok)**2)
ccccccccccccccccccccccccccccccc

ccc
ccc 75Cu   74668c
ccc  231.87       0.14807E-02ns
      ID = 813
      
      naok = naok +1 

      val(1,naok) = id
cf6x cor
      val(2,naok) = T_F3S0-( -0.47239E-01*f6x+0.13768E-04*f6x**2)
cf6x cor2
      val(3,naok) = T_F3S0-( -4.74728E-02*f6x+3.76400E-06*f6x**2)
cf3a cor      
      val(4,naok) = val(2,naok)



cccccc  75Cu
c new
c$$$$$$
c corr f6x by F3x and F3a
c$$$$$$
cf6x by F3x
      val(10,naok) = F6x-(2.9901*f3x)
cf6x by F3a
c1st
      val(11,naok) = F6x-(0.63994*f3a)
c2nd
      val(12,naok) = F6x-((0.65655)*f3a + (-0.0053722)*f3a**2)
cf6x by F3x and F3a(first order)
      val(13,naok) = F6x-(2.9901*f3x)-(0.63994*f3a)
cf6x by F3x and F3a(second order) !!!use 
      val(14,naok) = F6x-(2.9901*f3x)-((0.65655)*f3a
     &+ (-0.0053722)*f3a**2)

c$$$$$$
c corr f6x by TOF_F3S0
cccc  -0.047633	231.87
      val(50,naok) = F6x-((1/(-0.047633))*(T_F3S0-231.87))
cccc

c new
c$$$$$$
c TOF F3s0 by F3x, F3a, f6x cor2  seperately
c$$$$$$
cf3x cor 
      val(16,naok) = T_F3S0-(-0.1178*f3x)
cf3a cor      
      val(18,naok) = T_F3S0-(-0.031086*f3a)
c f3x and f3a cor     !!!use 
      val(20,naok) = T_F3S0-(-0.1178*f3x+(-0.031086)*f3a)
cF3s0 F6x cor 1st order old f6x
      val(21,naok) = T_F3S0-(-0.047633*f6x)
cF3s0 F6x cor old 2nd order old f6x
      val(22,naok) = T_F3S0-(-0.04673*f6x+
     $(0.000055751)*f6x**2)
ccccccc

ccccccccccccc
c F3s0 by F3x, F3a, f6x cor2   1st order
ccccccccccccc
cF3s0 F6x cor 1st order (F6x old)
      val(24,naok) = val(20,naok)-(-0.047633*f6x)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(cor F3x 1st F3a 1st)  cor  1st order 
      val(25,naok) = val(20,naok)-(-0.047633*val(13,naok))
cF3s0 F6x ( corF3x 1st F3a 2nd) cor 1st order !use !!!use !!!final
      val(26,naok) = val(20,naok)-(-0.047633*val(14,naok))
cF3s0 F6x cor(F3x 1st only)  1st order !use !!!use !!!final2
      val(27,naok) = val(20,naok)-(-0.047633*val(10,naok))
cccccccccccc
c F3s0 by F3x, F3a, f6x cor2  2nd order
cccccccccccc
cF3s0 F6x cor  2nd order (F6x old)
      val(28,naok) = val(20,naok)-(-0.04673*f6x+ 
     &(0.000055751)*f6x**2)
cc F3s0 by F3x, F3a, f6x cor2  !! F6x cor by F3x and F3a
cF3s0 F6x(F3x 1st F3a 1st)  cor  2nd order 
      val(30,naok) = val(20,naok)-(-0.04673*val(13,naok)+ 
     &(0.000055751)*val(13,naok)**2)
cF3s0 F6x cor(F3x 1st F3a 2nd)  2nd order !use
      val(32,naok) = val(20,naok)-(-0.04673*val(14,naok)+ 
     &(0.000055751)*val(14,naok)**2)
cF3s0 F6x cor(F3x 1st only)  2nd order !use
      val(34,naok) = val(20,naok)-(-0.04673*val(10,naok)+ 
     &(0.000055751)*val(10,naok)**2)
ccccccccccccccccccccccccccccccc

cccccccccccccccccccccccc

ccc
ccc 76Cu  1690c not use 

ccc mean  237.74       0.64026E-01 ns
      ID = 814
      
      naok = naok +1 

      val(1,naok) = id
cf6x cor
      val(2,naok) = T_F3S0-( -0.54480E-01*f6x+ ( -0.47468E-04)*f6x**2)
cf6x cor2
      val(3,naok) = T_F3S0-( 3.87730E-02*f6x+ ( 6.17762E-04)*f6x**2)
cf3a cor      
      val(4,naok) = val(2,naok)*1.
ccccccccccccccccccccccccccccccc

cccccccccccccccccccccccc

ccc
ccc Ni73 2440c
ccc mean  234.69       0.57535E-01 ns
      ID = 815
      
      naok = naok +1 

      val(1,naok) = id
cf6x cor
      val(2,naok) = T_F3S0-( -0.47345E-01*f6x+ (-0.38473E-04)*f6x**2)
cf6x cor2
      val(3,naok) = T_F3S0-( 2.54E-03*f6x+ (-6.03E-04)*f6x**2)
cf3a cor      
      val(4,naok) = val(2,naok)*1.
ccccccccccccccccccccccccccccccc

      gammaz = 1./sqrt(1.-beta**2)
      Z_t = beta*sqrt(F3dE) ! Z tmp
c      Z_t2 = beta*sqrt(F3dE/(log(4866.)+
c     &log(beta**2/(1-beta**2))-beta**2))
      Z_t2 = beta*sqrt(F3dE/(log(5209.5)+
     &log(beta**2/(1-beta**2))-beta**2))
c      Z = Z_t*3.157 + 2.6493  ! Z
c      Z2 = Z_t2*3.157 + 2.6493  ! Z
c      aoz_t = Brho/(beta * gammaz) ! A/Q tmp
c      aoz = -0.82764 * aoz_t + 5.0073 ! A/Q
      Z= Z_t*0.9202 + 0.0349-0.02  ! Z y = 0.9275x - 0.1806 (5 center opint)
      Z2 = Z_t2*2.6762 -2.3605+0.14 ! Z y = 2.6947x - 2.5754 (5 center opint)
      aoz_t = Brho/(beta * gammaz) ! A/Q tmp
      !aoz_t_2 = Brho_br/(beta * gammaz) ! A/Q tmp
      !aoz = -0.82764 * aoz_t + 5.0073 ! A/Q 
      ! y = 0.3222x - 0.0029 (5 center opint)
      aoz = 0.3208 * aoz_t + 0.0086 ! A/Q


      id = 551
      naok = naok +1
      val(1,naok)= id
      val(2,naok)= beta
      val(3,naok)= Z_t
      val(4,naok)=Z_t2
      val(5,naok)= aoz_t
      val(6,naok)= aoz
      val(7,naok)=Z
      val(8,naok)=Z2

c      print*,'beta','z_t','z_t2','aoz_t','aoz'
c      print*,beta,z_t,z_t2,aoz_t,aoz
c      print*,'evtnum,trignum',evtnum,trignum ,'1'      
c event number

      evtnum = evtnum + 1
c      if(i.eq.evtnum) then
c         evtnum = evtnum + 1
c      endif
      id = 552
      naok = naok + 1
      val(1,naok) = id
      val(2,naok) = evtnum
      val(3,naok) = trignum
c      print*,'evtnum,trignum',evtnum,trignum, '2'





cccccccccccccc
c TOF correction
c 
c
cccccccccccccc
      id = 553
      naok = naok + 1
      val(1,naok) = id
      val(3,naok) =beta_br*T_F3s0
      val(4,naok) =beta_br*T_F3s0_1 !diff/2 in
      val(6,naok) =beta_br*T_F3s0-0.0*F3x-0.0*F3a-0.0*DPOP2
      val(8,naok) =beta_br*T_F3s0_1-0.0*F3x-0.0*F3a-0.0*DPOP2
      val(10,naok) =val(6,naok)/beta_br -217.3 !TOF 
      val(12,naok) =val(8,naok)/beta_br -217.3 !TOF



      T_F3s0_c=val(10,naok)
      T_F3s0_c1=val(12,naok)

cccccccccccccc
c PPAC Tsum X & Y 
c gate
c
cccccccccccccc
      id = 561

      naok = naok + 1
      val(1,naok) = id
      val(3,naok) = f31atxsum
      val(5,naok) = f31atysum
      val(7,naok) = f31btxsum
      val(9,naok) = f31btysum
      val(11,naok) = f32atxsum
      val(13,naok) = f32atysum
      val(15,naok) = f32btxsum
      val(17,naok) = f32btysum
      val(19,naok) = f6txsum
      val(21,naok) = f6tysum
      
      RETURN
      END

c ======================================
