c     subroutine : enctraj.f
c     description(15)
c     data selection
c     
c
      subroutine enctraj(val,nx,ny,naok,analyzer)

c     traj(iprm,idetec)
c         traj: tracking information measured by using the detector
c             iprm: number of (X, Y, A, B, and T)
c           idetec: Focal plane
c           About idetec
c               Plastic scinti.
c                   PL3=1,PL7=2,PL9=3,PLX=4,PLS20=5,PLS21=6,PLS22=7,PL23=8
c               F3  =9-11
C                   F3TR(Tracker)=9,F3DC(MWDC)=10,F3PP(PPAC)=11
C               F4  =12 (PPAC)
c               F5  =13 (PPAC)
c               F6  =14 (PPAC)
C               FH7 =15 (MWDC)
c               FH9 =16 (MWDC)
c               S0  =17 (MWDC)
c               S2  =18 (CRDC)
c               S0PP=19 (PPAC)
c     About 
c      ID=1-8  : Plastic
c      W#:  1                        7 
c           ID(nprm)                 T
c      ID=9-10 : Tracking on focal plane
c      W#:  1        2  3  4  5  6
c           ID(nprm) X  Y  A  B  Z

      implicit none
      include '../core/analyslogic.fh'
      include '../bld/fpimgs.fh'
c      include '../fpd/fpimgs.fh'
      include './trajectory.fh'
      include '../ppac/valcommon.fh'      
      include '../ppac/valppac.fh'
      include '../plastic/spla-common.fh'
      integer analyzer
      integer nx,ny,naok
      real val(nx,ny)
      real zfp,zgt
      integer i,j
      parameter(zfp=0.,zgt=0.)

      if (initencflag(analyzer)) then
         write(6,10) 
     $        ' ANAPAW-M : [enctraj]            analys     =', analyzer
 10      format(1x,a,1x,i4)
         initencflag(analyzer) = .false. 
      endif
CCCCCCCCCCCCCCCCC
C Initialization
CCCCCCCCCCCCCCCCC
      do i=1,nprm
         do j=1,ndetec
            trj(i,j)=-1000000000
         enddo
      enddo
CCCCCCCCCCCCCCCCC
C Plastic
CCCCCCCCCCCCCCCCC
c      trj(T,PL3)=spla_gaint(PL3,MIDDLE)
      trj(T,PL7)=spla_gaint(PL7,MIDDLE)
      trj(T,PL9)=spla_gaint(PL9,MIDDLE)
      trj(T,PLX)=spla_gaint(PLX,MIDDLE)
      trj(T,PLS20)=spla_gaint(PLS20,MIDDLE)
      trj(T,PLS21)=spla_gaint(PLS21,MIDDLE)
      trj(T,PLS22)=spla_gaint(PLS22,MIDDLE)
      trj(T,PLS23)=spla_gaint(PLS23,MIDDLE)
    
c      naok=naok+1
c      val(1,naok)=1
c      val(7,naok)=trj(T,PL3) 
      naok=naok+1
      val(1,naok)=2     
      val(7,naok)=trj(T,PL7)
      naok=naok+1
      val(1,naok)=3     
      val(7,naok)=trj(T,PL9)
      naok=naok+1
      val(1,naok)=4     
      val(7,naok)=trj(T,PLX)
      naok=naok+1
      val(1,naok)=5     
      val(7,naok)=trj(T,PLS20)
      naok=naok+1
      val(1,naok)=6
      val(7,naok)=trj(T,PLS21)
      naok=naok+1
      val(1,naok)=7     
      val(7,naok)=trj(T,PLS22)
      naok=naok+1
      val(1,naok)=8     
      val(7,naok)=trj(T,PLS23)

CCCCCCCCCCCCCCCCCCCC
c Position Detector
CCCCCCCCCCCCCCCCCCCC
C F3 
Cc TRACKER
c      traj(X,F3TR)=
c      naok=naok+1
c      val(1,naok)=9
C      val(2,naok)=traj(X,F3TR)     
c     
c     
C MWDC
C
c original     
c      trj(X,F3DC)=x3a
c      trj(Y,F3DC)=y3a
c      trj(A,F3DC)=a3a
c      trj(B,F3DC)=b3a
c      trj(Z,F3DC)=0.
c illegal (F3 offset)
      trj(X,F3DC)=x3a
      trj(Y,F3DC)=y3a
      trj(A,F3DC)=a3a
      trj(B,F3DC)=b3a
      trj(Z,F3DC)=0.

      
c      write(*,*) "---"
c      write(*,*) "traj",x3a, y3a, a3a, b3a


c sharaq06 broken MWDC
c      trj(X,F3DC)=(ppacx(7)+ppacx(8))/2.
c      trj(Y,F3DC)=
c      trj(A,F3DC)=
c      trj(B,F3DC)=
c      trj(Z,F3DC)=0.




c     trj(X,F3DC) = xpp31 
c     trj(Y,F3DC) = ypp31
c     trj(A,F3DC) = app*1000.
c     trj(B,F3DC) = bpp*1000.
c     trj(Z,F3DC) = 0.

      naok=naok+1
      val(1,naok)=10
      val(2,naok)=trj(X,F3DC)
      val(3,naok)=trj(Y,F3DC)
      val(4,naok)=trj(A,F3DC)
      val(5,naok)=trj(B,F3DC)
      val(6,naok)=trj(Z,F3DC)
C PPAC  

      trj(X,F3PP)=ppac_fx(3)
      trj(Y,F3PP)=ppac_fy(3)
      trj(A,F3PP)=ppac_fa(3)*1000
      trj(B,F3PP)=ppac_fb(3)*1000
      trj(Z,F3PP)=0.


      naok=naok+1
      val(1,naok)=11
      val(2,naok)=trj(X,F3PP)
      val(3,naok)=trj(Y,F3PP)
      val(4,naok)=trj(A,F3PP)
      val(5,naok)=trj(B,F3PP)
      val(6,naok)=trj(Z,F3PP)
CF4    
      trj(X,F4)=ppac_fx(4)
      trj(Y,F4)=ppac_fy(4)
      trj(Z,F4)=0.
      
c      write (*,*) ppac_fx(4), ppac_fy(4)

      naok=naok+1                   
      val(1,naok)=12          
      val(2,naok)=trj(X,F4)
      val(3,naok)=trj(Y,F4)
cF5     
      trj(X,F5)=ppac_fx(5)
      trj(Y,F5)=ppac_fy(5)
      trj(A,F5)=ppac_fa(5)*1000
      trj(B,F5)=ppac_fb(5)*1000
      trj(Z,F5)=0.

      naok=naok+1                   
      val(1,naok)=13          
      val(2,naok)=trj(X,F5)
      val(3,naok)=trj(Y,F5)      
      val(4,naok)=trj(A,F5)
      val(5,naok)=trj(B,F5)
      val(6,naok)=trj(Z,F5)
cF6     
      trj(X,F6)=ppac_fx(6)
      trj(Y,F6)=ppac_fy(6)
      trj(Z,F6)=0.
c      write(*,*)  trj(X,F6), trj(Y,F6)
      naok=naok+1             
      val(1,naok)=14          
      val(2,naok)=trj(X,F6)      
      val(3,naok)=trj(Y,F6)
      val(6,naok)=trj(Z,F6)
CFH7
      if((abs(x71a).lt.200.).and.(abs(x72a).lt.200)) then
         trj(A,FH7)=(x72a-x71a)/(z72xa-z71xa)*1000
         trj(X,FH7) =(x72a*(zfp-z71xa)+x71a*(z72xa-zfp))/(z72xa-z71xa)
      else
         trj(A,FH7)=-1000.
         trj(X,FH7)=-1000.
      endif

      if((abs(y71a).lt.200.).and.(abs(y72a).lt.200)) then
         trj(B,FH7)=(y72a-y71a)/(z72ya-z71ya)*1000
c         write(*,*) 'FH7',y71a,y72a
         trj(Y,FH7)=(y72a*(zfp-z71ya)+y71a*(z72ya-zfp))/(z72ya-z71ya)     
      else
         trj(B,FH7)=-1000.
         trj(Y,FH7)=-1000.
      endif
      trj(Z,FH7)=zfp
c        write(*,*) 'FH7',y71a,y72a, trj(Y,FH7)
c        write(*,*) 'FH7',x71a,x72a, trj(X,FH7)
      naok=naok+1
      val(1,naok)=15          
      val(2,naok)=trj(X,FH7)
      val(3,naok)=trj(Y,FH7)
      val(4,naok)=trj(A,FH7)
      val(5,naok)=trj(B,FH7)
      val(6,naok)=trj(Z,FH7)

cFH9
      trj(X,FH9)=x91a
      trj(Y,FH9)=y91a
      trj(Z,FH9)=zfp
      
      naok=naok+1
      val(1,naok)=16          
      val(2,naok)=trj(X,FH9)
      val(3,naok)=trj(Y,FH9)

CS0
c input 
c   xX1a, yX1a, zX1a
c   xS0PP, yS0PP, zS0PP = zgt = 0
c    (xX2a, yX2a) is replaced by (xS0PP,yS0PP)
c
c  |   ------------- X-Cathode
c  |   | 4.3mm
c  |   ------------- Anode
c  |   | 4.3mm
c  |   ------------- Y-Cathode
c  \/
c  beam
c
      xX2a = xS0PP
      yX2a = yS0PP
      zX2xa = zgt
      zX2ya = zgt+8.6
      if((abs(xX1a).lt.200.).and.(abs(xX2a).lt.200)) then
         trj(A,S0)=(xX2a-xX1a)/(zX2xa-zX1xa)*1000
         trj(X,S0) =(xX2a*(zgt-zX1xa)+xX1a*(zX2xa-zgt))/(zX2xa-zX1xa)
      elseif((abs(xX2a).lt.200)) then
         trj(X,S0) = xX2a
         trj(A,S0) = -1000.
      else
         trj(A,S0)=-1000.
         trj(X,S0)=-1000.
      endif

      if((abs(yX1a).lt.200.).and.(abs(yX2a).lt.200)) then
         trj(B,S0)=(yX2a-yX1a)/(zX2ya-zX1ya)*1000
         trj(Y,S0)=(yX2a*(zgt-zX1ya)+yX1a*(zX2ya-zgt))/(zX2ya-zX1ya)     
      else if((abs(yX2a).lt.200)) then
         trj(Y,S0) = yX2a
         trj(B,S0) = -1000.
      else
         trj(B,S0)=-1000.
         trj(Y,S0)=-1000.
      endif

c      write(*,*) trj(X,S0),trj(Y,S0),trj(A,S0),trj(B,S0)      

c      trj(X,S0)=xS0        
c      trj(Y,S0)=yS0
c      trj(A,S0)=aS0        
c      trj(B,S0)=bS0
c      trj(Z,S0)=zgt


      naok=naok+1             
      val(1,naok)=17          
      val(2,naok)=trj(X,S0)
      val(3,naok)=trj(Y,S0)
      val(4,naok)=trj(A,S0)
      val(5,naok)=trj(B,S0)
      val(6,naok)=trj(Z,S0)

cS2
c      write(*,*) xs21a,xs22a,ys21a,ys22a,zs21xa,zs21ya,zs22xa,zs22ya

c      if((abs(xs21a).lt.200.).and.(abs(xs22a).lt.200)) then
c         
c         trj(A,S2)=(xS22a-xS21a)/(zS22xa-zs21xa)*1000
c         
c         trj(X,S2) 
c     &        =(xS22a*(zgt-zS21xa)+xS21a*(zS22xa-zgt))/(zS22xa-zS21xa)
c    
c      else
c         trj(A,S2)=-1000.
c         trj(X,S2)=-1000.
c      endif
c      
c      if((abs(yS21a).lt.200.).and.(abs(yS22a).lt.200)) then
c         trj(B,S2)=(yS22a-yS21a)/(zS22ya-zS21ya)*1000
c         trj(Y,S2)
c     &        =(yS22a*(zgt-zS21ya)+yS22a*(zS22ya-zgt))/(zS22ya-zS21ya)     
c      else
c         trj(B,S2)=-1000.
c         trj(Y,S2)=-1000.
c      endif


c require calling encs2
      trj(X,S2)=xS2        
      trj(Y,S2)=yS2
      trj(A,S2)=aS2        
      trj(B,S2)=bS2
      trj(Z,S2)=0.

      naok=naok+1             
      val(1,naok)=18          
      val(2,naok)=trj(X,S2)
      val(3,naok)=trj(Y,S2)
      val(4,naok)=trj(A,S2)
      val(5,naok)=trj(B,S2)
      val(6,naok)=trj(Z,S2)

cfor SHARAQ03
cS0PPAC
      trj(X,S0PP)=xS0PP        
      trj(Y,S0PP)=yS0PP
c      trj(A,S0PP)=trj(A,S0)        
c      trj(B,S0PP)=trj(B,S0)
c      trj(Z,S0PP)=0.

      naok=naok+1             
      val(1,naok)=19          
      val(2,naok)=trj(X,S0PP)
      val(3,naok)=trj(Y,S0PP)
c      val(4,naok)=trj(A,S0PP)
c      val(5,naok)=trj(B,S0PP)
c      val(6,naok)=trj(Z,S0PP)

      return
      end
