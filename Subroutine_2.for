      subroutine usdfld(field,statev,pnewdt,direct,t,celent,time,dtime,
     1 cmname,orname,nfield,nstatv,noel,npt,layer,kspt,kstep,kinc,
     2 ndi,nshr,coord,jmac,jmtyp,matlayo,laccflg)
C
      include 'aba_param.inc'
C
      character*80 cmname,orname
      character*8  flgray(15)
      dimension field(nfield),statev(nstatv),direct(3,3),t(3,3),time(2),
     & coord(*),jmac(*),jmtyp(*)
      dimension array(15),jarray(15)
C     Call and read current stresses 
      call getvrm('SP',array,jarray,flgray,jrcd,
     &jmac, jmtyp, matlayo, laccflg)

c      if (kstep.eq.1 .and. kinc.eq.1) then
c       read y coordinates at first increment
c       y = coord(2)
C	Define depth h
c	h=30.0-y

      SP1=(ARRAY(1))
      SP2=(ARRAY(2))
      SM=SP2
C
      IF(SM.GE.0) THEN
      G0=0
      EO=0
      ELSE
      SU=2.8+0.37*SM
      EO=2000*SU
      ENDIF

C
C     Call and read State Dependent Variable SDV and name it EPSMAX
      call getvrm('SDV',array,jarray,flgray,jrcd,
     &jmac, jmtyp, matlayo, laccflg)     
      SUMAX=ARRAY(1)
C
      IF(TIME(2).LE.1.0) THEN
      FIELD(1)=EO
      FIELD(2)=SU
      ELSE
      FIELD(1)=STATEV(1)
      FIELD(2)=SU
      ENDIF      
C     
      STATEV(1)=FIELD(1)
      STATEV(2)=FIELD(2)
      STATEV(3)=SP1
      STATEV(4)=SP2

C     If error, write comment to .DAT file:
      IF(JRCD.NE.0)THEN
      WRITE(6,*) 'REQUEST ERROR IN USDFLD FOR ELEMENT NUMBER ',
     & NOEL,'INTEGRATION POINT NUMBER ',NPT
      ENDIF
C
      return
      end