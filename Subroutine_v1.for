      subroutine usdfld(field,statev,pnewdt,direct,t,celent,time,dtime,
     1 cmname,orname,nfield,nstatv,noel,npt,layer,kspt,kstep,kinc,
     2 ndi,nshr,coord,jmac,jmtyp,matlayo,laccflg)
c
      include 'aba_param.inc'
c
      character*80 cmname,orname
      character*8  flgray(15)
      dimension field(nfield),statev(nstatv),direct(3,3),t(3,3),time(2),
     & coord(*),jmac(*),jmtyp(*)
      dimension array(15),jarray(15)
C     Call and read current stresses 
      call getvrm('SP',array,jarray,flgray,jrcd,
     &jmac, jmtyp, matlayo, laccflg)
	S22=ARRAY(2)
c
      if (kstep.eq.1 .and. kinc.eq.1) then
c       read y coordinates at first increment
        y = coord(2)
c       define depth. 
        h= 30.0 - y
c	Define vertical effective stress
	SV=S22 - 10 * h
c	Update undrained shear strength value
        Su=2.8+0.37*ABS(SV)
        E = 2000*Su
c       a=2000
       field(1)=E
       field(2)=Su
      else
c       assign initial values computed in the first inc.
        field(1) = statev(1)
        field(2) = statev(2)
      end if 
c  
      statev(1)=field(1)
      statev(2)=field(2)
c     If error, write comment to .DAT file:
      if(JRCD.NE.0)then
      write(6,*) 'REQUEST ERROR IN USDFLD FOR ELEMENT NUMBER ',
     & NOEL,'INTEGRATION POINT NUMBER ',NPT
      endif
c
      return
      end