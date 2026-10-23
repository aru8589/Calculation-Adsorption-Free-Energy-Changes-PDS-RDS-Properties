
IMPLICIT REAL*8(A-H,O-Z)

Dimension X(50,50),B(50,50),Y(50,50),Z(50,50),A(50,50),P(50,50),Q(50,50),S(50,50),W(50,50),U(50,50),V(50,50),M(50,50)
!Dimension X1(50,50),B1(50,50),Y3(50,50),Z1(50,50),A1(50,50),P1(50,50),Q1(50,50),S1(50,50),W1(50,50),U1(50,50),V1(50,50),M1(50,50)
Dimension D1(50,50),A1(50,50),R1(50,50),UR1(50,50),B1(50,50),M1(50,50)
Dimension Y3(50,50),Z1(50,50),Y4(50,50),Y5(50,50),Y6(50,50),Y7(50,50),Y8(50,50),Y9(50,50),Y10(50,50)
Character*45 LIST(50,50),LIST1(50,50),SYSTEM1(50,50),SYSTEM(50,50)

OPEN(UNIT = 11, FILE = 'STEP.DAT', STATUS= 'OLD')
OPEN(UNIT = 13, FILE = 'INPUT.DAT', STATUS= 'OLD')
OPEN(UNIT = 16, FILE = 'PROPERTIES.DAT', STATUS= 'OLD')
OPEN(UNIT = 17, FILE = '1-RESULT.OUT', STATUS= 'NEW')
OPEN(UNIT = 21, FILE = '2-RESULT.OUT', STATUS= 'NEW')

write(17,*)
write(17,*)"HOW MANY STEP?"
read(11,*)I1,I7
write(17,*)I1
write(17,*)

I8 = I1
if(I1.eq.0)then 
I5=1
I6=1

else
I5=0
I6=I1
endif

I4=0

do 14 J1 = I5,I5
do 14 L1= I5,I6
read(13,*)Y3(L1,J1),Y3(L1,I5+1),Y3(L1,I5+2),Y3(L1,I5+3),Y3(L1,I5+4),Y3(L1,I5+5),Y3(L1,I5+6),Y3(L1,I5+7),LIST1(L1,I5+8)
A(L1,J1)=Y3(L1,I5)
P(L1,J1)=Y3(L1,I5+1)
Q(L1,J1)=Y3(L1,I5+2)
W(L1,J1)=Y3(L1,I5+3)
S(L1,J1)=Y3(L1,I5+4)
U(L1,J1)=Y3(L1,I5+5)
V(L1,J1)=Y3(L1,I5+6)
M(L1,J1)=Y3(L1,I5+7)
LIST(L1,J1)=LIST1(L1,I5+8)

!write(*,303)A(L1,J1),P(L1,J1),Q(L1,J1),W(L1,J1),S(L1,J1),U(L1,J1),V(L1,J1),M(L1,J1),LIST(L1,J1)
!303   FORMAT(F15.6,F15.6,2X,F15.6,2X,F15.6,3X,F15.6,F15.6,3X,F15.6,12X,I3,13X,45A24)
14 enddo

do 29 K3 = 1,1
do 29 K4 = 1,I7
read(16,*)Y4(K4,K3),Y4(K4,K3+1),Y4(K4,K3+2),Y4(K4,K3+3),Y4(K4,K3+4),Y4(K4,K3+5),SYSTEM1(K4,K3+6)
D1(K4,K3)=Y4(K4,K3)
A1(K4,K3)=Y4(K4,K3+1)
R1(K4,K3)=Y4(K4,K3+2)
UR1(K4,K3)=Y4(K4,K3+3)
B1(K4,K3)=Y4(K4,K3+4)
M1(K4,K3)=Y4(K4,K3+5)
SYSTEM(K4,K3)=SYSTEM1(K4,K3+6)

!write(*,304)D1(K4,K3),A1(K4,K3),R1(K4,K3),UR1(K4,K3),B1(K4,K3),M1(K4,K3),SYSTEM(K4,K3)
!304   FORMAT(F15.6,2X,F15.6,2X,F15.6,3X,F15.6,F15.6,8X,I3,8X,45A24)

write(21,*)
write(21,*)SYSTEM(K4,K3),R1(K4,K3)
write(21,*)
write(21,*)'DIAMETER     =',D1(K4,K3),"   A                           ",SYSTEM(K4,K3)
write(21,*)'AREA         =',A1(K4,K3),"  A^2                          ",SYSTEM(K4,K3)
write(21,*)'E(RELAXED)   =',R1(K4,K3),"  eV                           ",SYSTEM(K4,K3)
write(21,*)'E(UNRELAXED) =',UR1(K4,K3),"  eV                           ",SYSTEM(K4,K3)
write(21,*)'E(BULK)      =',B1(K4,K3),"  eV                           ",SYSTEM(K4,K3)
write(21,*)'NO. OF ATOM  =',M1(K4,K3),"                                             ",SYSTEM(K4,K3)
write(21,*)

Y7(K4,K3)=  UR1(K4,K3)-R1(K4,K3)
Y8(K4,K3)=  M1(K4,K3)*B1(K4,K3)
Y9(K4,K3)=  R1(K4,K3)-Y8(K4,K3)
Y10(K4,K3)= Y9(K4,K3)/A1(K4,K3)

write(21,*)'COMPRESIVE TENSILE STRAIN   =',Y7(K4,K3),"                ",SYSTEM(K4,K3)
write(21,*)'SURFACE (FACET) ENERGY      =',Y10(K4,K3),"   eV A^-2      ",SYSTEM(K4,K3)
write(21,*)
29 enddo

if(I7.le.1)GOTO 105

do 28 K5=1,1
do 28 K6=1,I7
Y5(K6,K5)= D1(K5,K5)- D1(K5+1,K5)
Y6(K6,K5)= Y5(K6,K5)/D1(K6,K5)
write(21,*)'              STRAIN        =',Y6(K6,K5),SYSTEM(K6,K5)

28 enddo

105 if(I1.ne.0)then
    I4=0
    else
    I5=1
    endif

do 12 J1= I5,I5
do 12 L1= I5,I6
B(L1,J1)= S(L1,J1)- U(L1,J1)- V(L1,J1)

X(L1,J1) = S(L1,J1) + Q(L1,J1) + M(L1,J1)*(-3.4149433) + W(L1,J1) + A(L1,J1)

!write(*,*)
12 enddo

write(17,*)"             ","  ---------------------------------   ::  SUMMARY  ::   ---------------------------------"
write(17,*)
write(17,*)" RESULT :"
write(17,*)

write(17,*)"    BINDING ENERGY                     FREE ENERGY                         ELEMENTARY REACTION"
write(17,*)"    ADSORBED SPEC.                   ELEMENTARY STEP                             SPECIES"

do 70 J1 = I5,I5
do 70 L1 = I5,I6
do 90 J3 = 0,0

if(I1.eq.0.and.B(L1,J1).GT.0.0.or.B(L1,J1).LT.-8.0)then
B(L1,J1)= 0.0

elseif(L1.GT.I5.and.L1.LT.I6.and.B(L1,J1).LT.-8.0.or.B(L1,J1).GT.0.0)then
B(L1,J1)= 0.0

else
B(L1,J1)= B(L1,J1)
endif

write(17,*)B(L1,J1),"       ",X(L1,J1),"   ","  ( ev )","        ",LIST(L1,J1)
write(17,*)

     if(L1+1.GT.I8)goto 1     
     Y(L1+1,J1) = X(L1+1,J1)-X(L1,J1)
1    Z(L1,J1)= X(L1,J1)-X(J3,J1)

!write(*,*)Z(L1,J1)

90 enddo
70 enddo

Z1(3,1)= Z(3,1) 

write(17,*)"NOTE ::"
write(17,*)"BINDING ENERGY = TOTAL ENERGY- SPE/TE(A)- SPE/TE(B)"
write(17,*)"FREE ENERGY = TOTAL ENERGY+ ZPE+ NO. OF PROTON*(-3.4149433)+ E(ORS)+ E(SRFC/CLSTER)" 
write(17,*)
write(17,*)"IF BINDING ENERGY LESS THAN -8.0 eV OR MORE THAN 0.0 eV,"
write(17,*)"IT WILL BE CONSIDERED HERE AS ZERO OR CHEMISORBED SPECIES."

if(I8.eq.0)goto 110       
write(17,*)
write(17,*)"      DIFFERENCE                          FREE ENERGY                     COORDINATE                        STEP"


write(17,*)"                                  ",X(I5,I5),"       ",Z(I5,I5),"          ",LIST(I5,I5)

Z(3,1)= Z1(3,1)

do 100 J1= I5,I5
do 100 L1 = 1,I8
write(17,*)Y(L1,J1),"        ",X(L1,J1),"        ",Z(L1,J1),"         ",LIST(L1,J1)
100 enddo

110 stop
    end
