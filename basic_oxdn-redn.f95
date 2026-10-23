
Dimension X(100),B(100),Y(100),Z(100),R(50,50),A(50),P(50),Q(50),S(50),W(50),U(50),V(50),L3(50),M(50),Y3(50,50)
Character*45 SPECIES,LIST(50)

write(*,*)
150     write(*,*)"HOW MANY STEP? PRESS '0' FOR ALTERNATIVE ONLY"
        read(*,*)I1
        write(*,*)

if(I1.eq.0)then 
I5=1
I6=1

else
I5=0
I6=I1
endif

I4=0
do 12 J1 = I5,I6

write(*,*)"ENTER NAME OF SPECIES"
read(*,*)SPECIES

LIST(J1) = SPECIES

write(*,*)"REACTION STEP : ",LIST(J1)
write(*,*)

write(*,*)"TYPE NO. OF VIBRATIONAL MODE"
read(*,*) J

write(*,*)"TYPE REQUIRED",J,"NO. OF VIBRATIONAL FREQUENCY VALUES IN mev"

Y2=0.0
L3(J1)=J

do 10 K= 1,J
read(*,*)Y1

Y3(J1,K)=Y1
R(J1,K)=Y1/1000
Y2 = Y2 + Y1/1000
10 enddo

write(*,*)
write(*,*)"FREQUENCY( ev )             FREQ. (mev)              ADSORBED SPECIES"

do 40 L=J1,J1
do 40 K=1,J

write(*,*)R(L,K),"         ",Y3(L,K),"        ",LIST(J1)
40 enddo

write(*,*)
write(*,*)"SUM OF VIBRATIONAL FREQUENCY =",Y2,"ev"
write(*,*)

P(J1)=Y2

Z1 = Y2/2
write(*,*)"ZPE =",Z1,"ev"
write(*,*)

Q(J1)=Z1

write(*,*)"TYPE TOTAL ENERGY OF TARGET SPECIES IN ev"
read(*,*)E
S(J1)= E

write(*,*)
write(*,*)"TYPE ENERGY OF OTHER SPECIES, IF ANY"
read(*,*)T

W(J1)= T

write(*,*)
write(*,*)"NO. OF 'OH' ION ASSOCIATED FOR THIS STEP"
read(*,*)I

M(J1)= I

write(*,*)
if(I1.ne.0)then
I4=0
else
I5=1
endif

if(J1.eq.0.or.J1.eq.I1)then

write(*,*)"TOTAL (SURFACE/CLUSTER) ENERGY IN ev"
read(*,*)C

A(J1)= C
U1 = 0.0
V1 = 0.0
U(J1)= U1
V(J1)= V1
B(J1) = 0.0

else
A(J1)= 0.0

write(*,*)
write(*,*)"CALCULATE BINDING ENERGY "

write(*,*)"TYPE SPE/TE(Fe or A), SPE/TE(INT or B) in ev" 
read(*,*)U1,V1

U(J1)= U1
V(J1)= V1

B(J1)= S(J1)- U(J1)- V(J1)

endif

X(J1) = S(J1) + Q(J1) + M(J1)*(-10.8671) + W(J1) + A(J1)

write(*,*)
12 enddo

write(*,*)"             ","  ---------------------------------   ::  SUMMARY  ::   ---------------------------------"
write(*,*)"INPUT DETAILS :"
write(*,*)

write(*,*)"     No. OF FREQ     SPECIES"

do 35 J1=I5,I6
write(*,*)L3(J1),"          ",LIST(J1)
35 enddo

write(*,*)
write(*,*)"FREQUENCY( ev )           FREQ. (mev)          ADSORBED SPECIES"

do 20 J1 = I5, I6
do 20 K = 1,L3(J1)

write(*,*)R(J1,K),"       ",Y3(J1,K),"   ",LIST(J1)
write(*,*)
20 enddo

write(*,*)

write(*,*)" E(SRFC/CLSTR)","    SUM OF FREQ.","          ZPE","            E(ORS)","        TOTAL ENERGY","        &
SPE(A)","           SPE(B)","           No.OF OH","                   SYSTEM"

B(I4)=0.0
do 30 J1 = I5,I6
write(*,*)A(J1),P(J1),Q(J1),W(J1),S(J1),U(J1),V(J1),M(J1),"        (ev)","           ",LIST(J1)
30 enddo

write(*,*)
write(*,*)
write(*,*)" RESULT :"
write(*,*)

write(*,*)"BINDING ENERGY            FREE ENERGY                        ELEMENTARY REACTION"
write(*,*)"ADSORBED SPEC.          ELEMENTARY STEP                            SPECIES"

do 70 J1 = I5,I6
do 90 J3 = 0,0

if(I1.eq.0.and.B(J1).GT.0.0.or.B(J1).LT.-8.0)then
B(J1)= 0.0

elseif(J1.GT.I5.and.J1.LT.I6.and.B(J1).LT.-8.0.or.B(J1).GT.0.0)then
B(J1)= 0.0

else
B(J1)= B(J1)
endif

write(*,*)B(J1),"       ",X(J1),"   ","  ( ev )","        ",LIST(J1)
write(*,*)

     if(J1+1.gt.I1)goto 1
     Y(J1+1)= X(J1+1)-X(J1)
1    Z(J1)= X(J1)-X(J3)

90 enddo
70 enddo

write(*,*)"NOTE ::"
write(*,*)"BINDING ENERGY = TOTAL ENERGY- SPE/TE(A)- SPE/TE(B)"
write(*,*)"FREE ENERGY = TOTAL ENERGY+ ZPE+ NO. OF OH ION*(-10.8671)+ E(ORS)+ E(SRFC/CLSTER)" 
write(*,*)
write(*,*)"IF BINDING ENERGY LESS THAN -8.0 eV OR MORE THAN 0.0 eV,"
write(*,*)"IT WILL BE CONSIDERED HERE AS ZERO OR CHEMISORBED SPECIES."
write(*,*)"TO SEE VALUE OF THEM, OPEN THE PROGRAMM, SWITCH OFF LINE NO.164-172,"
write(*,*)"BY PUTTING # BEFORE EACH OF THOSE LINE AND RUN IT AGAIN."
write(*,*)
if(I1.eq.0)goto 110

write(*,*)
write(*,*)"   DIFFERENCE             FREE ENERGY               COORDINATE                STEP"

J2=0
write(*,*)"                        ",X(J2),"        ",Z(J2),"         ",LIST(J2)

do 100 J1 = 1,I1
write(*,*)Y(J1),"        ",X(J1),"        ",Z(J1),"         ",LIST(J1)
100 enddo
write(*,*)

110    write(*,*)"HAVE ANY OTHER ALTERNATIVE WAY ?  PRESS '0' FOR YES"
       read(*,*)N

if(N.eq.0)goto 150

stop
end
