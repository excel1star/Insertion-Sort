      SUBROUTINE AORDER(N,A,IPERM)                                              
C----- ORDER SET OF NUMBERS BY PERMUTATION INDEX                                
C----- WRITTEN BY AL VACHRIS.. ADAPTED FROM A ROUTINE                           
C----- ORDER WRITTEN BY FRANK NOLAN                                             
      DIMENSION IPERM(1),A(1)                                                   
      IF(N.EQ.0) GO TO 900                                                      
C----- INITALIZE TO IDENTITY PERMUTATION                                        
      K = 1                                                                     
      MM =IABS(N)                                                               
      DO 10 I=1,MM                                                              
   10 IPERM(I)=I                                                                
      IONE=1                                                                    
C-----  ORDER FROM LO-TO-HI                                                     
  100 L = K                                                                     
      K = K + 1                                                                 
  110 DO 120 I = IONE,L                                                         
      INDEX=IPERM(I)                                                            
C-----  TEST VALUE AGAINST ORDERED LIST                                         
      IF(A(K).LT.A(INDEX)) GO TO 130                                            
  120 CONTINUE                                                                  
      GO TO 150                                                                 
  130 J=K                                                                       
C-----  SHIFT ORDERED STRING TO MAKE ROOM FOR NEW VALUE                         
      DO 140 M=I,L                                                              
      IPERM(J)=IPERM(J-1)                                                       
  140 J=J-1                                                                     
C-----  INSERT VALUE                                                            
      IPERM(I)=K                                                                
  150 IF(K.GE.MM) GO TO 160                                                     
      IONE=1                                                                    
C----- IF NEXT VALUE IS LARGER THAN CURRENT INSERT                              
C----- BEGIN TEST FROM THE POINT OF INSERTION                                   
      IF(A(K+1).GE.A(K)) IONE=I                                                 
      GO TO 100                                                                 
  160 IF(N.GT.0) GO TO 900                                                      
C----- REVERSE PERMUTATION FOR HI-TO-LO ORDER                                   
      IONE=MM/2                                                                 
      J=MM+1                                                                    
      DO 170 I=1,IONE                                                           
      J=J-1                                                                     
      ITEMP=IPERM(J)                                                            
      IPERM(J)=IPERM(I)                                                         
  170 IPERM(I)=ITEMP                                                            
  900 RETURN                                                                    
      END                                                                       
      SUBROUTINE RANKIT(N,IPERM,IRANK)                                          
      DIMENSION IPERM(1),IRANK(1)                                               
      DO 110 I=1,N                                                              
      J=IPERM(I)                                                                
  110 IRANK(J)=I                                                                
      RETURN                                                                    
      END                                                                       
      SUBROUTINE XCNGE(N,VALUE,IPERM,IRANK,IP,IR)                               
      DIMENSION VALUE(1),IPERM(1),IRANK(1),IP(1),IR(1)                          
      DO 110 I=1,N                                                              
      IP(I)=IPERM(I)                                                            
  110 IR(I)=IRANK(I)                                                            
      DO 120 I=1,N                                                              
      J=IP(I)                                                                   
      TEMP=VALUE(I)                                                             
      VALUE(I)=VALUE(J)                                                         
      VALUE(J)=TEMP                                                             
      K=IR(I)                                                                   
      IP(K)=J                                                                   
      IR(J)=K                                                                   
      IP(I)=I                                                                   
  120 IR(I)=I                                                                   
      RETURN                                                                    
      END                                                                       
