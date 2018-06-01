      subroutine ansort(n,a,iperm)                                              
      real*8 a(n)                                                               
      dimension iperm(n)                                                        
c----- order set of real*8 words by permutation index                           
c----- written by al vachris.. adapted from a routine                           
c----- order written by frank nolan                                             
      if(n.eq.0) go to 900                                                      
c----- initalize to identity permutation                                        
      k = 1                                                                     
      mm =iabs(n)                                                               
      do 10 i=1,mm                                                              
   10 iperm(i)=i                                                                
      ione=1                                                                    
c-----  order from lo-to-hi                                                     
  100 l = k                                                                     
      k = k + 1                                                                 
  110 do 120 i = ione,l                                                         
      index=iperm(i)                                                            
c-----  test value against ordered list                                         
      if(a(k).lt.a(index)) go to 130                                            
  120 continue                                                                  
      go to 150                                                                 
  130 j=k                                                                       
c-----  shift ordered string to make room for new value                         
      do 140 m=i,l                                                              
      iperm(j)=iperm(j-1)                                                       
  140 j=j-1                                                                     
c-----  insert value                                                            
      iperm(i)=k                                                                
  150 if(k.ge.mm) go to 160                                                     
      ione=1                                                                    
c----- if next value is larger than current insert                              
c----- begin test from the point of insertion                                   
      if(a(k+1).ge.a(k)) ione=i                                                 
      go to 100                                                                 
  160 if(n.gt.0) go to 900                                                      
c----- reverse permutation for hi-to-lo order                                   
      ione=mm/2                                                                 
      j=mm+1                                                                    
      do 170 i=1,ione                                                           
      j=j-1                                                                     
      itemp=iperm(j)                                                            
      iperm(j)=iperm(i)                                                         
  170 iperm(i)=itemp                                                            
  900 return                                                                    
      end                                                                       

