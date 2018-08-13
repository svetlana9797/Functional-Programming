
squares:: [Int] ->[Int]
squares xs = [x*x| x <- xs]

odds :: [Int] ->[Int]
odds xs = [x | x <- xs, odd x]

doubleUs x y = x*2 + y*2
doubleMe x = x*2
srAr x y = 0.5* doubleUs x y 

doubleSmall x = if x<4
                then x*2
                else x*4
signumm x = if x<0
           then -1
		   else if x == 0 
		        then 0
				else 1
				
factoriall n =if n==0
              then 1
			  else n* factoriall (n - 1)
			  
fibonaccci n =if n==0 
              then 0
			  else if n==1
			       then 1
				   else fibonaccci (n-1) + fibonaccci (n-2)
				   
ssum start end = if start==end 
                then end
				else start + ssum (start+1) end
fastxptt:: Int->Int->Int
fastxptt x n  = if n==0
                then 1
				else if mod n 2==0 
                     then fastxptt (x^2) n 2
				     else x * fastxptt x (n-1)
