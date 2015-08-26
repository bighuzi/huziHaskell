module Main where
	main = putStrLn "Hello World!"
	
	adjective =["fast","slow","bright","dark"]
	nouns =["frog","fish","cat"]
	name =[x++" "++y|x<-adjective,y<-nouns]
	c = [x^2|x<-[1..20],odd x,x/=5]
	l1 = [1]++[2]	--together
	l2 = 1:[2,3]	--add head
	l3 = [1,2,3]!!2 --from 0 
	l4 = [1]>[2]	--
	l5 = head c
	l6 = tail c
	l7 = last c
	l8 = init c

	l9 = length c
	l10= null c
	l11= reverse c
	l12= take	2 c
	l13= drop 3
	l14= minimum c
	l15= maximum c
	l16= sum c
	l17= product c
	l18= elem 1 c
	l19= cycle [1,2,3]
	l20= repeat c
	t1=(1,2)
	foo ::[Char]->[Char]
	foo x = x ++ " "
	factorial :: Integer -> Integer
	factorial n = product[1..n]

	lucky::(Integral a) => a-> String
	lucky 7 ="Lucky"
	lucky x ="sorry,you are not lucky enough"

	len'::[a]->Integer
	len' [] =0
	len' xss@(_:xs) = 1+(len' xs)

	len''::(Eq a)=>[a]->Integer
	len'' xss
		| xss==[] =0
		| otherwise =1+(len'' (tail xss))

	calcBmi::(RealFloat a)=>[(a,a)]->[a]
	calcBmi xs = [bmi|(height,weight)<-xs,let bmi = weight/height^2,bmi>20]

	maximum' :: (Ord a)=>[a]->a
	maximum' [] = error"Error,No Maximum"
	maximum' [x]=x
	maximum' (x:xs) =max x (maximum' xs)	

	replicate' :: Int->a->[a]
	replicate' n a
		| n<=0 = []
		|otherwise = a:replicate' (n-1) a

	reverse' ::[a]->[a]
	reverse' [] = []
	reverse' (x:xs)=reverse' (xs) ++[x]

	zip' ::[a]->[a]->[(a,a)]
	zip' _ [] =[]
	zip' [] _ = []
	zip' (x:xs) (y:ys) =(x,y):zip' xs ys

	elem' ::(Eq a)=>a->[a]->Bool
	elem' x [] = False
	elem' x (y:ys) 
		|x==y = True
		|otherwise = elem' x ys

	quicksort :: (Ord a)=>[a]->[a]
	quicksort [] 		=[]
	quicksort (a:xs) 	=
		let smaller 	=[y|y<-xs,y<a];
			larger 		=[y|y<-xs,y>=a]
		in quicksort smaller++[a]++ quicksort larger	

	quicksort' ::(Ord a)=>[a]->[a]
	quicksort' []	=[]	
	quicksort' (a:xs) =
		let 	small = [x|x<-xs,x<=a];
				large = [x|x<-xs,x>a]
		in 	quicksort small ++ [a]++quicksort large

--Thinking calculate the char number of a String

	comparetohundred :: Int->Ordering
	comparetohundred = compare 100

	divideByten :: (Fractional a)=> a->a
	divideByten =(10/)

--elem a->([a]->Bool)
	isUpperAlpha::Char->Bool
	isUpperAlpha =(`elem` ['A'..'Z'])
	
	twicefun::(a->a)->a->a
	twicefun f x = f (f x)

	zipWith'::(a->b->c)->[a]->[b]->[c]
	zipWith' _ _ [] =[]
	zipWith' _ [] _ =[]
	zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs

	flip''::(a->b->c)->(b->a->c)
	flip'' f  = g
		where g x y = f y x

	flip'::(a->b->c)->b->a->c
	flip' f x y = f y x

	map'::(a->b)->[a]->[b]
	map' _ [] =[]
	map' f (a:as)= f a : map' f as

	filter'::(a->Bool)->[a]->[a]
	filter' _ [] =[]
	filter' f (a:as) 
		|f a  		= a : filter' f as
		|otherwise 	= filter' f as
--fold something
--foldl,foldr,fold::[a]
	sum'fold::Num a => [a]->a
	sum'fold xs =foldl (+) 0 xs

	sum''fold ::Num a =>[a]->a
	sum''fold = foldl (\acc x ->acc+x) 0 

	map'fold::(a->b)->[a]->[b]
	map'fold f = foldr (\x acc->f x:acc) []
	--reconsider the code above

	-- scanl (+) [1,2,3,4]
	-- sqrtSum::[Int]
	-- sqrtSum =takeWhile (<10000) (scanl1 (+) (map sqrt [1..]))

	data Person = Person {firstName::String
						 ,lastName::String
						 ,age::Int
						 ,height::Float
						 ,phoneNumber::String
						 ,flavor::String} deriving(Show)

 	-- data Bool = False|True
 	-- data Int  = -10|...|-1|0|1|...|10
 	data Shape = Circle Float Float Float|Rectangle Float Float Float Float  deriving (Show)
 	
						 
	