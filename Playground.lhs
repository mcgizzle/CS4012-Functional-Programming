>{-# LANGUAGE GADTs, ExistentialQuantification #-}
>module Playground where

GADTS 

>data Expr a where
>        Add :: Num a => Expr a -> Expr a -> Expr a
>        Eq  :: Eq a => Expr a -> Expr a -> Expr Bool

We can use GADT's to provide run-time information about whether or not the list is empty
If the list contains 'Z' we know it is empty

>data Z
>data S n
>data List a n where
>        Nil  :: List a Z
>        Cons :: a -> List a b -> List a (S b)

By using the fact that the list must be non-empty we can get the help of the type checker to prevent us writing:
let emptylist = Nil
safeHead emptyList
This will return a type error as 'List a Z' will not match the expected 'List a (S n)'

>safeHead :: List a (S n) -> a 
>safeHead (Cons a _ ) = a

This comes with some caveats ofcourse, we can no longer wirte the following function
f 0 = Nil
f 1 = Cons 1 Nil

'f 0' binds 'List a Z' to the type of 'f'
The compiler will shout at us because expected 'List a Z' will not match the impossible 'List a (S Z)'

AVL Tree

>data AVL a n where
>       Empty :: AVL a Z
>       Node  :: AVL a b -> AVL a b -> AVL a (S b)
>       LNode :: AVL a (S b) -> AVL a b -> AVL a (S (S b))
>       RNode :: AVL a b -> AVL a (S b) -> AVL a (S (S b))

Heterogenous List

>data HList0 where
>       HNil0  :: HList0
>       HCons0 :: a -> HList0 -> HList0 

        HCons0 :: Show a -> HList0i -> HList0
        HCons0 :: (a, a -> String) -> HList0

Existential Quantification 

Useless
We dont know anything about the types, so we cannot perform any computations

>data HList = HNil
>             | forall a. HCons a HList 

Useful
Constrain it to the class Showable, which provides useful functions that can used accross all types in the list

>data HList1 = HNil1
>              | forall a. Show a => HCons1 a HList1

>printList1 :: HList1 -> IO ()
>printList1 HNil1 = return ()
>printList1 (HCons1 x xs) = putStrLn (show x) >> printList1 xs

Package our own functions up with the list

>data HList2 = HNil2
>              | forall a. HCons2 (a,a -> String) HList2

>printList2 :: HList2 -> IO ()
>printList2 HNil2 = return ()
>printList (HCons2 (x,s) xs) = putStrLn (s x) >> printList2 xs

Phantom Types
data Ptr a = MkPtr Addr
Say we had the following functions 
peek :: Ptr a -> IO a
poke :: Ptr a -> a -> IO ()
The compiler will protect us from the following
        do 
          ptr <- allocPtr
          poke ptr (42 :: Int)
          f :: Float <- peek ptr

