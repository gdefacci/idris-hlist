module Data.HList

import Data.Vect

%access public export
%default total

data HList: (n :Nat) -> Vect n Type -> Type where
  Nil : HList 0 []
  (::) : t -> (HList n tps) -> HList (S n) (t :: tps)

get : (fin:Fin n1) -> HList n1 tps -> (index fin tps)
get FZ (y :: z) = y
get (FS x) (y :: z) = get x z

set : (fin:Fin n1) -> HList n1 tps -> (index fin tps) -> HList n1 tps
set FZ (x :: z) y = y :: z
set (FS s) (x :: w) y = x :: (set s w y)

head : HList (S len) (h :: tps) -> h
head (x :: y) = x

LastType : HList (S n) (a :: tps) -> Type
LastType {a} (x :: []) = a
LastType {a} (x :: l1 @ (t :: ts)) = LastType l1

last : ( l : HList (S n) (t :: ts) ) -> LastType l
last (x :: []) = x
last (x :: l1 @ (y :: z)) = last l1

ZipTypeVect : HList n ts1 -> HList n ts2 -> Vect n Type
ZipTypeVect {ts1 = []} {ts2 = []} [] [] = []
ZipTypeVect {ts1 = (t :: tps)} {ts2 = (y :: xs)} (x :: z) (w :: s) = (t, y) :: ZipTypeVect z s

ZipType : HList n ts1 -> HList n ts2 -> Type
ZipType {n} x y = HList n (ZipTypeVect x y)

zip : (l1 : HList n ts1) -> ( l2 : HList n ts2) -> ZipType l1 l2
zip [] [] = []
zip (x :: y) (z :: w) = (x, z) :: zip y w

length : HList n ts1 -> Nat
length {n} x = n

take : (n : Nat) -> ( l : HList (n + m) ts ) -> HList n (take n ts)
take Z l = []
take (S k) (x :: y) = x :: take k y

drop : (n : Nat) -> ( l : HList (n + m) ts ) -> HList m (drop n ts)
drop Z l = l
drop (S k) (x :: y) = drop k y

init : HList (S len) (x :: ts) -> HList len (init (x :: ts))
init (x :: []) = []
init (x :: l @ (y :: z)) = x :: (init l)

tail : HList (S len) (x :: ts) -> HList len ts
tail (x :: y) = y

(++) : ( xs : HList m tms ) -> (ys : HList n tns ) -> HList (m + n) (tms ++ tns)
(++) [] ys = ys
(++) (x :: y) ys = x :: (y ++ ys)

updateTypeVect : (fin:Fin n) -> (HList n tps) -> Type -> Vect n Type
updateTypeVect {n = (S k)} {tps = (t :: xs)} FZ (x :: z) y = y :: xs
updateTypeVect {n = (S k)} {tps = (t :: xs)} (FS z) (x :: w) y = t :: (updateTypeVect z w y)

UpdateType : (fin:Fin n) -> (HList n tps) -> Type -> Type
UpdateType fin x y = HList _ $ updateTypeVect fin x y

update : (fin:Fin n) -> (l : HList n tps) -> t -> (UpdateType fin l t)
update FZ (y :: z) x = x :: z
update (FS y) (z :: w) x = z :: (update y w x)

insertAtTypeVect : (fin:Fin (S n)) -> (HList n tps) -> Type -> Vect (S n) Type
insertAtTypeVect {n = n} {tps = tps} FZ x y = y :: tps
insertAtTypeVect {n = Z} {tps = []} (FS z) [] y = [y]
insertAtTypeVect {n = (S k)} {tps = (t :: xs)} (FS z) (x :: w) y = t :: (insertAtTypeVect z w y)

InsertAtType : (fin:Fin (S n)) -> (HList n tps) -> Type -> Type
InsertAtType fin x y = HList _ $ insertAtTypeVect fin x y

insertAt : (fin:Fin (S n)) -> (l : HList n tps) -> t -> InsertAtType fin l t
insertAt FZ l x = x :: l
insertAt (FS y) [] x = [x]
insertAt (FS y) (z :: w) x = z :: insertAt y w x

deleteAtTypeVect : (fin:Fin (S n)) -> (HList (S n) tps) -> Vect n Type
deleteAtTypeVect {tps = (t :: xs)} FZ (x :: y) = xs
deleteAtTypeVect {tps = (t :: (y :: tps))} (FS FZ) (x :: (z :: w)) = t :: tps
deleteAtTypeVect {tps = (t :: xs)} (FS (FS y)) (x :: z) = t :: (deleteAtTypeVect (FS y) z)

DeleteAtType : (fin:Fin (S n)) -> (HList (S n) tps) -> Type
DeleteAtType fin x = HList _ $ deleteAtTypeVect fin x

deleteAt : (fin:Fin (S n)) -> (l : HList (S n) tps) -> (DeleteAtType fin l)
deleteAt FZ (x :: y) = y
deleteAt (FS FZ) (y :: (x :: z)) = y :: z
deleteAt (FS (FS x)) (y :: z) = y :: deleteAt (FS x) z

implementation Eq (HList 0 []) where
  (==) []      []      = True

implementation (Eq a, Eq (HList len elems)) => Eq (HList (S len) (a :: elems)) where
  (==) (x::xs) (y::ys) = x == y && xs == ys
