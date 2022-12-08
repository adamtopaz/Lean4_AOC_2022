import Aoc2022.Utils
import Std.Data.HashMap.Basic
import Init.Data.Array.Basic
import Init.Data.Queue
import Lean

open System Std 

namespace Day8

def input : FilePath := "/home/adam/Lean4-Projects/aoc2022/input_08"

open Std

def Map (x y : Nat) := HashMap (Fin x × Fin y) Nat

def Map.set (M : Map x y) (a : Fin x) (b : Fin y) (n : Nat) : Map x y := 
  HashMap.insert M ⟨a,b⟩ n

def Map.get (M : Map x y) (a : Fin x) (b : Fin y) : Nat := 
  match M.find? (a,b) with 
  | none => 0
  | some t => t

def Fin.lt (a : Fin x) : List (Fin x) := Id.run $ do 
  let mut out := []
  for h : i in [:a] do
    out := out.cons ⟨i, Nat.lt_trans h.2 a.2⟩
  return out

def Fin.gt (a : Fin x) : List (Fin x) := Id.run $ do
  let mut out := []
  for h : i in [a+1:x] do
    out := out.cons ⟨i,h.2⟩
  return out

def Map.visible? (M : Map x y) (a : Fin x) (b : Fin y) : Bool := 
  let height := M.get a b
  let xlt : List Bool := Fin.lt a |>.map (M.get · b < height)
  let ylt : List Bool := Fin.lt b |>.map (M.get a · < height)
  let xgt : List Bool := Fin.gt a |>.map (M.get · b < height)
  let ygt : List Bool := Fin.gt b |>.map (M.get a · < height)
  xlt.foldl (· && ·) true || ylt.foldl (· && ·) true || 
    xgt.foldl (· && ·) true || ygt.foldl (· && ·) true 

-- This approach will probably be too slow for part 2...
def first_part : IO Nat := do
  let data ← IO.FS.lines input
  let mut map : Map 99 99 := HashMap.empty
  for hi : i in [0:99] do for hj : j in [0:99] do 
    map := map.set ⟨i,hi.2⟩ ⟨j,hj.2⟩ data[i]!.toList[j]!.toString.toNat!
  return map.toList.map Prod.fst |>.map (fun (a,b) => map.visible? a b)
    |>.map (if · then 1 else 0)
    |>.sum

end Day8