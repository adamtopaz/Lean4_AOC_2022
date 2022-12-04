import Aoc2022.Utils

open System

namespace Day4

def input : FilePath := "/home/adam/Lean4-Projects/aoc2022/input_04"

structure Interval where 
  min : Nat 
  max : Nat
deriving Inhabited 

instance : Repr Interval where
  reprPrec := fun I _ => f!"{I.min},{I.max}"

instance : ToString Interval where
  toString := fun I => s!"{I.min},{I.max}"

def parse_interval (S : String) : Interval := 
  let T := S.splitOn "-"
  ⟨T[0]!.toNat!, T[1]!.toNat!⟩

def parse_line (S : String) : Interval × Interval := 
  let T := S.splitOn ","
  (parse_interval T[0]!, parse_interval T[1]!)

instance : HasSubset Interval where
  Subset := fun I J => J.min ≤ I.min ∧ I.max ≤ J.max

instance : DecidableRel (fun I J : Interval => I ⊆ J) := 
fun I J => show Decidable (J.min ≤ I.min ∧ I.max ≤ J.max) from inferInstance

def first_part : IO Nat := do 
  let data : Array Nat := (← IO.FS.lines input).map parse_line
    |>.map fun (I,J) => if I ⊆ J ∨ J ⊆ I then 1 else 0
  return data.sum

def overlap? (I J : Interval) : Bool := 
  (I.min ≤ J.min ∧ J.max ≤ I.max) ||
  (J.min ≤ I.min ∧ I.max ≤ J.max) || 
  (I.min ≤ J.min ∧ J.min ≤ I.max ∧ I.max ≤ J.max) ||
  (J.min ≤ I.min ∧ I.min ≤ J.max ∧ J.max ≤ I.max) 

def second_part : IO Nat := do
  let data : Array Nat := (← IO.FS.lines input).map parse_line
    |>.map fun (I,J) => if overlap? I J then 1 else 0
  return data.sum

end Day4