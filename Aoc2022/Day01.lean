import Std
import Aoc2022.Utils

open System

namespace Day1

def input : FilePath := "/home/adam/Lean4-Projects/aoc2022/input_01"

def first_part : IO Nat := do
  let data := ((← IO.FS.lines input).splitOn "").map 
    fun L => (L.map String.toNat!).sum
  match data.getMax? (fun m n => m ≤ n) with
  | some n => return n
  | none => panic! "oops"

def second_part : IO Nat := do
  let data := ((← IO.FS.lines input).splitOn "").map 
    fun L => (L.map String.toNat!).sum
  let data := data.qsort (fun m n => m ≥ n)
  return data[0:3].toArray.sum

end Day1