import Aoc2022.Utils
import Std.Data.HashMap.Basic
import Init.Data.Array.Basic
import Init.Data.Queue

open System Std

namespace Day6

def input : FilePath := "/home/adam/Lean4-Projects/aoc2022/input_06"

open Std

def part (n : Nat) : IO Nat := do 
  let data := (← IO.FS.lines input)[0]!.toList
  let mut D : Queue Char := Queue.empty
  for i in [0:n] do
    D := D.enqueue data[i]!
  for i in [n:data.length] do
    D := D.enqueue data[i]!
    if D.noDups then return (i+1) 
    match D.dequeue? with 
    | some (_,t) => D := t
    | none => panic! "oops"
  return 0

end Day6