import Aoc2022.Utils
import Std.Data.HashMap.Basic
import Init.Data.Array.Basic
import Init.Data.Queue
import Lean

open System Std 

namespace Day10

def input : FilePath := "/home/adam/Lean4-Projects/aoc2022/input_10"

open Std

def first_part : IO Int := do
  let data ← IO.FS.lines input
  let mut cycle_number : Int := 1
  let mut register : Int := 1
  let mut values : Array Int := #[]
  for d in data do 
    match d.splitOn " " with 
    | ["noop"] => 
        values := values.push (register * cycle_number)
        cycle_number := cycle_number + 1
    | ["addx",n] => 
        let n := n.toInt!
        values := values.push (register * cycle_number)
        cycle_number := cycle_number + 1
        values := values.push (register * cycle_number)
        cycle_number := cycle_number + 1
        register := register + n
    | _ => continue
  return values[20-1]! + values[60-1]! + values[100-1]! + values[140-1]! + 
    values[180-1]! + values[220-1]!

def second_part : IO String := do
  let data ← IO.FS.lines input
  let mut register : Int := 1
  let mut cycle_number : Nat := 1
  let mut values : Array Int := #[]
  for d in data do
    match d.splitOn " " with 
    | ["noop"] => 
        values := values.push register
        cycle_number := cycle_number + 1
    | ["addx",n] => 
        let n := n.toInt!
        values := values.push register 
        cycle_number := cycle_number + 1
        values := values.push register 
        cycle_number := cycle_number + 1
        register := register + n
    | _ => continue
  let mut out : String := ""
  for i in [:values.size], x in values do
    let a : String := 
      if (i : Int) % 40 ∈ [x-1,x,x+1] then "#" else "."
    out := out.append a 
    if i % 40 == 39 then 
      out := out.append "\n"
  return out

end Day10