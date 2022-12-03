import Aoc2022.Utils

open System

namespace Day3

def priority (c : Char) : Nat := 
1 + "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".toList.indexOf c

def get_matches (A B : List α) [DecidableEq α] : List α := Id.run $ do 
  let mut out : List α := []
  for a in A do 
    if a ∈ B ∧ a ∉ out then out := out.cons a
  return out

def input : FilePath := "/home/adam/Lean4-Projects/aoc2022/input_03"

def first_part : IO Nat := do 
  let data ← IO.FS.lines input
  let mut result : Nat := 0
  for d in data do
    let l := d.length
    -- This can't be the best way to split a string!
    let d1 := d.toList.toArray[0:l/2].toArray.toList 
    let d2 := d.toList.toArray[l/2:l].toArray.toList
    result := result + ((get_matches d1 d2).map priority |>.sum)
  return result

def get_bunches (L : List α) (n : Nat) : List (List α) := Id.run $ do 
  let mut out : List (List α) := []
  let mut temp : List α := []
  for l in L do 
    temp := temp.cons l
    if temp.length == n 
    then 
      out := out.cons temp
      temp := []
  return out

def second_part : IO Nat := do
  let data := get_bunches (← IO.FS.lines input).toList 3
  let mut result : Nat := 0
  for d in data do 
    let m := get_matches (get_matches d[0]!.toList d[1]!.toList) d[2]!.toList
    result := result + priority m[0]!
  return result 

end Day3