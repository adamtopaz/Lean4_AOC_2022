import Aoc2022.Utils
import Std.Data.HashMap.Basic
import Init.Data.Array.Basic
import Init.Data.Queue
import Lean

open System Std 

namespace Day7

def input : FilePath := "/home/adam/Lean4-Projects/aoc2022/input_07"

open Std

abbrev FileSystem := HashMap (Array String) (Lean.HashSet (Nat × String))

def List.isInit (a b : List α) [BEq α] : Bool := 
match a, b with 
| [], _ => true
| (_ :: _), [] => false
| (x :: xs), (y :: ys) => x == y && List.isInit xs ys

def Array.isInit [BEq α] (a b : Array α) : Bool := 
List.isInit a.toList b.toList

def FileSystem.listInit (fs : FileSystem) (d : Array String) : Array (Array String) := 
fs.toArray.map Prod.fst |>.filter <| Array.isInit d

def FileSystem.insert (fs : FileSystem) (dir : Array String) (F : Nat) (nm : String) : FileSystem := 
  match fs.find? dir with 
  | some a => HashMap.insert fs dir (a.insert (F,nm))
  | none => HashMap.insert fs dir (Lean.HashSet.empty.insert (F,nm))

def FileSystem.touchDir (fs : FileSystem) (dir : Array String) : FileSystem :=
  match fs.find? dir with 
  | some _ => fs
  | none => HashMap.insert fs dir Lean.HashSet.empty

def FileSystem.localSize (fs : FileSystem) (dir : Array String) : Nat := 
match fs.find? dir with 
  | some a => a.toList.map Prod.fst |>.sum
  | none => 0

def FileSystem.size (fs : FileSystem) (dir : Array String) : Nat := 
  FileSystem.listInit fs dir |>.map (FileSystem.localSize fs) |>.sum

def first_part : IO Nat := do
  let data := ← IO.FS.lines input
  let mut fs : FileSystem := HashMap.empty
  let mut current_dir : Array String := #[]
  let mut out : Nat := 0
  for d in data do
    match d.splitOn " " with 
    | ["$","cd","/"] => current_dir := #[]
    | ["$","cd",".."] => current_dir := current_dir.pop
    | ["$","cd",nm] => current_dir := current_dir.push nm
    | ["$",_] => continue
    | ["dir",nm] => fs := FileSystem.touchDir fs (current_dir.push nm)
    | [nm,fnm] => fs := FileSystem.insert fs current_dir nm.toNat! fnm
    | _ => panic! "oops"
  for d in fs.toList.map Prod.fst do 
    let sz := FileSystem.size fs d
    if sz ≤ 100000 then out := out + sz
  return out 

def second_part : IO Nat := do
  let data := ← IO.FS.lines input
  let mut fs : FileSystem := HashMap.empty
  let mut current_dir : Array String := #[]
  for d in data do
    match d.splitOn " " with 
    | ["$","cd","/"] => current_dir := #[]
    | ["$","cd",".."] => current_dir := current_dir.pop
    | ["$","cd",nm] => current_dir := current_dir.push nm
    | ["$",_] => continue
    | ["dir",nm] => fs := FileSystem.touchDir fs (current_dir.push nm)
    | [nm,fnm] => fs := FileSystem.insert fs current_dir nm.toNat! fnm
    | _ => panic! "oops"
  let need := 30000000 - (70000000 - fs.size #[])
  let some smallest := fs.toList.map Prod.fst 
    |>.map (FileSystem.size fs ·)
    |>.filter (· ≥ need)
    |>.minimum? | panic! "oops!"
  return smallest

end Day7