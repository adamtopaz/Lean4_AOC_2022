import Aoc2022.Utils
import Std.Data.HashMap.Basic
import Init.Data.Array.Basic

open System Std

namespace Day5

def input : FilePath := "/home/adam/Lean4-Projects/aoc2022/input_05"

abbrev Game := HashMap Nat (List Char)

def Game.add (G : Game) (c : Char) (i : Nat) : Game := 
  match G.find? i with 
  | none => G.insert i [c]
  | some t => G.insert i (c :: t)

def Game.pop (G : Game) (i : Nat) : Option Char × Game :=  
match G.find! i with
| [] => (none, G)
| (c :: xs) => (c, G.insert i xs)

def Game.move (G : Game) (i j : Nat) : Game := 
match Game.pop G i with 
| (none,H) => H
| (some c, H) => Game.add H c j

def Game.moves (G : Game) (n i j : Nat) : Game := Id.run $ do 
  let mut out := G
  for _ in [0:n] do 
    out := Game.move out i j
  return out 

def Game.moves' (G : Game) (n i j : Nat) : Game := Id.run $ do 
  let mut out := G
  let to_move := (G.find! i).toArray[0:n]
  out := HashMap.insert out i (out.find! i).toArray[n:].toArray.toList
  out := HashMap.insert out j (to_move.toArray.toList ++ out.find! j)
  return out

def String.filter (S : String) (p : Char → Bool) : String := 
((S.toList.filter p).map Char.toString).foldr (· ++ ·) ""

instance : Membership Char String where
  mem := fun c s => c ∈ s.toList

instance (c : Char) (s : String) : Decidable (c ∈ s) := 
show Decidable (c ∈ s.toList) from inferInstance

def parse_command (S : String) : List Nat := 
((String.filter S (fun c => c ∈ " 0123456789")).splitOn " ").filter 
  (fun t => t != "")
  |>.map String.toNat!

def first_part : IO String := do 
  let mut out : Game := HashMap.empty
  let data ← IO.FS.lines input
  let some height := data.indexOf? " 1   2   3   4   5   6   7   8   9 " | panic! "oops"
  for i in [0:height] do
    for j in [0:9] do
      let d := data[height-i-1]!
      let c := d.toList[1 + 4 * j]!
      if c ∈ "ABCDEFGHIJKLMNOPQRSTUVWXYZ" then 
        out := Game.add out c (j + 1)
  for command in data[height+2:] do 
    let cmd := parse_command command
    out := Game.moves out cmd[0]! cmd[1]! cmd[2]!
  let mut st : String := ""
  for i in [0:9] do
    st := st ++ (HashMap.find! out (i+1))[0]!.toString
  return st

def second_part : IO String := do 
  let mut out : Game := HashMap.empty
  let data ← IO.FS.lines input
  let some height := data.indexOf? " 1   2   3   4   5   6   7   8   9 " | panic! "oops"
  for i in [0:height] do
    for j in [0:9] do
      let d := data[height-i-1]!
      let c := d.toList[1 + 4 * j]!
      if c ∈ "ABCDEFGHIJKLMNOPQRSTUVWXYZ" then 
        out := Game.add out c (j + 1)
  for command in data[height+2:] do 
    let cmd := parse_command command
    out := Game.moves' out cmd[0]! cmd[1]! cmd[2]!
  let mut st : String := ""
  for i in [0:9] do
    st := st ++ (HashMap.find! out (i+1))[0]!.toString
  return st

end Day5