import Aoc2022.Utils
import Std.Data.HashMap.Basic
import Init.Data.Array.Basic
import Init.Data.Queue
import Lean

open System Std 

namespace Day9

def input : FilePath := "/home/adam/Lean4-Projects/aoc2022/input_09"

open Std

/-
-- This is the original solution to part 1
-- but part 2 was written to take care of arbitrary lengths.

inductive Sgn where | zero | one | neg_one deriving Inhabited

instance : OfNat Sgn (nat_lit 0) where ofNat := .zero

instance : OfNat Sgn (nat_lit 1) where ofNat := .one

instance : Neg Sgn where 
  neg 
  | .zero => .zero 
  | .one => .neg_one 
  | .neg_one => .one

instance : ToString Sgn where
  toString
  | 0 => "0"
  | 1 => "1"
  | (-1) => "-1"

instance : Mul Sgn where
  mul
  | .zero, _ => .zero
  | _, .zero => .zero
  | .one, x => x
  | x, .one => x
  | .neg_one, .neg_one => .neg_one

inductive Cmd where | U | D | L | R deriving Inhabited, Repr

instance : ToString Cmd where
  toString
  | .R => "R"
  | .L => "L"
  | .U => "U"
  | .D => "D"

notation "R" => Cmd.R
notation "U" => Cmd.U
notation "D" => Cmd.D
notation "L" => Cmd.L

instance : HMul Cmd (Sgn × Sgn) (Sgn × Sgn) where
  hMul
  | U, (_,-1) => (0,-1)
  | D, (_,1) => (0,1)
  | R, (-1,_) => (-1,0)
  | L, (1,_) => (1,0)
  | U, (x,0) => (x,-1)
  | U, (x,1) => (x,0)
  | D, (x,-1) => (x,0)
  | D, (x,0) => (x,1)
  | L, (0,x) => (1,x)
  | L, (-1,x) => (0,x)
  | R, (1,x) => (0,x)
  | R, (0,x) => (-1,x)

instance : HMul Cmd (Int × Int) (Int × Int) where
  hMul
  | U, (i,j) => (i,j+1)
  | D, (i,j) => (i,j-1)
  | R, (i,j) => (i+1,j)
  | L, (i,j) => (i-1,j)

instance : Coe Sgn Int where
  coe
  | 0 => 0
  | 1 => 1
  | -1 => -1

instance : HAdd Int Sgn Int where
  hAdd | i, j => i + j

instance : HAdd Sgn Int Int where
  hAdd | i, j => i + j

instance [HAdd α β γ] [HAdd α' β' γ'] : HAdd (α × α') (β × β') (γ × γ') where
  hAdd | (x,y), (z,w) => (x + z, y + w)

instance [HMul α β γ] [HMul α' β' γ'] : HMul (α × α') (β × β') (γ × γ') where
  hMul | (x,y), (z,w) => (x * z, y * w)

def first_part : IO Nat := do
  let data ← IO.FS.lines input
  let mut visited : Lean.HashSet (Int × Int) := Lean.HashSet.empty
  let mut H_pos : Int × Int := (0,0)
  let mut T_pos : Int × Int := (0,0)
  let mut dir : Sgn × Sgn := (0,0) 
  for d in data do 
    let [cmd,ct] := d.splitOn " " | panic! ""
    let cmd : Cmd := 
      match cmd with 
      | "R" => .R
      | "L" => .L
      | "U" => .U
      | "D" => .D
      | _ => unreachable!
    let some ct := ct.toNat? | unreachable!
    for _ in [:ct] do
      visited := visited.insert T_pos
      H_pos := cmd * H_pos
      dir := cmd * dir
      T_pos := H_pos + dir
  visited := visited.insert T_pos
  return visited.toArray.size

-/

instance : Coe (Sgn × Sgn) (Int × Int) where
  coe | (i,j) => (i,j)

instance : Coe (Int × Int) (Sgn × Sgn) where
  coe | (i,j) => (i.toSgn,j.toSgn)

structure Snake where data : List (Int × Int) deriving Repr

instance : ToString Snake where toString | ⟨S⟩ => toString S

def Snake.tail (S : Snake) : Int × Int :=
  match S.data.last' with 
  | none => (0,0)
  | some t => t

def moveToward : Int × Int → Int × Int → Int × Int := fun (a,b) (x,y) => Id.run $ do
  let (u,v) := (a,b) - (x,y)
  let mut δ : Sgn × Sgn := (0,0)
  match u, v with
  | 2, 2 => δ := (1,1)
  | 2, 1 => δ := (1,1)
  | 2, 0 => δ := (1,0)
  | 2, -1 => δ := (1,-1)
  | 2, -2 => δ := (1,-1)
  | 1, -2 => δ := (1,-1)
  | 0, -2 => δ := (0,-1)
  | -1, -2 => δ := (-1,-1)
  | -2, -2 => δ := (-1,-1)
  | -2, -1 => δ := (-1,-1)
  | -2, 0 => δ := (-1,0)
  | -2, 1 => δ := (-1,1)
  | -2, 2 => δ := (-1,1)
  | -1, 2 => δ := (-1,1)
  | 0, 2 => δ := (0,1)
  | 1, 2 => δ := (1,1)
  | _, _ => δ := (0,0)
  return (x,y) + δ

def hMul_aux : Sgn × Sgn → Snake → Snake := fun c S => Id.run $ do
  let mut out : Array (Int × Int) := #[]
  let n := S.data.length
  out := out.insertAt! 0 (S.data.head! + c) 
  for i in [1:n] do
    let t := out[i-1]!
    let q := S.data[i]!
    out := out.push (moveToward t q) 
  return ⟨out.toList⟩

instance : HMul (Sgn × Sgn) Snake Snake where
  hMul := hMul_aux

notation "H" => ((0,0) : Sgn × Sgn)
notation "R" => ((1,0) : Sgn × Sgn)
notation "L" => ((-1,0) : Sgn × Sgn)
notation "D" => ((0,-1) : Sgn × Sgn)
notation "U" => ((0,1) : Sgn × Sgn)
notation "UR" => ((1,1) : Sgn × Sgn)
notation "UL" => ((-1,1) : Sgn × Sgn)
notation "DR" => ((1,-1) : Sgn × Sgn)
notation "DL" => ((-1,-1) : Sgn × Sgn)

def part (n : Nat) : IO Nat := do
  let data ← IO.FS.lines input
  let mut visited : Lean.HashSet (Int × Int) := Lean.HashSet.empty
  let mut S : Snake := Snake.mk <| List.repeat n (0,0)
  for d in data do 
    let [cmd,ct] := d.splitOn " " | panic! ""
    let cmd : Sgn × Sgn := 
      match cmd with 
      | "R" => R
      | "L" => L
      | "U" => U
      | "D" => D
      | _ => unreachable!
    let some ct := ct.toNat? | unreachable!
    for _ in [:ct] do
      visited := visited.insert S.tail
      S := cmd * S
    visited := visited.insert S.tail
  return visited.toArray.size

end Day9