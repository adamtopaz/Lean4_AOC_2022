import Aoc2022.Utils

open System

namespace Day2

def input : FilePath := "/home/adam/Lean4-Projects/aoc2022/input_02"

def R : Nat := 1
def P : Nat := 2
def S : Nat := 3
def L : Nat := 0
def D : Nat := 3
def W : Nat := 6

def parse_line_1 : String → Nat 
| "A X" => R + D
| "A Y" => P + W
| "A Z" => S + L
| "B X" => R + L
| "B Y" => P + D
| "B Z" => S + W
| "C X" => R + W
| "C Y" => P + L
| "C Z" => S + D
| _ => panic! "oops!"

def first_part : IO Nat := return ((← IO.FS.lines input).map parse_line_1).sum

def parse_line_2 : String → Nat 
| "A X" => S + L
| "A Y" => R + D
| "A Z" => P + W
| "B X" => R + L
| "B Y" => P + D
| "B Z" => S + W
| "C X" => P + L
| "C Y" => S + D
| "C Z" => R + W
| _ => panic! "oops!"

def second_part : IO Nat := return ((← IO.FS.lines input).map parse_line_2).sum

end Day2