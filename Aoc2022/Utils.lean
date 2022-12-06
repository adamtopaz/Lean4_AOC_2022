import Std.Data.List.Basic

def Array.splitOn {α : Type u_1} [BEq α] (x : α) :
  Array α → Array (Array α) := 
  fun L => (L.toList.splitOn x).toArray.map List.toArray

def List.sum [Add α] [OfNat α (nat_lit 0)] : List α → α
  | [] => 0
  | (x :: xs) => x + xs.sum

def Array.sum [Add α] [OfNat α (nat_lit 0)] : Array α → α :=
  fun L => L.toList.sum

def Std.Queue.noDups (D : Queue α) := 
  D.toArray.toList.Pairwise (· ≠ ·)

instance (D : Std.Queue α) [DecidableRel ((· ≠ ·) : α → α → Prop)] : 
  Decidable D.noDups := by
  unfold Std.Queue.noDups
  exact inferInstance