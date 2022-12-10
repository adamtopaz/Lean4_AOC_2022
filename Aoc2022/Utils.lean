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

def List.repeat (n : Nat) (x : α) : List α := 
  match n with 
  | 0 => []
  | (n+1) => x :: List.repeat n x

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

instance : Coe Sgn Int where
  coe
  | 0 => 0
  | 1 => 1
  | -1 => -1

def Int.toSgn (n : Int) : Sgn :=
match n with 
| Int.ofNat 0 => 0
| Int.ofNat (_+1) => 1
| Int.negSucc _ => -1

instance : Add Sgn where
  add | i, j => Int.toSgn <| i + j 

instance [HAdd α β γ] [HAdd α' β' γ'] : HAdd (α × α') (β × β') (γ × γ') where
  hAdd | (x,y), (z,w) => (x + z, y + w)

instance [HMul α β γ] [HMul α' β' γ'] : HMul (α × α') (β × β') (γ × γ') where
  hMul | (x,y), (z,w) => (x * z, y * w)

instance [HSub α β γ] [HSub α' β' γ'] : HSub (α × α') (β × β') (γ × γ') where
  hSub | (x,y), (z,w) => (x - z, y - w)

