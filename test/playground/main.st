language core;
extend with #let-bindings;

type X = Bool
type Z = X

fn main(n : Nat) -> [Bool] {
  return let x = [] in x
}
