--- Dada a definição de números naturais a seguir:
--- data Nat = Zero | Suc Nat
---    deriving(Eq,Show)
--- implementar as seguintes funções:
data Nat = Zero | Suc Nat
   deriving(Eq,Show)
   
  
--- Implementar a função to_int que converte um Nat para um inteiro:
--- to_int :: Nat -> Int
to_int :: Nat -> Int
to_int Zero = 0
to_int (Suc n) = 1 + to_int n


--- Implementar a função to_nat que transforma um inteiro em um Nat:
--- to_nat :: Int -> Nat
to_nat :: Int -> Nat
to_nat 0 = Zero
to_nat n = Suc (to_nat (n-1)) 


--- Implementar usando recursão a função mult que multiplica dois Nats. (Você NÃO pode usar to_int ou to_nat):
--- mult :: Nat -> Nat -> Nat
soma :: Nat -> Nat -> Nat
soma Zero n = n
soma (Suc n1) n2 = soma n1 (Suc n2)

mult :: Nat -> Nat -> Nat
mult Zero (Suc n) = Zero
mult (Suc n1) n2 = soma (mult n1 (n2)) n2  


--- Implementar a função leq (menor ou igual) que recebe dois Nats e devolve um booleano.
--- (Você NÃO pode usar to_int ou to_nat):
--- leq :: Nat -> Nat -> Bool
leq :: Nat -> Nat -> Bool
leq Zero n1 = True
leq n2 Zero = False
leq (Suc n1) (Suc n2) = leq n1 n2

--- Implementar a função sub que executa a subtração de dois naturais (Você NÃO pode usar to_int ou to_nat):
--- sub :: Nat -> Nat -> Nat
--- 3 - 2
--- 2 - 1
--- 1 - 0 = 1
sub :: Nat -> Nat -> Nat
sub n Zero = n
sub n1 (Suc n2) = sub (Suc n1) n2
  

  
  