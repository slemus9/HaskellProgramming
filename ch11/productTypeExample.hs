data QuantumBool = QuantumTrue | QuantumFalse | QuantumBoth
 deriving (Eq, Show)

data TwoQs = MkTwoQs QuantumBool QuantumBool deriving (Eq, Show)

-- type TwoQs = (QuantumBool, QuantumBool) ; Cardinality remains the same
