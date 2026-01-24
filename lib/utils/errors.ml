exception CxError of string

type kernel_error_code =
  | LexerError
  | ParserError
  | NotAdmissible
  | MalformedAxiom
  | UnknownRule
  | MalformedRule
  | NotAllowedSkolemTerm
  | RefOutOfBound
  | ImplicationFormExpected
  | ReferenceExpected
  | RuleConstraintViolation
  | AxiomViolation
  | AssumptionViolation
  | RuleViolation
  | ContextViolation
  | InvalidReference
  | UnknownProblem
  | VarExpectedInSubMap
  | DuplicateVarInSubMap

exception KernelError of kernel_error_code * Lexing.position option

let kernel_error_message = function
  | LexerError -> "illegal character"
  | ParserError -> "malformed expression"
  | NotAdmissible -> "not admissible"
  | MalformedAxiom -> "malformed axiom"
  | UnknownRule -> "unknown rule"
  | MalformedRule -> "malformed rule"
  | NotAllowedSkolemTerm -> "not allowed skolem term"
  | RefOutOfBound -> "ref out of bound"
  | ImplicationFormExpected -> "implication form expected"
  | ReferenceExpected -> "reference expected"
  | RuleConstraintViolation -> "rule constraint violation"
  | AxiomViolation -> "axiom violation"
  | AssumptionViolation -> "assumption violation"
  | RuleViolation -> "rule violation"
  | ContextViolation -> "context violation"
  | InvalidReference -> "invalid reference"
  | UnknownProblem -> "problem"
  | VarExpectedInSubMap -> "expected Var in sub_map"
  | DuplicateVarInSubMap -> "duplicate variable in sub_map"
