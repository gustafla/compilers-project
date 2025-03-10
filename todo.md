- [ ] Errors with location
  - [X] Create appropriate struct and Error implementation
  - [ ] Parser needs to be refactored, None-cases must contain Location (`enum ParseResult`)
- [ ] Assign function to var
- [ ] Support multiple modules
- [X] Add function support
  - [X] Add module type
  - [X] Add IR generation for functions
  - [X] Make assembly generator emit code for multiple functions
    - [X] Labels in functions must be unique within the module
  - [X] Implement function parsing
  - [X] Add user-defined functions in symbol tables
    - [X] In the interpreter, map each user-defined function to a Python function that runs the interpreter on the user-defined function’s body
    - [X] In the type-checker, map each user-defined function to a function type based on the type annotations
    - [X] In the IR and Assembly generators, do the same as with built-in functions
  - [X] Make incoming parameters available as variables in all stages that use a symbol table
    - [X] Interpreter
    - [X] Type checker
    - [X] IR
    - [X] Assembly
  - [X] Change the type-checker, IR generator and Assembly generator to process function definitions in addition to the top-level expression
  - [X] Implement return ... expressions in all compiler stages
    - [X] A function whose return type is not Unit must execute a return expression.
    - [X] Test Gadget does have cases where the only return occurs within a while true loop
- [X] Add end-to-end testing
  - [X] Dynamic test generation
- [X] Assembly generator
  - [X] Corner cases like assignment, and, or
  - [X] Error handling for asm::assembler
- [X] IR generator
  - [X] Corner cases like assignment
  - [X] Make Vars and Labels integer-based
  - [ ] Tests for IR generator
- [X] Add break and continue support
  - [X] In the interpreter too
- [X] Type checker
  - [ ] Add tests for type checker
- [X] Parser
  - [X] Add tests for parser
