### Parser + Lexer work

This folder's purpose is to keep track of the work that has been in regards to the hand-written parser.
We have implemented the followings :

- `ReadWriteStateResult` Monad that handles all these cases (maybe we do not need the Read and write!!) + handles Failures as part of the Try with => we shall use a DU to represent Failures.
- We lex using AP, and generate Indent and Dedent tokens
- We parse the token stream by type-instanciating the `RWSResult` monad.
