-module(dev_gleam_counter_ffi).
-export([coerce/1, add_one/1, add_nums/2]).

coerce(X) -> X.
add_one(N) -> N + 1.
add_nums(A, B) -> A + B.
