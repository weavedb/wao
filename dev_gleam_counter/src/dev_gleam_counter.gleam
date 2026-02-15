//// A HyperBEAM counter device implemented in Gleam.
//// Demonstrates that Gleam modules work as HyperBEAM devices since
//// they compile to BEAM bytecode on the same VM.
////
//// Device name: gleam-counter@1.0

/// Opaque type for dynamically-typed Erlang terms.
/// Gleam is statically typed but HyperBEAM passes dynamic Erlang maps,
/// so we bridge the gap with this type and FFI helpers.
pub type ErlTerm

/// Erlang FFI — called at runtime on the same BEAM VM
@external(erlang, "hb_ao", "set")
fn hb_ao_set(msg: ErlTerm, data: ErlTerm, opts: ErlTerm) -> ErlTerm

@external(erlang, "maps", "get")
fn maps_get(key: ErlTerm, map: ErlTerm, default: ErlTerm) -> ErlTerm

@external(erlang, "maps", "from_list")
fn make_map(pairs: List(#(String, ErlTerm))) -> ErlTerm

/// FFI helpers from companion .erl file for dynamic type bridging
@external(erlang, "dev_gleam_counter_ffi", "coerce")
fn coerce(value: a) -> ErlTerm

@external(erlang, "dev_gleam_counter_ffi", "add_one")
fn add_one(n: ErlTerm) -> ErlTerm

@external(erlang, "dev_gleam_counter_ffi", "add_nums")
fn add_nums(a: ErlTerm, b: ErlTerm) -> ErlTerm

pub fn info(msg: ErlTerm, _msg2: ErlTerm, opts: ErlTerm) -> Result(ErlTerm, Nil) {
  let data = make_map([#("version", coerce("1.0"))])
  Ok(hb_ao_set(msg, data, opts))
}

pub fn init(msg: ErlTerm, _msg2: ErlTerm, opts: ErlTerm) -> Result(ErlTerm, Nil) {
  let data = make_map([#("num", coerce(0))])
  Ok(hb_ao_set(msg, data, opts))
}

pub fn inc(msg1: ErlTerm, _msg2: ErlTerm, opts: ErlTerm) -> Result(ErlTerm, Nil) {
  let num = maps_get(coerce("num"), msg1, coerce(0))
  let data = make_map([#("num", add_one(num))])
  Ok(hb_ao_set(msg1, data, opts))
}

pub fn add(msg1: ErlTerm, msg2: ErlTerm, opts: ErlTerm) -> Result(ErlTerm, Nil) {
  let num = maps_get(coerce("num"), msg1, coerce(0))
  let plus = maps_get(coerce("plus"), msg2, coerce(0))
  let data = make_map([#("num", add_nums(num, plus))])
  Ok(hb_ao_set(msg1, data, opts))
}

pub fn get(msg1: ErlTerm, _msg2: ErlTerm, _opts: ErlTerm) -> Result(ErlTerm, Nil) {
  Ok(msg1)
}

pub fn compute(msg1: ErlTerm, _msg2: ErlTerm, _opts: ErlTerm) -> Result(ErlTerm, Nil) {
  Ok(msg1)
}

pub fn snapshot(msg: ErlTerm, _msg2: ErlTerm, _opts: ErlTerm) -> Result(ErlTerm, Nil) {
  Ok(msg)
}

pub fn normalize(msg: ErlTerm, _msg2: ErlTerm, _opts: ErlTerm) -> Result(ErlTerm, Nil) {
  Ok(msg)
}
