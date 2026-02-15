(module
  ;; 1 page = 64KB of memory (memory64 - i64 indexed)
  (memory (export "memory") i64 1)

  ;; Global: bump allocator pointer (starts at 4096 to leave room for data)
  (global $bump (mut i64) (i64.const 4096))
  ;; Global: counter value (pure integer, not a pointer)
  (global $counter (mut i32) (i32.const 0))

  ;; ===== Data section: pre-baked JSON response templates =====
  ;; Offset 0: response prefix — puts counter value as a Message in outbox
  ;; {"ok":true,"response":{"Output":{"data":""},"Messages":[{"Data":"
  (data (i64.const 0) "{\"ok\":true,\"response\":{\"Output\":{\"data\":\"\"},\"Messages\":[{\"Data\":\"")
  ;; Length = 65

  ;; Offset 80: response suffix
  ;; "}]}}
  (data (i64.const 80) "\"}]}}")
  ;; Length = 5

  ;; ===== Search patterns =====
  ;; "Inc" pattern at offset 128
  (data (i64.const 128) "\"name\":\"Action\",\"value\":\"Inc\"")
  ;; Length = 29
  ;; "Get" pattern at offset 192
  (data (i64.const 192) "\"name\":\"Action\",\"value\":\"Get\"")
  ;; Length = 29

  ;; ===== malloc: bump allocator =====
  (func $malloc (export "malloc") (param $size i64) (result i64)
    (local $ptr i64)
    (local.set $ptr (global.get $bump))
    (global.set $bump (i64.add (global.get $bump) (local.get $size)))
    (local.get $ptr)
  )

  ;; ===== free: no-op =====
  (func $free (export "free") (param $ptr i64) (result i64)
    (i64.const 0)
  )

  ;; ===== Helper: decimal i32 to string =====
  ;; Writes decimal representation of $val at $dst, returns length as i64
  (func $itoa (param $val i32) (param $dst i64) (result i64)
    (local $tmp i32)
    (local $digits i64)
    (local $i i64)
    (local $d i32)

    ;; Handle 0
    (if (i32.eqz (local.get $val))
      (then
        (i32.store8 (local.get $dst) (i32.const 48)) ;; '0'
        (return (i64.const 1))
      )
    )

    ;; Count digits
    (local.set $tmp (local.get $val))
    (local.set $digits (i64.const 0))
    (block $done
      (loop $count
        (br_if $done (i32.eqz (local.get $tmp)))
        (local.set $tmp (i32.div_u (local.get $tmp) (i32.const 10)))
        (local.set $digits (i64.add (local.get $digits) (i64.const 1)))
        (br $count)
      )
    )

    ;; Write digits in reverse
    (local.set $tmp (local.get $val))
    (local.set $i (i64.sub (local.get $digits) (i64.const 1)))
    (block $done2
      (loop $write
        (br_if $done2 (i64.lt_s (local.get $i) (i64.const 0)))
        (local.set $d (i32.rem_u (local.get $tmp) (i32.const 10)))
        (i32.store8
          (i64.add (local.get $dst) (local.get $i))
          (i32.add (local.get $d) (i32.const 48)) ;; '0' + digit
        )
        (local.set $tmp (i32.div_u (local.get $tmp) (i32.const 10)))
        (local.set $i (i64.sub (local.get $i) (i64.const 1)))
        (br $write)
      )
    )
    (local.get $digits)
  )

  ;; ===== Helper: copy N bytes from src to dst =====
  (func $memcpy (param $dst i64) (param $src i64) (param $len i64)
    (local $i i64)
    (local.set $i (i64.const 0))
    (block $done
      (loop $copy
        (br_if $done (i64.ge_u (local.get $i) (local.get $len)))
        (i32.store8
          (i64.add (local.get $dst) (local.get $i))
          (i32.load8_u (i64.add (local.get $src) (local.get $i)))
        )
        (local.set $i (i64.add (local.get $i) (i64.const 1)))
        (br $copy)
      )
    )
  )

  ;; ===== Helper: search for substring in memory =====
  ;; Returns 1 if found, -1 if not found
  (func $strstr (param $haystack i64) (param $max_len i64) (param $needle i64) (result i32)
    (local $i i64)
    (local $j i64)
    (local $nlen i64)
    (local $match i32)

    ;; Get needle length (null-terminated)
    (local.set $nlen (i64.const 0))
    (block $nend
      (loop $ncount
        (br_if $nend (i32.eqz (i32.load8_u (i64.add (local.get $needle) (local.get $nlen)))))
        (local.set $nlen (i64.add (local.get $nlen) (i64.const 1)))
        (br $ncount)
      )
    )

    ;; Search
    (local.set $i (i64.const 0))
    (block $notfound
      (loop $search
        (br_if $notfound (i64.gt_u (i64.add (local.get $i) (local.get $nlen)) (local.get $max_len)))
        ;; Check if haystack[i..i+nlen] == needle
        (local.set $j (i64.const 0))
        (local.set $match (i32.const 1))
        (block $nomatch
          (loop $cmp
            (br_if $nomatch (i64.ge_u (local.get $j) (local.get $nlen)))
            (if (i32.ne
              (i32.load8_u (i64.add (i64.add (local.get $haystack) (local.get $i)) (local.get $j)))
              (i32.load8_u (i64.add (local.get $needle) (local.get $j)))
            )
              (then
                (local.set $match (i32.const 0))
                (br $nomatch)
              )
            )
            (local.set $j (i64.add (local.get $j) (i64.const 1)))
            (br $cmp)
          )
        )
        (if (local.get $match) (then (return (i32.const 1)))) ;; found
        (local.set $i (i64.add (local.get $i) (i64.const 1)))
        (br $search)
      )
    )
    (i32.const -1) ;; not found
  )

  ;; ===== Helper: strlen (null-terminated) =====
  (func $strlen (param $ptr i64) (result i64)
    (local $len i64)
    (local.set $len (i64.const 0))
    (block $done
      (loop $count
        (br_if $done (i32.eqz (i32.load8_u (i64.add (local.get $ptr) (local.get $len)))))
        (local.set $len (i64.add (local.get $len) (i64.const 1)))
        (br $count)
      )
    )
    (local.get $len)
  )

  ;; ===== handle: main entry point =====
  ;; Called by json-iface@1.0 with pointers to JSON strings
  (func $handle (export "handle") (param $msg_ptr i64) (param $proc_ptr i64) (result i64)
    (local $msg_len i64)
    (local $out_ptr i64)
    (local $pos i64)
    (local $num_len i64)
    (local $found i32)

    ;; Get message length
    (local.set $msg_len (call $strlen (local.get $msg_ptr)))

    ;; Check for "Inc" action
    (local.set $found (call $strstr (local.get $msg_ptr) (local.get $msg_len) (i64.const 128)))
    (if (i32.ne (local.get $found) (i32.const -1))
      (then
        ;; Increment counter
        (global.set $counter (i32.add (global.get $counter) (i32.const 1)))
      )
    )

    ;; Check for "Get" action (only if not Inc)
    (if (i32.eq (local.get $found) (i32.const -1))
      (then
        (local.set $found (call $strstr (local.get $msg_ptr) (local.get $msg_len) (i64.const 192)))
      )
    )

    ;; Allocate output buffer (256 bytes)
    (local.set $out_ptr (call $malloc (i64.const 256)))
    (local.set $pos (i64.const 0))

    ;; Write prefix (65 bytes):
    ;; {"ok":true,"response":{"Output":{"data":""},"Messages":[{"Data":"
    (call $memcpy (local.get $out_ptr) (i64.const 0) (i64.const 65))
    (local.set $pos (i64.const 65))

    ;; Write counter value as Message Data
    (local.set $num_len (call $itoa (global.get $counter) (i64.add (local.get $out_ptr) (local.get $pos))))
    (local.set $pos (i64.add (local.get $pos) (local.get $num_len)))

    ;; Write suffix (5 bytes): "}]}}
    (call $memcpy (i64.add (local.get $out_ptr) (local.get $pos)) (i64.const 80) (i64.const 5))
    (local.set $pos (i64.add (local.get $pos) (i64.const 5)))

    ;; Null terminate
    (i32.store8 (i64.add (local.get $out_ptr) (local.get $pos)) (i32.const 0))

    ;; Return pointer to response
    (local.get $out_ptr)
  )
)
