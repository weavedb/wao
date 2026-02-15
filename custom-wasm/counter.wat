(module
  ;; 1 page = 64KB of memory
  (memory (export "memory") 1)

  ;; Global: bump allocator pointer (starts at 4096 to leave room for data)
  (global $bump (mut i32) (i32.const 4096))
  ;; Global: counter value
  (global $counter (mut i32) (i32.const 0))

  ;; ===== Data section: pre-baked JSON response templates =====
  ;; Offset 0: response prefix '{"ok":true,"response":{"Output":{"data":"'
  (data (i32.const 0) "{\"ok\":true,\"response\":{\"Output\":{\"data\":\"")
  ;; Length = 42

  ;; Offset 64: response suffix '"},"Messages":[]}}'
  (data (i32.const 64) "\"},\"Messages\":[]}}")
  ;; Length = 18

  ;; ===== malloc: bump allocator =====
  (func $malloc (export "malloc") (param $size i32) (result i32)
    (local $ptr i32)
    (local.set $ptr (global.get $bump))
    (global.set $bump (i32.add (global.get $bump) (local.get $size)))
    (local.get $ptr)
  )

  ;; ===== free: no-op =====
  (func $free (export "free") (param $ptr i32) (result i32)
    (i32.const 0)
  )

  ;; ===== Helper: write a byte at position =====
  ;; (not exported, used internally)

  ;; ===== Helper: decimal i32 to string =====
  ;; Writes decimal representation of $val at $dst, returns length
  (func $itoa (param $val i32) (param $dst i32) (result i32)
    (local $len i32)
    (local $tmp i32)
    (local $digits i32)
    (local $i i32)
    (local $d i32)

    ;; Handle 0
    (if (i32.eqz (local.get $val))
      (then
        (i32.store8 (local.get $dst) (i32.const 48)) ;; '0'
        (return (i32.const 1))
      )
    )

    ;; Count digits
    (local.set $tmp (local.get $val))
    (local.set $digits (i32.const 0))
    (block $done
      (loop $count
        (br_if $done (i32.eqz (local.get $tmp)))
        (local.set $tmp (i32.div_u (local.get $tmp) (i32.const 10)))
        (local.set $digits (i32.add (local.get $digits) (i32.const 1)))
        (br $count)
      )
    )

    ;; Write digits in reverse
    (local.set $len (local.get $digits))
    (local.set $tmp (local.get $val))
    (local.set $i (i32.sub (local.get $digits) (i32.const 1)))
    (block $done2
      (loop $write
        (br_if $done2 (i32.lt_s (local.get $i) (i32.const 0)))
        (local.set $d (i32.rem_u (local.get $tmp) (i32.const 10)))
        (i32.store8
          (i32.add (local.get $dst) (local.get $i))
          (i32.add (local.get $d) (i32.const 48)) ;; '0' + digit
        )
        (local.set $tmp (i32.div_u (local.get $tmp) (i32.const 10)))
        (local.set $i (i32.sub (local.get $i) (i32.const 1)))
        (br $write)
      )
    )
    (local.get $len)
  )

  ;; ===== Helper: copy N bytes from src to dst =====
  (func $memcpy (param $dst i32) (param $src i32) (param $len i32)
    (local $i i32)
    (local.set $i (i32.const 0))
    (block $done
      (loop $copy
        (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
        (i32.store8
          (i32.add (local.get $dst) (local.get $i))
          (i32.load8_u (i32.add (local.get $src) (local.get $i)))
        )
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $copy)
      )
    )
  )

  ;; ===== Helper: search for substring in memory =====
  ;; Returns offset or -1 if not found
  ;; Searches for null-terminated $needle in $haystack (up to $max_len bytes)
  (func $strstr (param $haystack i32) (param $max_len i32) (param $needle i32) (result i32)
    (local $i i32)
    (local $j i32)
    (local $nlen i32)
    (local $match i32)
    (local $hc i32)
    (local $nc i32)

    ;; Get needle length
    (local.set $nlen (i32.const 0))
    (block $nend
      (loop $ncount
        (br_if $nend (i32.eqz (i32.load8_u (i32.add (local.get $needle) (local.get $nlen)))))
        (local.set $nlen (i32.add (local.get $nlen) (i32.const 1)))
        (br $ncount)
      )
    )

    ;; Search
    (local.set $i (i32.const 0))
    (block $notfound
      (loop $search
        (br_if $notfound (i32.gt_u (i32.add (local.get $i) (local.get $nlen)) (local.get $max_len)))
        ;; Check if haystack[i..i+nlen] == needle
        (local.set $j (i32.const 0))
        (local.set $match (i32.const 1))
        (block $nomatch
          (loop $cmp
            (br_if $nomatch (i32.ge_u (local.get $j) (local.get $nlen)))
            (local.set $hc (i32.load8_u (i32.add (i32.add (local.get $haystack) (local.get $i)) (local.get $j))))
            (local.set $nc (i32.load8_u (i32.add (local.get $needle) (local.get $j))))
            (if (i32.ne (local.get $hc) (local.get $nc))
              (then
                (local.set $match (i32.const 0))
                (br $nomatch)
              )
            )
            (local.set $j (i32.add (local.get $j) (i32.const 1)))
            (br $cmp)
          )
        )
        (if (local.get $match) (then (return (local.get $i))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $search)
      )
    )
    (i32.const -1)
  )

  ;; ===== Helper: strlen (null-terminated) =====
  (func $strlen (param $ptr i32) (result i32)
    (local $len i32)
    (local.set $len (i32.const 0))
    (block $done
      (loop $count
        (br_if $done (i32.eqz (i32.load8_u (i32.add (local.get $ptr) (local.get $len)))))
        (local.set $len (i32.add (local.get $len) (i32.const 1)))
        (br $count)
      )
    )
    (local.get $len)
  )

  ;; ===== Search patterns stored in data section =====
  ;; "Inc" pattern at offset 128
  (data (i32.const 128) "\"name\":\"Action\",\"value\":\"Inc\"")
  ;; Length = 29
  ;; "Get" pattern at offset 192
  (data (i32.const 192) "\"name\":\"Action\",\"value\":\"Get\"")
  ;; Length = 29

  ;; ===== handle: main entry point =====
  ;; Called by json-iface@1.0 with pointers to JSON strings
  (func $handle (export "handle") (param $msg_ptr i32) (param $proc_ptr i32) (result i32)
    (local $msg_len i32)
    (local $out_ptr i32)
    (local $pos i32)
    (local $num_len i32)
    (local $found i32)

    ;; Get message length
    (local.set $msg_len (call $strlen (local.get $msg_ptr)))

    ;; Check for "Inc" action
    (local.set $found (call $strstr (local.get $msg_ptr) (local.get $msg_len) (i32.const 128)))
    (if (i32.ne (local.get $found) (i32.const -1))
      (then
        ;; Increment counter
        (global.set $counter (i32.add (global.get $counter) (i32.const 1)))
      )
    )

    ;; Check for "Get" action (only if not Inc)
    (if (i32.eq (local.get $found) (i32.const -1))
      (then
        (local.set $found (call $strstr (local.get $msg_ptr) (local.get $msg_len) (i32.const 192)))
      )
    )

    ;; Allocate output buffer (256 bytes should be enough)
    (local.set $out_ptr (call $malloc (i32.const 256)))
    (local.set $pos (i32.const 0))

    ;; Write prefix: {"ok":true,"response":{"Output":{"data":"
    (call $memcpy (local.get $out_ptr) (i32.const 0) (i32.const 42))
    (local.set $pos (i32.const 42))

    ;; Write counter value as the data
    (local.set $num_len (call $itoa (global.get $counter) (i32.add (local.get $out_ptr) (local.get $pos))))
    (local.set $pos (i32.add (local.get $pos) (local.get $num_len)))

    ;; Write suffix: "},"Messages":[]}}
    (call $memcpy (i32.add (local.get $out_ptr) (local.get $pos)) (i32.const 64) (i32.const 18))
    (local.set $pos (i32.add (local.get $pos) (i32.const 18)))

    ;; Null terminate
    (i32.store8 (i32.add (local.get $out_ptr) (local.get $pos)) (i32.const 0))

    ;; Return pointer to response
    (local.get $out_ptr)
  )
)
