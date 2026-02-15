//! Custom WASM64 module for HyperBEAM wasm-64@1.0 device.
//!
//! `#![no_std]` build to avoid bulk-memory instructions (memory.copy)
//! that WAMR doesn't support.
//!
//! Implements a counter handler for json-iface@1.0:
//!   Inc — increment counter, return count as Message
//!   Get — return current count as Message
//!   *   — return "0" as Message

#![no_std]
#![no_main]

use core::panic::PanicInfo;

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

// Bump allocator
static mut BUMP: usize = 4096;
static mut COUNTER: i32 = 0;

#[no_mangle]
pub extern "C" fn malloc(size: usize) -> usize {
    unsafe {
        let ptr = BUMP;
        BUMP += size;
        ptr
    }
}

#[no_mangle]
pub extern "C" fn free(_ptr: usize) -> usize {
    0
}

// Manual byte-by-byte copy (avoids memory.copy)
unsafe fn copy_bytes(dst: *mut u8, src: *const u8, len: usize) {
    let mut i = 0;
    while i < len {
        *dst.add(i) = *src.add(i);
        i += 1;
    }
}

// Write a byte slice to allocated memory, null-terminate, return pointer
unsafe fn write_bytes(data: &[u8]) -> usize {
    let ptr = malloc(data.len() + 1);
    copy_bytes(ptr as *mut u8, data.as_ptr(), data.len());
    *((ptr as *mut u8).add(data.len())) = 0;
    ptr
}

// Convert i32 to decimal string bytes, write at dst, return length
unsafe fn itoa(val: i32, dst: *mut u8) -> usize {
    if val == 0 {
        *dst = b'0';
        return 1;
    }

    // Count digits
    let mut tmp = val;
    let mut digits = 0usize;
    while tmp > 0 {
        tmp /= 10;
        digits += 1;
    }

    // Write digits in reverse
    tmp = val;
    let mut i = digits;
    while i > 0 {
        i -= 1;
        *dst.add(i) = b'0' + (tmp % 10) as u8;
        tmp /= 10;
    }
    digits
}

// Check if haystack contains needle (byte slices)
fn contains(haystack: &[u8], needle: &[u8]) -> bool {
    if needle.len() > haystack.len() {
        return false;
    }
    let mut i = 0;
    while i + needle.len() <= haystack.len() {
        let mut j = 0;
        let mut matched = true;
        while j < needle.len() {
            if haystack[i + j] != needle[j] {
                matched = false;
                break;
            }
            j += 1;
        }
        if matched {
            return true;
        }
        i += 1;
    }
    false
}

// Get length of null-terminated C string
unsafe fn strlen(ptr: *const u8) -> usize {
    let mut len = 0;
    while *ptr.add(len) != 0 {
        len += 1;
    }
    len
}

// Response prefix: {"ok":true,"response":{"Output":{"data":""},"Messages":[{"Data":"
const PREFIX: &[u8] = b"{\"ok\":true,\"response\":{\"Output\":{\"data\":\"\"},\"Messages\":[{\"Data\":\"";
// Response suffix: "}]}}
const SUFFIX: &[u8] = b"\"}]}}";

// Action patterns
const INC_PAT: &[u8] = b"\"name\":\"Action\",\"value\":\"Inc\"";
const GET_PAT: &[u8] = b"\"name\":\"Action\",\"value\":\"Get\"";

#[no_mangle]
pub extern "C" fn handle(msg_ptr: usize, _proc_ptr: usize) -> usize {
    unsafe {
        let msg_len = strlen(msg_ptr as *const u8);
        let msg = core::slice::from_raw_parts(msg_ptr as *const u8, msg_len);

        // Check for Inc action
        if contains(msg, INC_PAT) {
            COUNTER += 1;
        }

        // Build response buffer
        let buf_ptr = malloc(256);
        let dst = buf_ptr as *mut u8;
        let mut pos = 0usize;

        // Write prefix
        copy_bytes(dst, PREFIX.as_ptr(), PREFIX.len());
        pos += PREFIX.len();

        // Write counter value
        let num_len = itoa(COUNTER, dst.add(pos));
        pos += num_len;

        // Write suffix
        copy_bytes(dst.add(pos), SUFFIX.as_ptr(), SUFFIX.len());
        pos += SUFFIX.len();

        // Null terminate
        *dst.add(pos) = 0;

        buf_ptr
    }
}
