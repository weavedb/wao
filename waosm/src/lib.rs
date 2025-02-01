use wasm_bindgen::prelude::*;
use js_sys::Uint8Array;

#[wasm_bindgen]
pub struct Compressor {}

#[wasm_bindgen]
impl Compressor {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {}
    }

    fn to_base_256(mut n: usize) -> Vec<u8> {
        let mut result = Vec::new();
        while n > 0 {
            result.insert(0, (n % 256) as u8);
            n /= 256;
        }
        if result.is_empty() {
            result.push(0);
        }
        let mut final_result = vec![result.len() as u8];
        final_result.extend(result);
        final_result
    }

    #[wasm_bindgen]
    pub fn compress(&self, data: &[u8]) -> Uint8Array {
        let mut arr = Vec::new();
        let mut _arr = Vec::new();
        let mut mode = 1;
        let mut prev: Option<u8> = None;

        for &v in data {
            if prev.is_none() {
                _arr.push(v);
            } else if _arr.len() == 1 {
                mode = if prev.unwrap() != v { 2 } else { 1 };
                _arr.push(v);
            } else {
                if prev.unwrap() != v && mode == 1 {
                    Self::_comp(&mut arr, &_arr, mode);
                    _arr.clear();
                    _arr.push(v);
                    mode = 2;
                } else if prev.unwrap() == v && mode == 2 {
                    let last = _arr.pop().unwrap();
                    Self::_comp(&mut arr, &_arr, mode);
                    _arr.clear();
                    _arr.extend_from_slice(&[last, v]);
                    mode = 1;
                } else {
                    _arr.push(v);
                }
            }
            prev = Some(v);
        }

        if !_arr.is_empty() {
            Self::_comp(&mut arr, &_arr, mode);
        }

        let result = Uint8Array::new_with_length(arr.len() as u32);
        result.copy_from(&arr);
        result
    }

    fn _comp(arr: &mut Vec<u8>, _arr: &[u8], mode: u8) {
        let mut _nums: Option<Vec<u8>> = None;
        let mut _elms: Option<Vec<u8>> = None;
        let len = _arr.len();
        let mut flag: u8 = 0;

        if mode == 1 && _arr[0] != 0 {
            flag += 100;
            _elms = Some(vec![_arr[0]]);
        } else if mode == 2 {
            flag += 200;
            _elms = Some(_arr.to_vec());
        }

        if len < 10 {
            flag += len as u8;
        } else if len != 10 {
            let base256 = Self::to_base_256(len);
            let (elm, nums_slice) = base256.split_at(1);
            _nums = Some(nums_slice.to_vec());
            flag += elm[0] * 10;
            if let Some(ref mut nums_vec) = _nums {
                if !nums_vec.is_empty() && nums_vec[0] < 10 {
                    flag += nums_vec.remove(0);
                }
            }
        }

        arr.push(flag);
        if let Some(nums_vec) = _nums {
            arr.extend(nums_vec);
        }
        if let Some(elms_vec) = _elms {
            arr.extend(elms_vec);
        }
    }
}

#[wasm_bindgen]
pub struct Decompressor {}

#[wasm_bindgen]
impl Decompressor {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {}
    }

    fn to_10(arr: &[u8]) -> usize {
        let mut num = 0;
        for &v in arr {
            num = num * 256 + v as usize;
        }
        num
    }

    #[wasm_bindgen]
    pub fn decompress(&self, data: &[u8]) -> Uint8Array {
        let mut arr = Vec::new();
        let mut flag: Option<u8> = None;
        let mut elm: i32 = 0;
        let mut sum: i32 = 0;
        let mut nums: Vec<u8> = Vec::new();
        let mut first: Option<u8> = None;
        let mut second: Option<u8> = None;
        let mut third: Option<u8> = None;

        for &v in data {
            if flag.is_none() {
                flag = Some(v);
                let flag = v as i32;
                first = Some((flag / 100) as u8);
                second = Some(((flag - (flag / 100) * 100) / 10) as u8);
                third = Some((flag - (flag / 100) * 100 - ((flag - (flag / 100) * 100) / 10) * 10) as u8);
                nums.clear();

                if second.unwrap() == 0 {
                    nums.push(if third.unwrap() == 0 { 10 } else { third.unwrap() });
                    elm = 0;
                } else if third.unwrap() != 0 {
                    nums.push(third.unwrap());
                    elm = second.unwrap() as i32 - 1;
                } else {
                    elm = second.unwrap() as i32;
                }
            } else if sum == 0 {
                elm -= 1;
                nums.push(v);
            }

            if sum > 0 {
                if first.unwrap() == 2 {
                    arr.push(v);
                } else if first.unwrap() == 1 {
                    let count = Self::to_10(&nums);
                    for _ in 0..count {
                        arr.push(v);
                    }
                }
                sum -= 1;
                if sum == 0 {
                    flag = None;
                }
            } else if elm == 0 {
                if first.unwrap() == 0 {
                    let count = Self::to_10(&nums);
                    for _ in 0..count {
                        arr.push(0);
                    }
                    flag = None;
                } else if first.unwrap() == 2 {
                    sum = Self::to_10(&nums) as i32;
                } else {
                    sum = 1;
                }
            }
        }

        let result = Uint8Array::new_with_length(arr.len() as u32);
        result.copy_from(&arr);
        result
    }
}
