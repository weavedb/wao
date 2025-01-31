use wasm_bindgen::prelude::*;
use js_sys::Uint8Array;

#[wasm_bindgen]
pub struct Compressor {
    arr: Vec<u8>,
    arr2: Vec<u8>,
    mode: u8,
    prev: Option<u8>,
    elm_arr: Vec<u8>,
}

#[wasm_bindgen]
impl Compressor {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {
            arr: Vec::new(),
            arr2: Vec::new(),
            mode: 1,
            prev: None,
            elm_arr: Vec::new(),
        }
    }

    fn to_base_256(n: usize) -> Vec<u8> {
        let mut result = Vec::new();
        let mut n = n;
        while n > 0 {
            result.insert(0, (n % 256) as u8);
            n /= 256;
        }
        let mut final_result = vec![result.len() as u8];
        final_result.extend(result);
        final_result
    }

    fn comp(&mut self) {
        let mut nums: Option<Vec<u8>> = None;
        let mut elms: Option<Vec<u8>> = None;
        let len = self.elm_arr.len();
        let mut flag: u8 = 0;

        if self.mode == 1 && self.elm_arr[0] != 0 {
            flag += 100;
            elms = Some(vec![self.elm_arr[0]]);
        } else if self.mode == 2 {
            flag += 200;
            elms = Some(self.elm_arr.clone());
        }

        if len < 10 {
            flag += len as u8;
        } else if len != 10 {
            let base256 = Self::to_base_256(len);
            let (elm, nums_slice) = base256.split_at(1);
            nums = Some(nums_slice.to_vec());
            flag += elm[0] * 10;
            if let Some(ref nums_vec) = nums {
                if !nums_vec.is_empty() && nums_vec[0] < 10 {
                    flag += nums_vec[0];
                    nums = Some(nums_vec[1..].to_vec());
                }
            }
        }

        self.arr2.push(flag);
        if let Some(nums_vec) = nums {
            self.arr2.extend(nums_vec);
        }
        if let Some(elms_vec) = elms {
            self.arr2.extend(elms_vec);
        }
    }

    #[wasm_bindgen]
    pub fn compress(&mut self, data: &[u8]) -> Uint8Array {
        self.arr2.clear();
        self.elm_arr.clear();
        self.mode = 1;
        self.prev = None;

        for &v in data {
            if self.prev.is_none() {
                self.elm_arr.push(v);
            } else if self.elm_arr.len() == 1 {
                self.mode = if self.prev.unwrap() != v { 2 } else { 1 };
                self.elm_arr.push(v);
            } else {
                if self.prev.unwrap() != v && self.mode == 1 {
                    self.comp();
                    self.elm_arr.clear();
                    self.elm_arr.push(v);
                    self.mode = 2;
                } else if self.prev.unwrap() == v && self.mode == 2 {
                    let last = self.elm_arr.pop().unwrap();
                    self.comp();
                    self.elm_arr.clear();
                    self.elm_arr.extend_from_slice(&[last, v]);
                    self.mode = 1;
                } else {
                    self.elm_arr.push(v);
                }
            }
            self.prev = Some(v);
        }

        if !self.elm_arr.is_empty() {
            self.mode = if self.elm_arr.len() == 1 { 2 } else { 1 };
            self.comp();
        }

        unsafe {
            Uint8Array::view(&self.arr2)
        }
    }
}
#[wasm_bindgen]
pub struct Decompressor {
    arr: Vec<u8>,
    nums: Vec<u8>,
    flag: Option<u8>,
    elm: i32,
    sum: i32,
    first: Option<u8>,
    second: Option<u8>,
    third: Option<u8>,
}

#[wasm_bindgen]
impl Decompressor {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {
            arr: Vec::with_capacity(1024),
            nums: Vec::new(),
            flag: None,
            elm: 0,
            sum: 0,
            first: None,
            second: None,
            third: None,
        }
    }

    fn to_10(arr: &[u8]) -> usize {
        let mut num: usize = 0;
        for &v in arr {
            num = num * 256 + v as usize;
        }
        num
    }

    #[wasm_bindgen]
    pub fn decompress(&mut self, data: &[u8]) -> Uint8Array {
        self.arr.clear();
        self.nums.clear();
        self.flag = None;
        self.elm = 0;
        self.sum = 0;

        for &v in data {
            if self.flag.is_none() {
                self.flag = Some(v);
                let flag = v as i32;
                self.first = Some((flag / 100) as u8);
                self.second = Some(((flag - (flag / 100) * 100) / 10) as u8);
                self.third = Some((flag - (flag / 100) * 100 - ((flag - (flag / 100) * 100) / 10) * 10) as u8);
                self.nums.clear();

                if self.second.unwrap() == 0 {
                    self.nums.push(if self.third.unwrap() == 0 { 10 } else { self.third.unwrap() });
                    self.elm = 0;
                } else if self.third.unwrap() != 0 {
                    self.nums.push(self.third.unwrap());
                    self.elm = self.second.unwrap() as i32 - 1;
                } else {
                    self.elm = self.second.unwrap() as i32;
                }
            } else if self.sum == 0 {
                self.elm -= 1;
                self.nums.push(v);
            }

            if self.sum > 0 {
                if self.first.unwrap() == 2 {
                    self.arr.push(v);
                } else if self.first.unwrap() == 1 {
                    let count = Self::to_10(&self.nums);
                    for _ in 0..count {
                        self.arr.push(v);
                    }
                }
                self.sum -= 1;
                if self.sum == 0 {
                    self.flag = None;
                }
            } else if self.elm == 0 {
                if self.first.unwrap() == 0 {
                    let count = Self::to_10(&self.nums);
                    for _ in 0..count {
                        self.arr.push(0);
                    }
                    self.flag = None;
                } else if self.first.unwrap() == 2 {
                    self.sum = Self::to_10(&self.nums) as i32;
                } else {
                    self.sum = 1;
                }
            }
        }

        unsafe {
            Uint8Array::view(&self.arr)
        }
    }
}
