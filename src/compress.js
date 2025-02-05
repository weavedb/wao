function toBase256(n) {
  let result = []
  while (n > 0) {
    result.unshift(n % 256)
    n = Math.floor(n / 256)
  }
  return [result.length, ...result]
}

const _comp = (arr, _arr, mode) => {
  let _nums = null
  let _elms = null
  let len = _arr.length
  let flag = 0
  if (mode === 1 && _arr[0] !== 0) {
    flag += 100
    _elms = [_arr[0]]
  } else if (mode === 2) {
    flag += 200
    _elms = _arr
  }
  if (len < 10) flag += len
  else if (len !== 10) {
    const [elm, ...nums] = toBase256(len)
    _nums = nums
    flag += elm * 10
    if (_nums[0] < 10) flag += _nums.shift()
  }
  arr.push(flag)
  if (_nums) for (const n of _nums) arr.push(n)
  if (_elms) for (const n of _elms) arr.push(n)
}

export const compress = d => {
  let _arr = []
  let mode = 1
  let prev = null
  let max = 0
  let arr = []
  for (const v of d) {
    if (prev === null) _arr.push(v)
    else if (_arr.length === 1) {
      mode = prev !== v ? 2 : 1
      _arr.push(v)
    } else {
      if (prev !== v && mode === 1) {
        _comp(arr, _arr, mode)
        _arr = [v]
        mode = 2
      } else if (prev === v && mode === 2) {
        const last = _arr.pop()
        _comp(arr, _arr, mode)
        _arr = [last, v]
        mode = 1
      } else {
        _arr.push(v)
      }
    }
    prev = v
  }
  if (_arr.length > 0) _comp(arr, _arr, mode)
  return arr
}

export const decompress = d => {
  let arr = []
  let flag = null
  let elm = 0
  let sum = 0
  let nums = []
  let first = null
  let second = null
  let third = null
  for (let v of d) {
    if (flag === null) {
      flag = v
      first = Math.floor(flag / 100)
      second = Math.floor((flag - first * 100) / 10)
      third = flag - first * 100 - second * 10
      nums = []
      if (second === 0) {
        nums.push(third === 0 ? 10 : third)
        elm = 0
      } else if (third !== 0) {
        nums.push(third)
        elm = second - 1
      } else elm = second
    } else if (sum === 0) {
      elm -= 1
      nums.push(v)
    }
    if (sum > 0) {
      if (first === 2) arr.push(v)
      else if (first === 1) for (let i = 0; i < to10(nums); i++) arr.push(v)
      sum -= 1
      if (sum === 0) flag = null
    } else if (elm === 0) {
      if (first === 0) {
        for (let i = 0; i < to10(nums); i++) arr.push(0)
        flag = null
      } else if (first === 2) sum = to10(nums)
      else sum = 1
    }
  }
  return new Uint8Array(arr)
}

function to10(arr) {
  let num = 0
  for (let i = 0; i < arr.length; i++) num = num * 256 + arr[i]
  return num
}
