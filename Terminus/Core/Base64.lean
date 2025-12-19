-- Terminus.Core.Base64: Minimal base64 encoding for binary payloads

namespace Terminus

namespace Base64

private def alphabet : Array Char :=
  #[
    'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P',
    'Q','R','S','T','U','V','W','X','Y','Z',
    'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p',
    'q','r','s','t','u','v','w','x','y','z',
    '0','1','2','3','4','5','6','7','8','9',
    '+','/'
  ]

private def sextetChar (n : Nat) : Char :=
  alphabet.getD n '?'

/-- Encode bytes as base64 (RFC 4648, with `=` padding). -/
def encode (bytes : ByteArray) : String := Id.run do
  let mut out : List Char := []
  let mut i : Nat := 0
  let n := bytes.size

  while i < n do
    let b0 := bytes.get! i |>.toNat
    let b1 := if i + 1 < n then bytes.get! (i + 1) |>.toNat else 0
    let b2 := if i + 2 < n then bytes.get! (i + 2) |>.toNat else 0

    let triple := (b0 <<< 16) ||| (b1 <<< 8) ||| b2

    let c0 := sextetChar ((triple >>> 18) &&& 0x3F)
    let c1 := sextetChar ((triple >>> 12) &&& 0x3F)
    let c2 := sextetChar ((triple >>> 6) &&& 0x3F)
    let c3 := sextetChar (triple &&& 0x3F)

    out := out ++ [c0, c1]
    if i + 1 < n then out := out ++ [c2] else out := out ++ ['=']
    if i + 2 < n then out := out ++ [c3] else out := out ++ ['=']

    i := i + 3

  String.ofList out

end Base64

end Terminus

