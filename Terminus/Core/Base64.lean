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
  let n := bytes.size
  -- Pre-allocate array: ceil(n/3) * 4 characters
  let outSize := ((n + 2) / 3) * 4
  let mut out : Array Char := Array.mkEmpty outSize
  let mut i : Nat := 0

  while i < n do
    let b0 := bytes.get! i |>.toNat
    let b1 := if i + 1 < n then bytes.get! (i + 1) |>.toNat else 0
    let b2 := if i + 2 < n then bytes.get! (i + 2) |>.toNat else 0

    let triple := (b0 <<< 16) ||| (b1 <<< 8) ||| b2

    let c0 := sextetChar ((triple >>> 18) &&& 0x3F)
    let c1 := sextetChar ((triple >>> 12) &&& 0x3F)
    let c2 := sextetChar ((triple >>> 6) &&& 0x3F)
    let c3 := sextetChar (triple &&& 0x3F)

    out := out.push c0
    out := out.push c1
    out := out.push (if i + 1 < n then c2 else '=')
    out := out.push (if i + 2 < n then c3 else '=')

    i := i + 3

  String.ofList out.toList

/-- Encode a string as base64 -/
def encodeString (s : String) : String :=
  encode s.toUTF8

/-- Decode base64 character to 6-bit value, returns none for invalid chars -/
private def charToSextet (c : Char) : Option Nat :=
  if c >= 'A' && c <= 'Z' then some (c.toNat - 'A'.toNat)
  else if c >= 'a' && c <= 'z' then some (c.toNat - 'a'.toNat + 26)
  else if c >= '0' && c <= '9' then some (c.toNat - '0'.toNat + 52)
  else if c == '+' then some 62
  else if c == '/' then some 63
  else if c == '=' then some 0  -- Padding, value doesn't matter
  else none

/-- Decode base64 string to bytes (RFC 4648). Returns none on invalid input. -/
def decode (s : String) : Option ByteArray := Id.run do
  let chars := s.toList.filter fun c => c != '\n' && c != '\r' && c != ' '  -- Strip whitespace
  if chars.length % 4 != 0 then return none

  let mut out : ByteArray := ByteArray.empty
  let mut i : Nat := 0

  while i < chars.length do
    let c0 := chars.getD i '='
    let c1 := chars.getD (i + 1) '='
    let c2 := chars.getD (i + 2) '='
    let c3 := chars.getD (i + 3) '='

    let some s0 := charToSextet c0 | return none
    let some s1 := charToSextet c1 | return none
    let some s2 := charToSextet c2 | return none
    let some s3 := charToSextet c3 | return none

    let triple := (s0 <<< 18) ||| (s1 <<< 12) ||| (s2 <<< 6) ||| s3

    out := out.push ((triple >>> 16) &&& 0xFF).toUInt8
    if c2 != '=' then out := out.push ((triple >>> 8) &&& 0xFF).toUInt8
    if c3 != '=' then out := out.push (triple &&& 0xFF).toUInt8

    i := i + 4

  some out

/-- Decode base64 string to a UTF-8 string. Returns none on invalid input. -/
def decodeString (s : String) : Option String :=
  match decode s with
  | some bytes => some (String.fromUTF8! bytes)
  | none => none

end Base64

end Terminus

