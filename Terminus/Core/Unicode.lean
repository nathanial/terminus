-- Terminus.Core.Unicode: Unicode character width calculation (wcwidth equivalent)

namespace Terminus.Unicode

/-- Check if a code point is in any of the given ranges -/
def inRanges (cp : Nat) (ranges : List (Nat × Nat)) : Bool :=
  ranges.any fun (lo, hi) => cp >= lo && cp <= hi

/-- Zero-width character ranges (combining marks, control characters, etc.) -/
def zeroWidthRanges : List (Nat × Nat) := [
  -- Combining Diacritical Marks
  (0x0300, 0x036F),
  -- Combining Diacritical Marks Extended
  (0x1AB0, 0x1AFF),
  -- Combining Diacritical Marks Supplement
  (0x1DC0, 0x1DFF),
  -- Combining Diacritical Marks for Symbols
  (0x20D0, 0x20FF),
  -- Combining Half Marks
  (0xFE20, 0xFE2F),
  -- Zero-width characters
  (0x200B, 0x200F),  -- Zero-width space, joiners, directional marks
  (0x2028, 0x202F),  -- Line/paragraph separators, narrow spaces
  (0x2060, 0x206F),  -- Word joiner, invisible operators
  (0xFEFF, 0xFEFF),  -- Byte order mark / zero-width no-break space
  -- Variation selectors
  (0xFE00, 0xFE0F),
  (0xE0100, 0xE01EF),
  -- Tags
  (0xE0000, 0xE007F),
  -- Hangul Jamo medial/final (combining)
  (0x1160, 0x11FF),
  (0xD7B0, 0xD7FF),
  -- Arabic combining marks
  (0x0610, 0x061A),
  (0x064B, 0x065F),
  (0x0670, 0x0670),
  (0x06D6, 0x06DC),
  (0x06DF, 0x06E4),
  (0x06E7, 0x06E8),
  (0x06EA, 0x06ED),
  -- Hebrew combining marks
  (0x0591, 0x05BD),
  (0x05BF, 0x05BF),
  (0x05C1, 0x05C2),
  (0x05C4, 0x05C5),
  (0x05C7, 0x05C7),
  -- Thai combining marks
  (0x0E31, 0x0E31),
  (0x0E34, 0x0E3A),
  (0x0E47, 0x0E4E),
  -- Devanagari combining marks
  (0x0900, 0x0903),
  (0x093A, 0x094F),
  (0x0951, 0x0957),
  (0x0962, 0x0963),
  -- Other Indic scripts combining marks
  (0x0981, 0x0983),
  (0x09BC, 0x09BC),
  (0x09BE, 0x09CD),
  (0x0A01, 0x0A03),
  (0x0A3C, 0x0A51),
  (0x0A70, 0x0A71),
  (0x0A75, 0x0A75)
]

/-- Wide character ranges (CJK, emoji, etc.) that take 2 terminal cells -/
def wideRanges : List (Nat × Nat) := [
  -- Hangul Jamo (initial consonants)
  (0x1100, 0x115F),
  -- CJK Radicals Supplement
  (0x2E80, 0x2EFF),
  -- Kangxi Radicals
  (0x2F00, 0x2FDF),
  -- CJK Symbols and Punctuation
  (0x3000, 0x303F),
  -- Hiragana
  (0x3040, 0x309F),
  -- Katakana
  (0x30A0, 0x30FF),
  -- Bopomofo
  (0x3100, 0x312F),
  -- Hangul Compatibility Jamo
  (0x3130, 0x318F),
  -- Kanbun
  (0x3190, 0x319F),
  -- Bopomofo Extended
  (0x31A0, 0x31BF),
  -- CJK Strokes
  (0x31C0, 0x31EF),
  -- Katakana Phonetic Extensions
  (0x31F0, 0x31FF),
  -- Enclosed CJK Letters and Months
  (0x3200, 0x32FF),
  -- CJK Compatibility
  (0x3300, 0x33FF),
  -- CJK Unified Ideographs Extension A
  (0x3400, 0x4DBF),
  -- CJK Unified Ideographs
  (0x4E00, 0x9FFF),
  -- Yi Syllables
  (0xA000, 0xA48F),
  -- Yi Radicals
  (0xA490, 0xA4CF),
  -- Hangul Syllables
  (0xAC00, 0xD7AF),
  -- CJK Compatibility Ideographs
  (0xF900, 0xFAFF),
  -- Vertical Forms
  (0xFE10, 0xFE1F),
  -- CJK Compatibility Forms
  (0xFE30, 0xFE4F),
  -- Halfwidth and Fullwidth Forms (fullwidth range)
  (0xFF00, 0xFF60),
  (0xFFE0, 0xFFE6),
  -- CJK Unified Ideographs Extension B
  (0x20000, 0x2A6DF),
  -- CJK Unified Ideographs Extension C
  (0x2A700, 0x2B73F),
  -- CJK Unified Ideographs Extension D
  (0x2B740, 0x2B81F),
  -- CJK Unified Ideographs Extension E
  (0x2B820, 0x2CEAF),
  -- CJK Unified Ideographs Extension F
  (0x2CEB0, 0x2EBEF),
  -- CJK Compatibility Ideographs Supplement
  (0x2F800, 0x2FA1F),
  -- CJK Unified Ideographs Extension G
  (0x30000, 0x3134F),
  -- Emoji (most common ranges)
  (0x1F300, 0x1F5FF),  -- Miscellaneous Symbols and Pictographs
  (0x1F600, 0x1F64F),  -- Emoticons
  (0x1F680, 0x1F6FF),  -- Transport and Map Symbols
  (0x1F700, 0x1F77F),  -- Alchemical Symbols
  (0x1F900, 0x1F9FF),  -- Supplemental Symbols and Pictographs
  (0x1FA00, 0x1FA6F),  -- Chess Symbols
  (0x1FA70, 0x1FAFF),  -- Symbols and Pictographs Extended-A
  -- Enclosed Alphanumeric Supplement (circled letters, etc.)
  (0x1F100, 0x1F1FF),
  -- Playing cards, Mahjong tiles, Domino tiles
  (0x1F000, 0x1F0FF)
]

/-- Get the display width of a Unicode character.
    Returns 0 for combining characters, 2 for wide characters (CJK, emoji), 1 otherwise. -/
def charDisplayWidth (c : Char) : Nat :=
  let cp := c.toNat
  -- Control characters (except tab which is handled elsewhere)
  if cp < 32 || (cp >= 0x7F && cp < 0xA0) then 0
  -- Check zero-width ranges
  else if inRanges cp zeroWidthRanges then 0
  -- Check wide character ranges
  else if inRanges cp wideRanges then 2
  -- Default width is 1
  else 1

/-- Get the display width of a string (sum of character display widths) -/
def stringDisplayWidth (s : String) : Nat :=
  s.toList.foldl (fun acc c => acc + charDisplayWidth c) 0

end Terminus.Unicode

-- Extension methods for Char and String
namespace Char

/-- Get the display width of a Unicode character.
    Returns 0 for combining characters, 2 for wide characters (CJK, emoji), 1 otherwise. -/
def displayWidth (c : Char) : Nat := Terminus.Unicode.charDisplayWidth c

end Char

namespace String

/-- Get the display width of a string (sum of character display widths) -/
def displayWidth (s : String) : Nat := Terminus.Unicode.stringDisplayWidth s

/-- Get the display width of a substring up to a character index -/
def displayWidthTo (s : String) (charIndex : Nat) : Nat :=
  (s.take charIndex).displayWidth

/-- Find the character index that corresponds to a display column -/
def charIndexAtColumn (s : String) (col : Nat) : Nat := Id.run do
  let mut displayCol := 0
  let mut charIdx := 0
  for c in s.toList do
    if displayCol >= col then
      return charIdx
    displayCol := displayCol + c.displayWidth
    charIdx := charIdx + 1
  charIdx

end String
