/*
 * Copyright 2023 Neandertech
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cue4s

/** Visible width of rendered lines: ANSI escape sequences take no space, wide
  * (CJK, emoji) characters take two columns, combining marks none.
  */
object TextWidth:
  private final val ESC = '\u001b'
  private final val BEL = '\u0007'

  def of(s: String): Int =
    var w = 0
    var i = 0
    val n = s.length
    while i < n do
      val c = s.charAt(i)
      if c == ESC && i + 1 < n then
        s.charAt(i + 1) match
          case '[' => // CSI: parameters/intermediates, then a final byte 0x40-0x7e
            i += 2
            while i < n && (s.charAt(i) < 0x40 || s.charAt(i) > 0x7e) do i += 1
            i += 1
          case ']' => // OSC: terminated by BEL or ESC \
            i += 2
            while i < n && s.charAt(i) != BEL && !(s.charAt(
                i,
              ) == ESC && i + 1 < n && s.charAt(i + 1) == '\\')
            do i += 1
            i += (if i < n && s.charAt(i) == BEL then 1 else 2)
          case _ => i += 2 // two-character sequences like ESC 7
      else
        val cp = s.codePointAt(i)
        w += codePoint(cp)
        i += Character.charCount(cp)
      end if
    end while
    w
  end of

  /** Number of terminal rows a line occupies when wrapped at `cols`. */
  def rows(s: String, cols: Option[Int]): Int =
    cols match
      case Some(c) if c > 0 =>
        val w = of(s)
        if w == 0 then 1 else (w + c - 1) / c
      case _ => 1

  def codePoint(cp: Int): Int =
    if cp < 32 || (cp >= 0x7f && cp < 0xa0) then 0
    else if (cp >= 0x300 && cp <= 0x36f) || cp == 0x200b || cp == 0x200d ||
      (cp >= 0xfe00 && cp <= 0xfe0f)
    then 0
    else if (cp >= 0x1100 && cp <= 0x115f) || (cp >= 0x2e80 && cp <= 0xa4cf) ||
      (cp >= 0xac00 && cp <= 0xd7a3) || (cp >= 0xf900 && cp <= 0xfaff) ||
      (cp >= 0xfe30 && cp <= 0xfe4f) || (cp >= 0xff00 && cp <= 0xff60) ||
      (cp >= 0xffe0 && cp <= 0xffe6) || (cp >= 0x1f300 && cp <= 0x1faff) ||
      (cp >= 0x20000 && cp <= 0x3fffd)
    then 2
    else 1
end TextWidth
