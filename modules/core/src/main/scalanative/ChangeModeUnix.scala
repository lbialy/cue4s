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

import scala.scalanative.unsafe.*

trait ChangeModeUnix extends ChangeModeNative:
  val STDIN_FILENO = 0

  // @blocking: the keyboard thread spends most of its time in read(2); without
  // it, every stop-the-world GC in a multithreaded program waits for read to
  // return (up to VTIME = 0.5s), stalling other threads.
  @extern @blocking def read(fd: Int, buf: Ptr[Byte], count: Int): Int = extern

  /** One byte of input as 0..255, or 0 when nothing was read (VTIME timeout,
    * EINTR from SIGWINCH). The raw byte must be masked: returning a signed byte
    * makes every non-ASCII (UTF-8) byte negative, which the keyboard thread
    * treats as end of input and stops reading.
    */
  override def read(): Int =
    val b = stackalloc[Byte](1)
    if read(STDIN_FILENO, b, 1) == 1 then b(0) & 0xff else 0

  protected inline def assertAndReturn(b: Boolean, msg: String) =
    assert(b, msg)
    b

end ChangeModeUnix

object ChangeModeUnix:
  object Termios:
    @extern()
    def tcgetattr(fd: Int, state: Ptr[Byte]): Int = extern

    @extern()
    def tcsetattr(fd: Int, action: Int, state: Ptr[Byte]): Int = extern
  end Termios
end ChangeModeUnix
