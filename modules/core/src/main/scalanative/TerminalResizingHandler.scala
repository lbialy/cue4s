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

import scala.scalanative.libc.signal.*
import scala.scalanative.meta.LinktimeInfo
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.UShort

import types.*

private[cue4s] object types:
  opaque type WinSize = CStruct2[UShort, UShort]
  object WinSize:
    given Tag[WinSize] =
      Tag.materializeCStruct2Tag[UShort, UShort]
    extension (ws: WinSize)
      inline def row: CUnsignedShort = ws._1
      inline def col: CUnsignedShort = ws._2
    end extension
  end WinSize
end types

import types.*

@extern
private[cue4s] def scalanative_sigwinch(): Int = extern

@extern
private[cue4s] def scalanative_get_window_size(ws: Ptr[WinSize]): Int =
  extern

object TerminalResizingHandler:
  // The signal handler only flips this flag: running arbitrary Scala code
  // (allocation, rendering, monitors) inside a signal handler is unsafe and
  // crashes when the interrupted thread holds a lock the handler needs.
  // A watcher thread turns the flag into Resized events.
  @volatile private var pending = false

  private def measure(): TerminalEvent.Resized =
    val ws = stackalloc[WinSize]()
    scalanative_get_window_size(ws)
    TerminalEvent.Resized(
      (!ws).row.toInt.asInstanceOf[TerminalRows],
      (!ws).col.toInt.asInstanceOf[TerminalCols],
    )

  def use[A](send: TerminalEvent.Resized => Unit)(
      f: => A,
  ): A =
    // resizing is not implemented on windows (yet)
    if LinktimeInfo.isWindows then f
    else
      // Immediately after this handler is invoked, we send the first measurement of the terminal.
      // Users should receive `TerminalEvent.Init` and then `TerminalEvent.Resized`
      send(measure())
      @volatile var running = true
      val watcher = new Thread(
        () =>
          while running do
            if pending then
              pending = false
              send(measure())
            try Thread.sleep(25)
            catch case _: InterruptedException => ()
        ,
        "cue4s-resize-watcher",
      )
      watcher.setDaemon(true)
      try
        signal(
          scalanative_sigwinch(),
          CFuncPtr1.fromScalaFunction: _ =>
            pending = true,
        )
        watcher.start()
        f
      finally
        signal(scalanative_sigwinch(), SIG_DFL)
        running = false
        watcher.interrupt()
      end try
  end use
end TerminalResizingHandler
