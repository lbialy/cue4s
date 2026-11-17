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

private[cue4s] trait PlatformStd extends Output:
  // frames are collected here and written to stdout in one go
  private val frame      = new java.lang.StringBuilder
  private var frameDepth = 0

  override def logLn[A: AsString](a: => A): Unit =
    System.err.println(a.render)
    System.err.flush()

  override def out[A: AsString](a: A): Unit =
    val s = a.render
    this.synchronized:
      if frameDepth > 0 then frame.append(s)
      else
        System.out.print(s)
        System.out.flush()

  override def beginFrame(): Unit =
    this.synchronized:
      frameDepth += 1

  override def endFrame(): Unit =
    this.synchronized:
      frameDepth = (frameDepth - 1).max(0)
      if frameDepth == 0 && frame.length > 0 then
        System.out.print(frame.toString)
        System.out.flush()
        frame.setLength(0)
end PlatformStd
