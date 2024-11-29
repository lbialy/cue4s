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

private[cue4s] class InteractiveSingleChoice(
    prompt: Prompt.SingleChoice,
    terminal: Terminal,
    out: Output,
    theme: Theme,
    windowSize: Int,
) extends PromptFramework[String](terminal, out):
  import InteractiveSingleChoice.*

  override type PromptState = State

  override def isRunning(state: State): Boolean = state.status == Status.Running

  private lazy val altsWithIndex = prompt.alts.zipWithIndex
  private lazy val altMapping    = altsWithIndex.map(_.swap).toMap

  override def result(state: State): Either[PromptError, String] =
    state.display.showing
      .map(_._2)
      .map(altMapping)
      .toRight(PromptError("nothing selected"))

  override def initialState = State(
    text = "",
    all = altsWithIndex,
    status = Status.Running,
    display = InfiniscrollableState(
      showing = Some(altsWithIndex.map(_._2) -> 0),
      windowStart = 0,
      windowSize = windowSize,
    ),
  )

  override def handleEvent(event: Event) =
    event match
      case Event.Init => PromptAction.Start

      case Event.Key(KeyEvent.UP) =>
        PromptAction.Update(_.updateDisplay(_.up))

      case Event.Key(KeyEvent.DOWN) =>
        PromptAction.Update(_.updateDisplay(_.down))

      case Event.Key(KeyEvent.ENTER) => // enter
        PromptAction.Submit(result => state => state.finish(result))

      case Event.Key(KeyEvent.DELETE) => // enter
        PromptAction.Update(_.trimText)

      case Event.Char(which) =>
        PromptAction.Update(_.addText(which.toChar))

      case Event.Interrupt =>
        PromptAction.UpdateAndStop(_.cancel)

      case _ =>
        PromptAction.Continue
    end match
  end handleEvent

  import theme.*

  override def renderState(
      st: State,
      error: Option[PromptError],
  ): List[String] =
    val lines = List.newBuilder[String]

    st.status match
      case Status.Running =>
        // prompt question
        lines += "· " + (prompt.lab + " > ").prompt + st.text.input

        st.display.showing match
          case None =>
            lines += "no matches...".noMatches
          case Some((filtered, selected)) =>
            // Render only the visible window
            st.display
              .visibleEntries(filtered)
              .zipWithIndex
              .foreach:
                case (id, idx) =>
                  val alt = altMapping(id)
                  lines.addOne(
                    if id == selected then s"  ‣ $alt".selected
                    else if st.display.windowStart > 0 && idx == 0 then
                      s"  ↑ $alt".option
                    else if filtered.size > st.display.windowSize &&
                      idx == st.display.windowSize - 1 &&
                      filtered.indexOf(id) != filtered.size - 1
                    then s"  ↓ $alt".option
                    else s"    $alt".option,
                  )
        end match
      case Status.Finished(value) =>
        lines += "✔ ".selected +
          (prompt.lab + " ").prompt +
          value.emphasis
      case Status.Canceled =>
        lines += "× ".canceled +
          (prompt.lab + " ").prompt
    end match

    lines.result()
  end renderState
end InteractiveSingleChoice

private[cue4s] object InteractiveSingleChoice:
  enum Status:
    case Running
    case Finished(selected: String)
    case Canceled

  case class State(
      text: String,
      all: List[(String, Int)],
      status: Status,
      display: InfiniscrollableState,
  ):
    def updateDisplay(f: InfiniscrollableState => InfiniscrollableState) =
      copy(display = f(display))

    def finish(result: String) =
      copy(status = Status.Finished(result))

    def cancel = copy(status = Status.Canceled)

    def addText(t: Char) =
      val newText = text + t
      changeText(newText).updateDisplay(_.resetWindow())

    def trimText =
      changeText(text.dropRight(1)).updateDisplay(_.resetWindow())

    private def changeText(newText: String) =
      val newFiltered = all.filter((alt, _) =>
        newText.trim.isEmpty || alt
          .toLowerCase()
          .contains(newText.toLowerCase().trim()),
      )
      if newFiltered.nonEmpty then
        display.showing match
          case None =>
            val newShowing = newFiltered.headOption.map: (_, id) =>
              newFiltered.map(_._2) -> id

            updateDisplay(_.copy(showing = newShowing)).copy(text = newText)

          case Some((_, selected)) =>
            val newShowing = newFiltered.headOption.map: (_, id) =>
              val newSelected =
                newFiltered.find(_._2 == selected).map(_._2).getOrElse(id)
              newFiltered.map(_._2) -> newSelected

            updateDisplay(_.copy(showing = newShowing)).copy(text = newText)
      else updateDisplay(_.copy(showing = None)).copy(text = newText)
      end if
    end changeText
  end State
end InteractiveSingleChoice
