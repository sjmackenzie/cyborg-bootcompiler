// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.

// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.


package org.mozartoz.bootcompiler
package util

import java.io.PrintStream

/** Provides implicit conversions for writing code */
object Output {
  implicit def string2modformat(self: String) = new {
    def % (args: Any*) =
      self format (args:_*)
  }
}

/** Simple wrapper around a [[java.io.PrintStream]] for writing code
 *
 *  @constructor creates a new wrapper for a [[java.io.PrintStream]]
 *  @param underlying underlying [[java.io.PrintStream]]
 */
class Output(val underlying: PrintStream) {
  import Output._

  /** Prints a value on the underlying stream */
  def print(x: Any) = underlying.print(x)

  /** Prints a value and a line feed on the underlying stream */
  def println(x: Any) = underlying.println(x)

  /** Prints a line feed on the underlying stream */
  def println() = underlying.println()

  /** Prints a value on the underlying stream
   *
   *  This operator can be chained, as C++ streams.
   */
  def << (x: Any): this.type = {
    print(x)
    this
  }
}
