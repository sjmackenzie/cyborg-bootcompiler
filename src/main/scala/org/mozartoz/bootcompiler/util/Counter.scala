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

/** Simple counter for generating unique IDs
 *
 *  @constructor creates a new counter with a given initial value
 *  @param start first value that will be returned by `next()`
 */
class Counter(start: Int = 1) {
  private var _last = start-1

  /** Returns the next generated ID.
   *
   *  The first time it is called it returns `start`. Subsequent calls return
   *  monotonically increasing values, if wrapping is not an issue.
   */
  def next() = {
    _last += 1
    _last
  }
}
