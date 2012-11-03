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

import java.io.File

import scala.util.parsing.input._

trait FilePosition extends Position {
  def file: Option[File]

  def fileName = file map (_.getName) getOrElse FilePosition.NoFileName
}

object FilePosition {
  val NoFileName = "<unknown-file>"

  def fileOf(position: Position) = position match {
    case filePos: FilePosition => filePos.file
    case _ => None
  }

  def fileNameOf(position: Position, default: String = NoFileName) =
    fileOf(position) map (_.getName) getOrElse default
}
