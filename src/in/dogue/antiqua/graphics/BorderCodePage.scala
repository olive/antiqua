package in.dogue.antiqua.graphics

import in.dogue.antiqua.data.Code

case class BorderCodePage(
  /**  | */
  vertical:Code,

  /**  - */
  horizontal:Code,

  /** _
   * |   */
  upLeft:Code,

  /** _
   *   |  */
  upRight:Code,

  /**  |_  */
  downLeft:Code,

  /**  _|  */
  downRight:Code
)
