package boid.ui

import java.awt.Color

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by markus on 01/11/2014.
 */
class UITest extends FlatSpec with Matchers {
  "A name" should "be parsed with or without color" in {
    import boid.twoD.UI2D.parseName
    val justName = "3fqkejrfimlsdjfimsdo#"
    parseName(justName) should be (justName, None)
    val justName2 = "Jules de chez Smith en face"
    parseName(justName2) should be (justName2, None)
    val nameAndColor = "#1250453fqkejrfimlsdjfimsdo"
    parseName(nameAndColor) should be ("3fqkejrfimlsdjfimsdo", Some(new Color(0x125045)))
    val nameAndColor2 = "#afde00poclipop"
    parseName(nameAndColor2) should be ("poclipop", Some(new Color(0xafde00)))
    val nameAnd2Colors = "#afde00#afde00poclipop"
    parseName(nameAnd2Colors) should be ("#afde00poclipop", Some(new Color(0xafde00)))
  }
}
