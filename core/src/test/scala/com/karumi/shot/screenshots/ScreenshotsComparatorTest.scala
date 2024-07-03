package com.karumi.shot.screenshots

import com.karumi.shot.Resources
import com.karumi.shot.domain.{Dimension, Screenshot}
import com.karumi.shot.domain.model.ScreenshotsSuite
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ScreenshotsComparatorTest extends AnyFlatSpec with should.Matchers with Resources {

  private val sampleImage = getClass.getClassLoader.getResource("images/sample.png").getPath
  private val sampleImageScrambled =
    getClass.getClassLoader.getResource("images/sample-scrambled.png").getPath

  private val sampleX86   = getClass.getClassLoader.getResource("images/sample-x86.png").getPath
  private val sampleArm64 = getClass.getClassLoader.getResource("images/sample-arm64.png").getPath

  it should "not match image with same pixels but different order" in {
    val comparator = new ScreenshotsComparator()
    val screenshot = Screenshot(
      "test",
      sampleImage,
      sampleImageScrambled,
      "SomeClass",
      "ShoudFail",
      Dimension(768, 1280),
      null,
      null,
      null,
      List(sampleImageScrambled),
      Dimension(768, 1280)
    )
    val suite: ScreenshotsSuite = List(screenshot)

    val result = comparator.compare(suite, tolerance = 0.01, colorTolerance = 0)

    result.hasErrors shouldBe true
  }

  it should "not match image almost equal with colorTolerance = 0" in {
    val comparator = new ScreenshotsComparator()
    val screenshot = Screenshot(
      "test",
      sampleX86,
      sampleArm64,
      "SomeClass",
      "ShoudFail",
      Dimension(1080, 1878),
      null,
      null,
      null,
      List(sampleArm64),
      Dimension(1080, 1878)
    )
    val suite: ScreenshotsSuite = List(screenshot)

    val result = comparator.compare(suite, tolerance = 0.01, colorTolerance = 0)

    result.hasErrors shouldBe true

  }

  it should "match image almost equal using colorTolerance" in {
    val comparator = new ScreenshotsComparator()
    val screenshot = Screenshot(
      "test",
      sampleX86,
      sampleArm64,
      "SomeClass",
      "ShoudFail",
      Dimension(1080, 1878),
      null,
      null,
      null,
      List(sampleArm64),
      Dimension(1080, 1878)
    )
    val suite: ScreenshotsSuite = List(screenshot)

    val result = comparator.compare(suite, tolerance = 0.01, colorTolerance = 2)

    result.hasErrors shouldBe false

  }
}
