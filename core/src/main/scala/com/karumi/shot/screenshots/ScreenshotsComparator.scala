package com.karumi.shot.screenshots

import java.io.File

import com.karumi.shot.domain._
import com.karumi.shot.domain.model.ScreenshotsSuite
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel

import java.util.concurrent.ForkJoinPool
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ForkJoinTaskSupport

class ScreenshotsComparator {
  //limit the number of threads
  val numberOfThreads = Math.min(2, Runtime.getRuntime.availableProcessors())
  private val taskSupport = new ForkJoinTaskSupport(new ForkJoinPool(numberOfThreads))

  def compare(
   screenshots: ScreenshotsSuite,
   tolerance: Double,
   colorTolerance: Int
 ): ScreenshotsComparisionResult = {
    println(
      Console.GREEN + s"Comparing screenshots using $numberOfThreads max threads" + Console.RESET
    )
    val screenshotsParallel = screenshots.par
    screenshotsParallel.tasksupport = taskSupport

    val errors =
      screenshotsParallel.flatMap(compareScreenshot(_, tolerance, colorTolerance)).toList
    ScreenshotsComparisionResult(errors, screenshots)
  }

  private def compareScreenshot(
      screenshot: Screenshot,
      tolerance: Double,
      colorTolerance: Int
  ): Option[ScreenshotComparisonError] = {
    val recordedScreenshotFile = new File(screenshot.recordedScreenshotPath)
    if (!recordedScreenshotFile.exists()) {
      Some(ScreenshotNotFound(screenshot))
    } else {
      val oldScreenshot =
        ImmutableImage.loader().fromFile(recordedScreenshotFile)
      val newScreenshot = ScreenshotComposer.composeNewScreenshot(screenshot)
      if (!haveSameDimensions(newScreenshot, oldScreenshot)) {
        val originalDimension =
          Dimension(oldScreenshot.width, oldScreenshot.height)
        val newDimension = Dimension(newScreenshot.width, newScreenshot.height)
        Some(DifferentImageDimensions(screenshot, originalDimension, newDimension))
      } else if (
        imagesAreDifferent(screenshot, oldScreenshot, newScreenshot, tolerance, colorTolerance)
      ) {
        Some(DifferentScreenshots(screenshot))
      } else {
        None
      }
    }
  }

  private def imagesAreDifferent(
      screenshot: Screenshot,
      oldScreenshot: ImmutableImage,
      newScreenshot: ImmutableImage,
      tolerance: Double,
      colorTolerance: Int
  ) = {
    if (oldScreenshot == newScreenshot) {
      false
    } else {
      val oldScreenshotPixels = oldScreenshot.pixels
      val newScreenshotPixels = newScreenshot.pixels

      val differentPixels =
        oldScreenshotPixels
          .zip(newScreenshotPixels)
          .filter { case (a, b) => !isRGBSimilar(a, b, colorTolerance) }

      val percentageOfDifferentPixels =
        differentPixels.length.toDouble / oldScreenshotPixels.length.toDouble
      val percentageOutOf100        = percentageOfDifferentPixels * 100.0
      val imagesAreDifferent        = percentageOutOf100 > tolerance
      val imagesAreConsideredEquals = !imagesAreDifferent
      val screenshotName = screenshot.name

      if(imagesAreDifferent) {
        println(
          Console.RED + s"We detected a failure in the screenshot named $screenshotName and the percentage of different pixels is $percentageOutOf100 %" + Console.RESET
        )
      }

      if (imagesAreConsideredEquals && tolerance != Config.defaultTolerance) {
        println(
          Console.YELLOW + s"⚠️   Shot warning: There are some pixels changed in the screenshot named $screenshotName, but we consider the comparison correct because tolerance is configured to $tolerance % and the percentage of different pixels is $percentageOutOf100 %" + Console.RESET
        )
      }
      imagesAreDifferent
    }
  }

  private def isRGBSimilar(p1: Pixel, p2: Pixel, colorTolerance: Int): Boolean = {
    val absDiffRed   = Math.abs(p1.red - p2.red)
    val absDiffGreen = Math.abs(p1.green - p2.green)
    val absDiffBlue  = Math.abs(p1.blue - p2.blue)
    absDiffRed <= colorTolerance && absDiffGreen <= colorTolerance && absDiffBlue <= colorTolerance
  }

  private def haveSameDimensions(
      newScreenshot: ImmutableImage,
      recordedScreenshot: ImmutableImage
  ): Boolean =
    newScreenshot.width == recordedScreenshot.width && newScreenshot.height == recordedScreenshot.height

}
