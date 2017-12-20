import helpers.{ResultPrinter, Timer}
import models.{Particle, Vector3D}

import scala.io.Source

object Day_20 extends App {

  private val input = Source.fromResource("day-20.txt").getLines().toArray

  val result1 = Timer.time {
    val particles = parseParticles(input)
    test1(particles)
  }

  val result2 = Timer.time {
    val particles = parseParticles(input)
    test2(particles)
  }

  ResultPrinter.printResult(result1, result2)


  def test1(particles: Array[Particle]): Int = {
    particles.minBy(p => p.acceleration.distance).id
  }

  def test2(particles: Array[Particle]): Int = {
    var particlesLeft = particles.clone()

    while (collidesPossible(particlesLeft)) {
      val collides = particlesLeft.groupBy(_.position).filter(_._2.length > 1).flatMap(_._2).toArray
      particlesLeft = particlesLeft.filterNot(p => collides.contains(p))

      particlesLeft.foreach { p =>
        p.accelerate()
        p.move()
      }
    }

    particlesLeft.length
  }


  private def collidesPossible(particles: Array[Particle]): Boolean = {
    for (i <- particles.indices) {
      val current = particles(i)
      val rest = particles.splitAt(i)._2.filter(p => current.acceleration.isSameDirection(p.acceleration))
      for (j <- rest.indices) {
        val p = rest(j)
        if (p.acceleration > current.acceleration &&
          p.position < current.position) return true
      }
    }

    false
  }

  private def parseParticles(input: Array[String]): Array[Particle] = {
    input.zipWithIndex.map { case (line, index) =>
      val values = line.replace("p=<", "").replace(" v=<", "").replace(" a=<", "").replace(">", "").split(',').map(_.trim.toInt)
      val position = Vector3D(values.head, values(1), values(2))
      val velocity = Vector3D(values(3), values(4), values(5))
      val acceleration = Vector3D(values(6), values(7), values(8))
      Particle(index, position, velocity, acceleration)
    }
  }

}
