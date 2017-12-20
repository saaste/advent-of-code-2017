package models

case class Particle(id: Int,
                    private var pos: Vector3D,
                    private var vel: Vector3D,
                    private val acc: Vector3D) {

  def position: Vector3D = pos
  def acceleration: Vector3D = acc

  def accelerate(): Unit = vel.add(acc)
  def move(): Unit = pos.add(vel)

}
