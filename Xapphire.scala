import javax.swing.JFrame
import java.lang.Math._
import javax.swing.JPanel
import java.awt.Image
import java.awt.image.BufferedImage
import java.awt.Graphics
import java.awt.Dimension
import java.awt.Color

case class Vector(val x : Double, val y : Double, val z : Double) {

	def this() = this(0d, 0d, 0d)
	def -() = Vector(-x, -y, -z)
	def +(v : Vector) = Vector(x+v.x, y+v.y, z+v.z)
	def -(v : Vector) = Vector(x-v.x, y-v.y, z-v.z)
	def ^(v : Vector) = Vector(y * v.z - z * v.y, -x * v.z + z * v.x, x * v.y - y * v.x)
	def *(v : Vector) = x*v.x + y*v.y + z*v.z
	def *(s : Double) = Vector(x*s, y*s, z*s)
	def /(s : Double) = Vector(x/s, y/s, z/s)
	def magnitude() = sqrt(x * x + y * y + z * z)
	def normalize() = {
		val magnitudeValue = magnitude
		if (magnitudeValue > 0.0)
			(this / magnitudeValue)
		else
			(this)
	}
}

object World {
	val sphere = Vector(0.0d, 0.0d, -10.0d)
	val radius = 2.5d
	var light = sphere + Vector(10.0d, 10.0d, 10.0d)
	val lightBrightness = 1d
	val ambient = 0.2d
}

class Ray(originV : Vector, directionV : Vector) {
	val origin = originV
	val direction = directionV normalize
	
	def trace() = {

		val dist = World.sphere - origin
		val B = direction * dist
		val D = B*B - dist * dist + World.radius * World.radius
		if (D < 0.0d) 
			Vector(0.2d, 0.5d, 0.9d)
		else {
			val t0 = B - sqrt(D)
			val t1 = B + sqrt(D)
			var retvalue = false
			var t = 1000000.0d
			if ((t0 > 0.001d) && (t0 < t)) 
			{
				t = t0
				retvalue = true
			} 
			if ((t1 > 0.001d) && (t1 < t)) 
			{
				t = t1
				retvalue = true
			}
			if(retvalue)
				lambert(t)
			else
				Vector(0.2d, 0.5d, 0.9d)
		}
	}
	
	def lambert(t : Double) = {
	
		val point = origin + direction * t
		val normal = (point - World.sphere).normalize()
		val toLight = (World.light - point).normalize()

		val cosine = normal * toLight

		val brightness = if(cosine >= 0.0)
				cosine * World.lightBrightness / (toLight*toLight) + World.ambient
			else
				World.ambient

		val percieved = 1.0 - exp(-brightness)
		
		Vector(percieved, percieved, percieved)
	}
}

object XImage extends JPanel {

	var image = new BufferedImage(640, 480, BufferedImage.TYPE_INT_RGB)
		
	override def paint(g : Graphics) : Unit = {
		g.drawImage(image, 0, 0, this)
	}

	def clip(brightness : Double) = {
		if(brightness > 1.0)
			1.0f
		else if(brightness < 0.0)
			0.0f
		else
			brightness toFloat
	}

	def paint(g2d : Graphics, color : Vector, x : Integer, y : Integer) = {
		g2d.setColor(new Color(clip(color.x), clip(color.y), clip(color.z)))
		g2d.drawLine(x, y, x, y)

	}
}

val frame = new JFrame("Xapphire")
frame setSize(640, 480)
frame setLocationRelativeTo(null)
frame setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
frame add (XImage)
frame setVisible(true)

var counter = 0.0d
var frames = 0.0d
val startTime = System currentTimeMillis

while(true) {
	val g2d = XImage.image.createGraphics()
	for(row <- 0 to 480) {
		for(column <- 0 to 640) {
		
			val x = column / 320.0d - 1.0d
			val y = row / 240.0d - 1.0d
		
			val ray = new Ray(Vector(0.0d, 0.0d, 1.0d), Vector(x, y, -1.0d))
			XImage paint(g2d, ray trace, column, row)
		}
	}
	g2d.dispose()
	XImage.repaint()
	counter += 0.1d
	World.light = World.sphere - Vector(-10.0d*cos(counter) + 10.0d*sin(counter), 5.0d,
		-10.0d*sin(counter) - 10.0d*cos(counter))

	frames += 1.0d
	frame.setTitle("" + (frames*1000 / (System.currentTimeMillis() - startTime)) + " FPS")
}

