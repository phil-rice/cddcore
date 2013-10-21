package org.cddcore.example

import org.cddcore.engine.Engine
import org.junit.runner.RunWith
import org.cddcore.engine.tests._

case class TrafficLight(red: Boolean = false, orange: Boolean = false, green: Boolean = false)
@RunWith(classOf[CddJunitRunner])
object DecideAction {

  //This isn't complex enough to really need use cases
  val decide = Engine[TrafficLight, String]().
    useCase("Cars need to obey traffic signals").
      scenario(TrafficLight(red = true)).
        expected("Stop").

      scenario(TrafficLight(red = true, orange = true)).
        expected("Stop").

      scenario(TrafficLight(green = true)).
        because((l: TrafficLight) => l.green).
        expected("Go").

      scenario(TrafficLight(orange = true)).
        because((l: TrafficLight) => l.orange & !l.red).
        expected("Stop").

    build
}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               