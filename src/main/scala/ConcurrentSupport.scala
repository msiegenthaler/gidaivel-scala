package ch.inventsoft.gidaivel

import ch.inventsoft.scalabase.process._
import Messages._

trait ConcurrentObjectSupport {
  object execute {
    object required {
    }

    object unlinked {
    }

    def inCaller[A](fun: => A): MessageSelector[A] @processCps = {
      val token = RequestToken.create[A]
      token.reply(fun)
      token.select
    }
    def inCaller_cps[A](fun: => A @processCps): MessageSelector[A] @processCps = {
      val token = RequestToken.create[A]
      token.reply(fun)
      token.select
    }
  }
}
