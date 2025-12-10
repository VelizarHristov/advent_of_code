package year_2025

import io.Source

import optimus.optimization
import optimus.optimization.MPModel
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.MPIntVar
import optimus.algebra.{Const, Expression}

import ch.qos.logback.classic.{Level, Logger}
import org.slf4j.LoggerFactory
import org.slf4j.Logger.ROOT_LOGGER_NAME

@main
def day10_2(): Unit =
  // silence logging from Optimus
  LoggerFactory.getLogger(ROOT_LOGGER_NAME).asInstanceOf[Logger].setLevel(Level.WARN)

  val res = Source.fromFile("resources/2025/10").getLines.toArray.map(line => {
    val sections = line.split(' ')
    val req = sections.last.tail.init.split(',').map(_.toInt)
    val btns = sections.tail.init.map(_.tail.init.split(',').map(_.toInt)).map: btn =>
      req.indices.map: i =>
        if (btn.contains(i)) 1 else 0

    given problem: MPModel = MPModel(SolverLib.oJSolver)
    val x = Array.tabulate(btns.size)(i => MPIntVar(s"x$i", 0 to 1000))
    for dim <- req.indices do
      val dimSumExpr = btns.indices.foldLeft(Const(0): Expression): (sum, idx) =>
        sum + (Const(btns(idx)(dim)) * x(idx))
      optimization.add(dimSumExpr := Const(req(dim)))
    optimization.minimize(x.reduce(_ + _))
    optimization.start()
    optimization.release()
    problem.objectiveValue
  }).sum.toInt
  println(res)
