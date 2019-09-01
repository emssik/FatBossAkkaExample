package as.blog.FatBoss

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}

object Reporter {

  def props(): Props = Props(new Reporter)

  final case class BeginWorkNR()
  final case class ReportPartNR(done: Int, from: Int)
}

class Reporter extends Actor with ActorLogging {
  import Reporter._

  def receive = {
    case BeginWorkNR() =>
      log.info("FatBoss zaczyna służbę w aktorze: " + sender())

    case ReportPartNR(done, from) =>
      // nie chcemy raportować za często
      if (done % 1000 == 0)
        log.info(s"FatBoss raportuje wybudowanie $done warsztatów z planowanych $from.")
  }
}

object FatBoss {

  // a co odpalimy sobie 2 mln aktorów, ktoś nam zabroni?
  val SOFT_LIMIT = 2000000
  var workersLimit: Int = SOFT_LIMIT
  var jobDone: Int = 0

  def props(system: ActorSystem, reporter: ActorRef): Props = Props(new FatBoss(system, reporter))

  // NR od NoReply, czyli po wykonaniu zadania nie jest oczekiwana odpowiedź
  final case class AskForWorkersNR(count: Int)
  final case class WorkCompletedNR(count: Int)
  final case class MakeFirstWorkshopNR()
}

class FatBoss(system: ActorSystem, reporter: ActorRef) extends Actor {

  import FatBoss._
  import Workshop._

  def updateWorkToDo(count: Int): Int = {
    // aktualizujemy pozostałą ilość pracowników
    workersLimit -= count
    count
  }

  def calculateWorkers(count: Int): Int =
    // wyznaczam ilość robotników do wysłania
    // wybieramy namniejszą wartość z 3 podanych
    // czyli albo ilość robotników o których proszono,
    // albo 100 bo Boss to świnia i nie daje więcej niż 100 pracowników
    // ale jak chce mniej to się nie wychyla i daje mniej
    // na końcu kontrolnie jeszcze lista pozostałych pracowników
    updateWorkToDo(List(count, 100, workersLimit).min)

  def receive = {
    case AskForWorkersNR(count: Int) =>
      calculateWorkers(count) match {
        case c: Int if c > 0 =>
          sender() ! StartWorkNR(c)

        case _ => ()
      }

    case WorkCompletedNR(count: Int) =>
      jobDone += count
      reporter ! Reporter.ReportPartNR(jobDone, SOFT_LIMIT)

    case MakeFirstWorkshopNR() =>
      system.actorOf(Workshop.props(system, self))
      reporter ! Reporter.BeginWorkNR()
  }
}

object Workshop {
  val rng = scala.util.Random

  def props(system: ActorSystem, fatBoss: ActorRef): Props = Props(new Workshop(system, fatBoss))

  final case class StartWorkNR(count: Int)
}

class Workshop(system: ActorSystem, fatBoss: ActorRef) extends Actor {

  fatBoss ! FatBoss.AskForWorkersNR(Workshop.rng.nextInt(200))

  def receive = {
    case Workshop.StartWorkNR(count: Int) =>
      // każdy uzyskany robotnik ma za zadanie wybudować
      // nowy warsztat
      for (i <- 1 to count) system.actorOf(Workshop.props(system, fatBoss))
      fatBoss ! FatBoss.WorkCompletedNR(count)
  }
}

object AkkaFatBoss extends App {
  val system: ActorSystem = ActorSystem("fatBossAkka")

  val reporter = system.actorOf(Reporter.props(), "reporter-actor")
  val fatBoss = system.actorOf(FatBoss.props(system, reporter), "fatboss-actor")
  fatBoss ! FatBoss.MakeFirstWorkshopNR()
}
