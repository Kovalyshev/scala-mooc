package homeworks.futures

import homeworks.HomeworksUtils.TaskSyntax

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Failure, Try}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    val transformed: List[Future[Either[A, Throwable]]] = futures.map(f => f.transform {
      case Failure(ex) =>
        Try(Right(ex))
      case Success(v) =>
        Try(Left(v))
    })
    for {
      fut <- Future.sequence(transformed)
      s <- Future(fut.filter(_.isLeft).flatMap(t => t.left.toOption))
      f <- Future(fut.filter(_.isRight).flatMap(t => t.toOption))
    } yield (s, f)
  }
}
