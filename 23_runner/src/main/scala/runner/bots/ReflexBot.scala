package runner.bots

import runner.*

/**
 * Детермінований рефлексивний бот (Етап 1).
 * Аналізує найближчий зріз простору 3x3 та ухвалює безпечне рішення.
 */
class ReflexBot(seed: Long = 202L) extends CyberBot:

  override def name: String = "ReflexBot"

  override def decide(observation: Observation): Action =
    val currentLane = observation.hero.lane

    observation.upcoming.headOption match
      case None =>
        Action.KeepRunning

      case Some(nextSlice) =>
        // Перевіряємо, чи можна вижити на поточній смузі без зміни смуги
        actionForLane(nextSlice, currentLane) match
          case Some(safeAction) =>
            safeAction

          case None =>
            // Поточна смуга заблокована шукаємо безпечну сусідню
            findSafeSideStep(nextSlice, currentLane)

  /**
   * Визначає безпечну дію в межах однієї смуги
   * Повертає None, якщо на смузі непрохідна комбінація перешкод
   */
  private def actionForLane(slice: TunnelSlice, lane: Lane): Option[Action] =
    val lowBlocked  = slice.hasObstacleAt(lane, Height.Low)
    val midBlocked  = slice.hasObstacleAt(lane, Height.Mid)
    val highBlocked = slice.hasObstacleAt(lane, Height.High)

    (lowBlocked, midBlocked, highBlocked) match
      // Смуга чиста або перешкода лише зверху  можна безпечно бігти
      case (false, false, false) => Some(Action.KeepRunning)
      case (false, false, true)  => Some(Action.KeepRunning)

      // Перешкода на рівні грудей пригинаємося
      case (false, true, _)      => Some(Action.Duck)

      // Перешкода знизу, але стеля вільна перестрибуємо
      case (true, _, false)      => Some(Action.Jump)

      // Комбо Low + High  або глуха стіна вижити на цій смузі неможливо
      case (true, _, true)       => None

  /**
   * Перевіряє сусідні смуги  для безпечного переходу вбік.
   */
  private def findSafeSideStep(slice: TunnelSlice, currentLane: Lane): Action =
    val canGoLeft  = currentLane.left.exists(l => isLaneEnterable(slice, l))
    val canGoRight = currentLane.right.exists(l => isLaneEnterable(slice, l))

    if canGoLeft then Action.MoveLeft
    else if canGoRight then Action.MoveRight
    else Action.KeepRunning // Захист від виходу за межі тунелю, якщо всі сусідні смуги заблоковані

  /**
   * При переході на сусідню смугу герой опиняється на ній у стані Running (займає Low та Mid),
   * тому для безпечного входу обидва ці рівні повинні бути вільними від перешкодgit status
   */
  private def isLaneEnterable(slice: TunnelSlice, lane: Lane): Boolean =
    !slice.hasObstacleAt(lane, Height.Low) && !slice.hasObstacleAt(lane, Height.Mid)