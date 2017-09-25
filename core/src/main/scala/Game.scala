package minionsgame.core

import scala.util.{Try,Success,Failure}
import RichImplicits._

/** TechLevel:
  * How far long the tech tree a side is for a given unit
  */
sealed trait TechLevel {
  override def toString() : String = this match {
    case TechLocked => "TechLocked"
    case TechUnlocked => "TechUnlocked"
    case TechAcquired => "TechAcquired"
  }
}
object TechLevel {
  def ofString(s:String): TechLevel = {
    s match {
      case "TechLocked" => TechLocked
      case "TechUnlocked" => TechUnlocked
      case "TechAcquired" => TechAcquired
      case _ => throw new Exception("Could not parse TechLevel: " + s)
    }
  }
}
case object TechLocked extends TechLevel
case object TechUnlocked extends TechLevel
case object TechAcquired extends TechLevel


/** Tech:
  * Element in the tech sequence.
  */
sealed trait Tech
case class PieceTech(pieceName:PieceName) extends Tech

/** TechState:
  * State of a single tech.
  */
case class TechState(
  val tech: Tech,
  val level: SideArray[TechLevel]
)

sealed trait GameAction
case class PayForReinforcement(side: Side, pieceName: PieceName) extends GameAction
case class PerformTech(side: Side, techLineIdx: Int) extends GameAction

case object Game {
  def apply(
    startingSide: Side,
    startingMana: SideArray[Int],
    extraTechCost: Int,
    techsAlwaysAcquired: Array[Tech],
    lockedTechs: Array[Tech]
  ) = {
    val game = new Game(
      curSide = startingSide,
      turnNumber = 0,
      mana = startingMana.copy(),
      techLine = (techsAlwaysAcquired ++ lockedTechs).map { tech =>
        TechState(tech,SideArray.ofArrayInplace(Array(TechLocked,TechLocked)))
      },
      piecesAcquired = SideArray.create(Set()),
      hasTechedThisTurn = false,
      extraTechCost = extraTechCost
    )
    for(techLineIdx <- 0 until techsAlwaysAcquired.length) {
      game.performTechWithoutCost(S0,techLineIdx)
      game.performTechWithoutCost(S0,techLineIdx)
      game.performTechWithoutCost(S1,techLineIdx)
      game.performTechWithoutCost(S1,techLineIdx)
    }
    game
  }
}

/** Game:
  * The "global" state which isn't local to a board.
  */
case class Game (
  var curSide: Side,
  var turnNumber: Int,

  val mana: SideArray[Int],

  val techLine: Array[TechState],
  val piecesAcquired: SideArray[Set[PieceName]],
  var hasTechedThisTurn: Boolean,
  val extraTechCost: Int

  // TODO(jpaulson): Spells for the moving side
) {

  def addMana(side: Side, amount: Int): Unit = {
    mana(side) = mana(side) + amount
  }

  def tryIsLegal(action: GameAction): Try[Unit] = {
    action match {
      case PayForReinforcement(side,pieceName) => tryCanPayForReinforcement(side,pieceName)
      case PerformTech(side,techLineIdx) => tryCanPerformTech(side,techLineIdx)
    }
  }

  def doAction(action: GameAction): Try[Unit] = {
    action match {
      case PayForReinforcement(side,pieceName) => payForReinforcement(side,pieceName)
      case PerformTech(side,techLineIdx) => performTech(side,techLineIdx)
    }
  }

  def endTurn(): Unit = {
    curSide = curSide.opp
    turnNumber += 1
    hasTechedThisTurn = false
  }

  private def tryCanPayForReinforcement(side: Side, pieceName: PieceName): Try[Unit] = {
    val stats = Units.pieceMap(pieceName)
    if(side != curSide)
      Failure(new Exception("Currently the other team's turn"))
    else if(!piecesAcquired(side).contains(pieceName))
      Failure(new Exception("Piece tech not acquired yet"))
    else if(mana(side) < stats.cost)
      Failure(new Exception("Not enough souls"))
    else
      Success(())
  }

  private def payForReinforcement(side: Side, pieceName: PieceName): Try[Unit] = {
    tryCanPayForReinforcement(side,pieceName) match {
      case (err : Failure[Unit]) => err
      case (suc : Success[Unit]) =>
        val stats = Units.pieceMap(pieceName)
        mana(side) = mana(side) - stats.cost
        suc
    }
  }

  private def tryCanPerformTech(side: Side, techLineIdx: Int): Try[Unit] = {
    if(side != curSide)
      Failure(new Exception("Currently the other team's turn"))
    else if(hasTechedThisTurn && mana(side) < extraTechCost)
      Failure(new Exception("Not enough souls to tech more than once this turn"))
    else if(techLineIdx < 0 || techLineIdx >= techLine.length)
      Failure(new Exception("Invalid tech idx"))
    else if(techLineIdx > 0 && techLine(techLineIdx-1).level(side) == TechLocked)
      Failure(new Exception("Must unlock techs in order"))
    else {
      val techState = techLine(techLineIdx)
      techState.level(side.opp) match {
        case TechAcquired => Failure(new Exception("Cannot acquire tech owned by the opponent"))
        case TechUnlocked | TechLocked =>
          techState.level(side) match {
            case TechAcquired => Failure(new Exception("Already own this tech"))
            case TechLocked | TechUnlocked =>
              Success(())
          }
      }
    }
  }

  private def performTech(side: Side, techLineIdx: Int): Try[Unit] = {
    tryCanPerformTech(side,techLineIdx) match {
      case (err : Failure[Unit]) => err
      case (suc : Success[Unit]) =>
        if(hasTechedThisTurn)
          mana(side) = mana(side) - extraTechCost
        else
          hasTechedThisTurn = true

        performTechWithoutCost(side,techLineIdx)
        suc
    }
  }

  private def performTechWithoutCost(side: Side, techLineIdx: Int): Unit = {
    val techState = techLine(techLineIdx)
    techState.level(side) match {
      case TechAcquired => assertUnreachable()
      case TechLocked =>
        techState.level(side) = TechUnlocked
      case TechUnlocked =>
        techState.level(side) = TechAcquired
        techState.tech match {
          case PieceTech(pieceName) =>
            piecesAcquired(side) = piecesAcquired(side) + pieceName
        }
    }
  }


}
