package minionsgame.core

import scala.util.{Try,Success,Failure}
import RichImplicits._

/** TechLevel:
  * How far long the tech tree a side is for a given unit
  */
sealed trait TechLevel {
  override def toString() : String = this match {
    case TechLocked => "Locked"
    case TechUnlocked => "Unlocked"
    case TechAcquired => "Acquired"
  }
  def toUnicodeSymbol() : String = this match {
    case TechLocked => ""
    case TechUnlocked => "\u25AE" // unicode rectangle
    case TechAcquired => "\u25AE\u25AE"
  }
}
object TechLevel {
  def ofString(s:String): TechLevel = {
    s match {
      case "TechLocked" => TechLocked
      case "TechUnlocked" => TechUnlocked
      case "TechAcquired" => TechAcquired
      case "Locked" => TechLocked
      case "Unlocked" => TechUnlocked
      case "Acquired" => TechAcquired
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
sealed trait Tech {
  def shortDisplayName(pieceMap: Map[PieceName, PieceStats]): String = {
    this match {
      case PieceTech(pieceName) => pieceMap(pieceName).shortDisplayName
      case Copycat => "Food"
      case Metamagic => "Science"
      case TechSeller => "Production"
    }
  }
  def displayName(pieceMap: Map[PieceName, PieceStats]): String = {
    this match {
      case PieceTech(pieceName) => pieceMap(pieceName).displayName
      case Copycat => "Food"
      case Metamagic => "Science"
      case TechSeller => "Production"
    }
  }
}
case class PieceTech(pieceName:PieceName) extends Tech
case object Copycat extends Tech
case object Metamagic extends Tech
case object TechSeller extends Tech
// TODO SPECIAL TECHS
// 25) Thaumaturgy: Instead of putting a spell under the tech line, you may discard one spell per turn for dollars equal to the number of boards.
// 26) Metamagic: You can assign your spells to your Captains and to the tech line however you wish.

/** TechState:
  * State of a single tech.
  */
case class TechState(
  val shortDisplayName: String,
  val displayName: String,
  val tech: Tech,
  val techNumber: Option[Int],
  val level: SideArray[TechLevel],
  val startingLevelThisTurn: SideArray[TechLevel]
)

sealed trait GameAction
case class PerformTech(side: Side, techLineIdx: Int) extends GameAction
case class UndoTech(side: Side, techLineIdx: Int) extends GameAction
case class SetBoardDone(boardIdx: Int, done: Boolean) extends GameAction
case class ResignBoard(boardIdx: Int) extends GameAction
case class BuyExtraTechAndSpell(side: Side) extends GameAction
case class UnbuyExtraTechAndSpell(side: Side) extends GameAction
case class SellTech(side: Side) extends GameAction
case class UnsellTech(side: Side) extends GameAction
//server->client only
case class PayForReinforcement(side: Side, pieceName: PieceName) extends GameAction
case class UnpayForReinforcement(side: Side, pieceName: PieceName) extends GameAction
case class ChooseSpell(side: Side, spellId: SpellId, boardIdx: Int) extends GameAction
case class UnchooseSpell(side: Side, spellId: SpellId, boardIdx: Int) extends GameAction
case class AddWin(side: Side, boardIdx: Int) extends GameAction
case class AddUpcomingSpells(side: Side, spellIds: Array[SpellId]) extends GameAction
// case class AddToScienceQueueAction(pieceName: PieceName, selectedCityId: Int) extends GameAction

case object Game {
  def apply(
    numBoards: Int,
    targetNumWins: Int,
    startingSide: Side,
    startingSouls: SideArray[Int],
    extraTechCost: Int,
    extraSoulsPerTurn: SideArray[Int],
    techsAlwaysAcquired: Array[Tech],
    otherTechs: Array[TechState],
    pieceMap: Map[PieceName, PieceStats]
  ) = {
    val techStatesAlwaysAcquired = techsAlwaysAcquired.map { tech =>
      TechState(
        shortDisplayName = tech.shortDisplayName(pieceMap),
        displayName = tech.displayName(pieceMap),
        tech = tech,
        techNumber = None,
        level = SideArray.create(TechAcquired),
        startingLevelThisTurn = SideArray.create(TechAcquired),
      )
    }
    val techStatesLocked = otherTechs
    val piecesAlwaysAcquired: Map[PieceName,TechState] =
      techStatesAlwaysAcquired.flatMap { techState =>
        techState.tech match {
          case PieceTech(pieceName) => Some(pieceName -> techState)
          case Copycat => None
          case Metamagic => None
          case TechSeller => None
        }
      }.toMap
    val piecesAcquired = SideArray.create(piecesAlwaysAcquired)
    otherTechs.foreach { techState =>
      Side.foreach { side =>
        if(techState.level(side) == TechAcquired) {
            techState.tech match {
              case PieceTech(pieceName) =>
                piecesAcquired(side) = piecesAcquired(side) + (pieceName -> techState)
              case Copycat | TechSeller | Metamagic => ()
            }
        }
      }
    }

    val game = new Game(
      numBoards = numBoards,
      curSide = startingSide,
      turnNumber = 0,
      winner = None,
      souls = startingSouls.copy(),
      wins = SideArray.create(0),
      techLine = techStatesAlwaysAcquired ++ techStatesLocked,
      piecesAcquired = piecesAcquired,
      soldTechThisTurn = false,
      extraTechsAndSpellsThisTurn = 0,
      usedTechsThisTurn = 0,
      spellsToChoose = Vector(),
      spellsChosen = Set(),
      boardsWithSpells = Map(),
      upcomingSpells = SideArray.create(Vector()),
      targetNumWins = targetNumWins,
      extraTechCost = extraTechCost,
      extraSoulsPerTurn = extraSoulsPerTurn,
      pieceMap = pieceMap,
      isBoardDone = Array.fill(numBoards)(false),
      newTechsThisTurn = Vector()
    )
    game
  }
}

/** Game:
  * The "global" state which isn't local to a board.
  */
case class Game (
  val numBoards: Int,
  var curSide: Side,
  var turnNumber: Int,
  var winner: Option[Side],

  val souls: SideArray[Int],
  val wins: SideArray[Int],

  val techLine: Array[TechState],
  val piecesAcquired: SideArray[Map[PieceName,TechState]],
  var soldTechThisTurn: Boolean,
  var extraTechsAndSpellsThisTurn: Int,
  var usedTechsThisTurn: Int,

  var spellsToChoose: Vector[Int],
  var spellsChosen: Set[Int],
  var boardsWithSpells: Map[Int,Int],
  var upcomingSpells: SideArray[Vector[Int]],

  val targetNumWins: Int,
  val extraTechCost: Int,
  val extraSoulsPerTurn: SideArray[Int],
  val pieceMap: Map[PieceName,PieceStats],

  //Flags set when user indicates that the board is done. Server ends the turn when all boards have this set.
  val isBoardDone: Array[Boolean],

  //Just for display purposes
  var newTechsThisTurn: Vector[(Side,Tech)]
) {

  def addSouls(side: Side, amount: Int): Unit = {
    souls(side) = souls(side) + amount
  }
  def soulsThisRound(side: Side): Int = {
    if(side == curSide) {
      0
    } else {
      extraSoulsPerTurn(side)
    }
  }

  def techsThisTurn(): Int = {
    extraTechsAndSpellsThisTurn + (if(soldTechThisTurn) 0 else 1)
  }
  def startedWithTech(tech: Tech, side: Side) = {
    techLine.find({ state => state.tech == tech}) match {
      case None => false
      case Some(state) => state.startingLevelThisTurn(side) == TechAcquired
    }
  }

  def tryIsLegal(action: GameAction): Try[Unit] = {
    action match {
      case PayForReinforcement(side,pieceName) => tryCanPayForReinforcement(side,pieceName)
      case UnpayForReinforcement(side,pieceName) => tryCanUnpayForReinforcement(side,pieceName)
      case ChooseSpell(side,spellId,boardIdx) => tryCanChooseSpell(side,spellId,boardIdx)
      case UnchooseSpell(side,spellId,boardIdx) => tryCanUnchooseSpell(side,spellId,boardIdx)
      case BuyExtraTechAndSpell(side) => tryCanBuyExtraTechAndSpell(side)
      case UnbuyExtraTechAndSpell(side) => tryCanUnbuyExtraTechAndSpell(side)
      case SellTech(side) => tryCanSellTech(side)
      case UnsellTech(side) => tryCanUnsellTech(side)
      case AddWin(_,_) => Success(())
      case AddUpcomingSpells(_,_) => Success(())
      case PerformTech(side,techLineIdx) => tryCanPerformTech(side,techLineIdx)
      case UndoTech(side,techLineIdx) => tryCanUndoTech(side,techLineIdx)
      case SetBoardDone(boardIdx,done) => tryCanSetBoardDone(boardIdx,done)
      case ResignBoard(boardIdx) => tryCanResignBoard(boardIdx)
    }
  }

  def doAction(action: GameAction): Try[Unit] = {
    action match {
      case PayForReinforcement(side,pieceName) => payForReinforcement(side,pieceName)
      case UnpayForReinforcement(side,pieceName) => unpayForReinforcement(side,pieceName)
      case ChooseSpell(side,spellId,boardIdx) => chooseSpell(side,spellId,boardIdx)
      case UnchooseSpell(side,spellId,boardIdx) => unchooseSpell(side,spellId,boardIdx)
      case BuyExtraTechAndSpell(side) => buyExtraTechAndSpell(side)
      case UnbuyExtraTechAndSpell(side) => unbuyExtraTechAndSpell(side)
      case SellTech(side) => sellTech(side)
      case UnsellTech(side) => unsellTech(side)
      case AddWin(side,_) => { doAddWin(side); Success(()) }
      case AddUpcomingSpells(side,spellIds) => { doAddUpcomingSpells(side,spellIds); Success(()) }
      case PerformTech(side,techLineIdx) => performTech(side,techLineIdx)
      case UndoTech(side,techLineIdx) => undoTech(side,techLineIdx)
      case SetBoardDone(boardIdx,done) => setBoardDone(boardIdx,done)
      case ResignBoard(boardIdx) => resignBoard(boardIdx)
    }
  }

  private def clearAndDrawSpells(): Unit = {
    spellsToChoose = Vector()
    spellsChosen = Set()
    boardsWithSpells = Map()
    while(upcomingSpells(curSide).nonEmpty && spellsToChoose.length < numBoards+1) {
      val spellId = upcomingSpells(curSide)(0)
      upcomingSpells(curSide) = upcomingSpells(curSide).drop(1)
      spellsToChoose = spellsToChoose :+ spellId
    }
  }

  // Given an index into the spell choice row, return the spell chosen and which side it belongs to
  // (or None if it hasn't been drawn yet)
  def resolveSpellChoice(idx : Int, side : Option[Side]) : (Option[SpellId], Option[Side]) = {
    side match {
      case None => (None, None)
      case Some(ourSide) =>
        if(ourSide == curSide) {
          if(idx == -1) {
            (None, None) // Available spells label
          } else if(idx < spellsToChoose.length) {
            val spellId = spellsToChoose(idx)
            val side = if(spellsChosen.contains(spellId)) None else Some(ourSide)
            (Some(spellId), side)
          } else if(idx-spellsToChoose.length == 0) {
            (None, None) // Upcoming spells label
          } else if(idx-spellsToChoose.length-1 < upcomingSpells(ourSide).length) {
            (Some(upcomingSpells(ourSide)(idx-spellsToChoose.length-1)), None)
          } else {
            (None, None) // Too far to the right
          }
        } else {
          if(idx == -1) { // Upcoming spells label
            (None, None)
          } else if(idx < upcomingSpells(ourSide).length) {
            (Some(upcomingSpells(ourSide)(idx)), None)
          } else {
            (None, None) // Too far to the right
          }
        }
    }
  }

  //A hook for things that also need to happen at the start of the game before any turns for setup
  //rather than just on the ends of turns
  def startGame(): Unit = {
    //Clear and draw spells first turn
    clearAndDrawSpells()
  }

  def endTurn(): Unit = {
    curSide = curSide.opp
    turnNumber += 1
    usedTechsThisTurn = 0
    extraTechsAndSpellsThisTurn = 0
    soldTechThisTurn = false

    newTechsThisTurn = Vector()
    techLine.foreach { techState =>
      Side.foreach { side =>
        if(techState.startingLevelThisTurn(side) != TechAcquired && techState.level(side) == TechAcquired) {
          newTechsThisTurn = newTechsThisTurn :+ ((side,techState.tech))
        }
        techState.startingLevelThisTurn(side) = techState.level(side)
      }
    }

    for(i <- 0 until isBoardDone.length)
      isBoardDone(i) = false

    clearAndDrawSpells()
  }

  private def tryCanPayForReinforcement(side: Side, pieceName: PieceName): Try[Unit] = {
    if(side != curSide)
      Failure(new Exception("Currently the other team's turn"))
    else if(!pieceMap.contains(pieceName))
      Failure(new Exception("Trying to pay for reinforcement piece with unknown name: " + pieceName))
    else {
      piecesAcquired(side).get(pieceName) match {
        case None => Failure(new Exception("Piece tech not acquired yet: " + pieceName))
        case Some(techState) =>
          if(techState.startingLevelThisTurn(side) != TechAcquired)
            Failure(new Exception("Cannot buy pieces on the same turn as teching to them"))
          else {
            val stats = pieceMap(pieceName)
            if(souls(side) < stats.cost)
              Failure(new Exception("Not enough souls"))
            else
              Success(())
          }
      }
    }
  }

  private def payForReinforcement(side: Side, pieceName: PieceName): Try[Unit] = {
    tryCanPayForReinforcement(side,pieceName) match {
      case (err : Failure[Unit]) => err
      case (suc : Success[Unit]) =>
        val stats = pieceMap(pieceName)
        addSouls(side, -stats.cost)
        suc
    }
  }

  private def tryCanUnpayForReinforcement(side: Side, pieceName: PieceName): Try[Unit] = {
    if(side != curSide)
      Failure(new Exception("Currently the other team's turn"))
    else if(!pieceMap.contains(pieceName))
      Failure(new Exception("Trying to unpay for reinforcement piece with unknown name: " + pieceName))
    else
      Success(())
  }

  private def unpayForReinforcement(side: Side, pieceName: PieceName): Try[Unit] = {
    tryCanUnpayForReinforcement(side,pieceName) match {
      case (err : Failure[Unit]) => err
      case (suc : Success[Unit]) =>
        val stats = pieceMap(pieceName)
        addSouls(side, stats.cost)
        suc
    }
  }

  private def tryCanChooseSpell(side: Side, spellId: SpellId, boardIdx: Int): Try[Unit] = {
    if(side != curSide)
      Failure(new Exception("Currently the other team's turn"))
    else if(!spellsToChoose.contains(spellId))
      Failure(new Exception("Trying to choose unknown spell id"))
    else if(spellsChosen.contains(spellId))
      Failure(new Exception("Spell already chosen by a board"))
    else {
      if(boardsWithSpells.contains(boardIdx) && !startedWithTech(Metamagic, side))
        Failure(new Exception("This board has already chosen a spell this turn"))
      else if(spellsChosen.size == numBoards)
        Failure(new Exception("Cannot choose more than " + numBoards + " spells in one turn"))
      else
        Success(())
    }
  }

  private def chooseSpell(side: Side, spellId: SpellId, boardIdx: Int): Try[Unit] = {
    tryCanChooseSpell(side,spellId,boardIdx) match {
      case (err : Failure[Unit]) => err
      case (suc : Success[Unit]) =>
        spellsChosen = spellsChosen + spellId
        boardsWithSpells = boardsWithSpells + (boardIdx -> (1 + boardsWithSpells.getOrElse(boardIdx, 0)))
        suc
    }
  }

  private def tryCanUnchooseSpell(side: Side, spellId: SpellId, boardIdx: Int): Try[Unit] = {
    if(side != curSide)
      Failure(new Exception("Currently the other team's turn"))
    else if(!spellsToChoose.contains(spellId))
      Failure(new Exception("Trying to undo choosing unknown spell id"))
    else if(!spellsChosen.contains(spellId))
      Failure(new Exception("Cannot undo choosing an unchosen spell"))
    else if(!boardsWithSpells.contains(boardIdx))
      Failure(new Exception("Cannot undo - board has not chosen a spell"))
    else
      Success(())
  }

  private def unchooseSpell(side: Side, spellId: SpellId, boardIdx: Int): Try[Unit] = {
    tryCanUnchooseSpell(side,spellId,boardIdx) match {
      case (err : Failure[Unit]) => err
      case (suc : Success[Unit]) =>
        spellsChosen = spellsChosen - spellId
        val oldSpells = boardsWithSpells(boardIdx)
        if(oldSpells == 1)
          boardsWithSpells = boardsWithSpells - boardIdx
        else
          boardsWithSpells = boardsWithSpells + (boardIdx -> (oldSpells - 1))
        suc
    }
  }

  private def doAddWin(side: Side): Unit = {
    wins(side) = wins(side) + 1
    if(wins(side) >= targetNumWins) {
      winner = Some(side)
    }
  }

  private def doAddUpcomingSpells(side: Side, spellIds: Array[SpellId]): Unit = {
    upcomingSpells(side) = upcomingSpells(side) ++ spellIds
  }

  private def tryCanPerformTech(side: Side, techLineIdx: Int): Try[Unit] = {
    if(side != curSide)
      Failure(new Exception("Currently the other team's turn"))
    else if(usedTechsThisTurn >= techsThisTurn())
      Failure(new Exception("Must buy extra tech to tech additional times this turn"))
    else if(techLineIdx < 0 || techLineIdx >= techLine.length)
      Failure(new Exception("Invalid tech idx"))
    else if(techLineIdx > 0 && techLine(techLineIdx-1).level(side) == TechLocked)
      Failure(new Exception("Must unlock techs in order"))
    else {
      val techState = techLine(techLineIdx)
      (techState.level(side), techState.level(side.opp)) match {
        case (TechAcquired, _) => Failure(new Exception("Already own this tech"))
        case (TechUnlocked, TechAcquired) =>
          if(startedWithTech(Copycat, side) || techState.tech == Copycat)
            Success(())
          else
            Failure(new Exception("Cannot acquire tech owned by the opponent"))
        case (_, _) => Success(())
      }
    }
  }

  private def performTech(side: Side, techLineIdx: Int): Try[Unit] = {
    tryCanPerformTech(side,techLineIdx) match {
      case (err : Failure[Unit]) => err
      case (suc : Success[Unit]) =>
        usedTechsThisTurn += 1
        val techState = techLine(techLineIdx)
        techState.level(side) match {
          case TechAcquired => assertUnreachable()
          case TechLocked =>
            techState.level(side) = TechUnlocked
          case TechUnlocked =>
            techState.level(side) = TechAcquired
            techState.tech match {
              case PieceTech(pieceName) =>
                piecesAcquired(side) = piecesAcquired(side) + (pieceName -> techState)
              case Copycat | TechSeller | Metamagic => ()
            }
        }
        suc
    }
  }

  private def tryCanUndoTech(side: Side, techLineIdx: Int): Try[Unit] = {
    if(side != curSide)
      Failure(new Exception("Currently the other team's turn"))
    else if(techLineIdx < 0 || techLineIdx >= techLine.length)
      Failure(new Exception("Invalid tech idx"))
    else {
      val techState = techLine(techLineIdx)
      techState.level(side) match {
        case TechLocked => Failure(new Exception("Cannot undo tech never acquired"))
        case TechUnlocked | TechAcquired =>
          if(techLine(techLineIdx).level(side) == techLine(techLineIdx).startingLevelThisTurn(side))
            Failure(new Exception("Cannot undo tech from previous turns"))
          else if(techLineIdx < techLine.length - 1 &&
            techLine(techLineIdx).level(side) == TechUnlocked &&
            techLine(techLineIdx+1).level(side) != techLine(techLineIdx+1).startingLevelThisTurn(side))
            Failure(new Exception("Cannot undo this tech without undoing later techs first"))
          else
            Success(())
      }
    }
  }

  private def undoTech(side: Side, techLineIdx: Int): Try[Unit] = {
    tryCanUndoTech(side,techLineIdx) match {
      case (err : Failure[Unit]) => err
      case (suc : Success[Unit]) =>
        usedTechsThisTurn -= 1
        val techState = techLine(techLineIdx)
        techState.level(side) match {
          case TechAcquired =>
            techState.level(side) = TechUnlocked
            techState.tech match {
              case PieceTech(pieceName) =>
                piecesAcquired(side) = piecesAcquired(side) - pieceName
              case Copycat =>
                techLine.zipWithIndex.foreach { case (state,idx) =>
                  if(state.startingLevelThisTurn(side) != state.level(side) &&
                    state.level(side) == TechAcquired &&
                    state.level(side.opp) == TechAcquired) {
                      undoTech(side, idx)
                    }
                }
              case TechSeller => ()
              case Metamagic => ()
            }
          case TechUnlocked =>
            techState.level(side) = TechLocked
          case TechLocked =>
            assertUnreachable()
        }
        suc
    }
  }

  private def tryCanBuyExtraTechAndSpell(side: Side): Try[Unit] = {
    if(side != curSide)
      Failure(new Exception("Currently the other team's turn"))
    else if(souls(side) < extraTechCost)
      Failure(new Exception("Not enough souls"))
    else
      Success(())
  }

  private def buyExtraTechAndSpell(side: Side): Try[Unit] = {
    tryCanBuyExtraTechAndSpell(side) match {
      case (err : Failure[Unit]) => err
      case (suc : Success[Unit]) =>
        addSouls(side, -extraTechCost)
        extraTechsAndSpellsThisTurn += 1
        val spellId = upcomingSpells(curSide)(0)
        upcomingSpells(curSide) = upcomingSpells(curSide).drop(1)
        spellsToChoose = spellsToChoose :+ spellId
        suc
    }
  }

  private def tryCanUnbuyExtraTechAndSpell(side: Side): Try[Unit] = {
    if(side != curSide)
      Failure(new Exception("Currently the other team's turn"))
    else if(extraTechsAndSpellsThisTurn <= 0)
      Failure(new Exception("No extra techs and spells to undo this turn"))
    else if(spellsChosen.contains(spellsToChoose.last))
      Failure(new Exception("Undo not possible since extra spell was claimed, that board must first undo claiming that spell"))
    else if(usedTechsThisTurn >= techsThisTurn())
      Failure(new Exception("Undo not possible since extra tech was used, must first choose a tech to undo"))
    else
      Success(())
  }

  private def unbuyExtraTechAndSpell(side: Side): Try[Unit] = {
    tryCanUnbuyExtraTechAndSpell(side) match {
      case (err : Failure[Unit]) => err
      case (suc : Success[Unit]) =>
        addSouls(side, extraTechCost)
        extraTechsAndSpellsThisTurn -= 1
        val spellId = spellsToChoose.last
        upcomingSpells(curSide) = spellId +: upcomingSpells(curSide)
        spellsToChoose = spellsToChoose.dropRight(1)
        suc
    }
  }

  private def tryCanSellTech(side: Side): Try[Unit] = {
    if(side != curSide) {
      Failure(new Exception("Currently the other team's turn"))
    } else {
      if(!startedWithTech(TechSeller, side))
        Failure(new Exception("Cannot sell tech without buying thaumaturgy"))
      else if(soldTechThisTurn)
        Failure(new Exception("Already sold starting tech this turn"))
      else if(usedTechsThisTurn >= techsThisTurn())
        Failure(new Exception("Cannot sell tech; already used this turn"))
      else
        Success(())
    }
  }
  private def sellTech(side: Side): Try[Unit] = {
    tryCanSellTech(side) match {
      case (err : Failure[Unit]) => err
      case (suc : Success[Unit]) =>
        addSouls(side, numBoards)
        soldTechThisTurn = true
        suc
    }
  }
  private def tryCanUnsellTech(side: Side): Try[Unit] = {
    if(side != curSide)
      Failure(new Exception("Currently the other team's turn"))
    else if(souls(side) < numBoards)
      Failure(new Exception("Not enough souls to buy back starting tech"))
    else if(!soldTechThisTurn)
      Failure(new Exception("No tech sell to undo this turn"))
    else
      Success(())
  }
  private def unsellTech(side: Side): Try[Unit] = {
    tryCanUnsellTech(side) match {
      case (err : Failure[Unit]) => err
      case (suc : Success[Unit]) =>
        addSouls(side, -numBoards)
        soldTechThisTurn = false
        suc
    }
  }

  private def tryCanSetBoardDone(boardIdx: Int, done: Boolean): Try[Unit] = {
    val _ = done
    if(boardIdx < 0 || boardIdx >= isBoardDone.length)
      Failure(new Exception("Invalid board idx"))
    else
      Success(())
  }

  private def setBoardDone(boardIdx: Int, done: Boolean): Try[Unit] = {
    tryCanSetBoardDone(boardIdx,done) match {
      case (err : Failure[Unit]) => err
      case (suc : Success[Unit]) =>
        isBoardDone(boardIdx) = done
        suc
    }
  }

  private def tryCanResignBoard(boardIdx: Int): Try[Unit] = {
    if(boardIdx < 0 || boardIdx >= isBoardDone.length)
      Failure(new Exception("Invalid board idx"))
    else
      Success(())
  }

  private def resignBoard(boardIdx: Int): Try[Unit] = {
    tryCanResignBoard(boardIdx) match {
      case (err : Failure[Unit]) => err
      case (suc : Success[Unit]) =>
        doAddWin(curSide.opp)
        suc
    }
  }

  // private def tryCanAddToScienceQueue(pieceName: PieceName, selectedCityId: Int): Try[Unit] = {
  //   Success(())
  // }

  // private def addToScienceQueue(pieceName: PieceName, selectedCityId: Int): Try[Unit] = {
  //   val selectedCity = externalInfo.pieceById(selectedCityId);
  //   selectedCity.scienceQueue = selectedCity.scienceQueue ::: List(externalInfo.pieceMap(pieceName));
  // }
}
