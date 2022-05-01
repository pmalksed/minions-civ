package minionsgame.jsclient

import scala.util.{Try, Success, Failure}

import minionsgame.core._
import RichImplicits._

//A thing on the board that could possibly mouse-overed, clicked, on, selected, etc.
sealed trait MouseTarget {
  def findPiece(board: BoardState): Option[Piece] = {
    this match {
      case MousePiece(spec,_) => board.findPiece(spec)
      case _ => None
    }
  }
  def getLoc(): Option[Loc] = {
    this match {
      case MouseNone => None
      case MouseSpellChoice(_,_,loc) => Some(loc)
      case MouseSpellHand(_,_,loc) => Some(loc)
      case MouseSpellPlayed(_,loc) => Some(loc)
      case MousePiece(_,loc) => Some(loc)
      case MouseTile(loc) => Some(loc)
      case MouseTech(_,loc) => Some(loc)
      case MouseReinforcement(_,_,loc) => Some(loc)
      case MouseDeadPiece(_,loc) => Some(loc)
      case MouseExtraTechAndSpell(loc) => Some(loc)
      case MouseEndTurn(loc) => Some(loc)
      case MouseResignBoard(loc) => Some(loc)
      case MouseRedo(loc) => Some(loc)
      case MouseNextBoard => None
      case MousePrevBoard => None
      case MouseTerrain(_,loc) => Some(loc)
      case MousePause(loc) => Some(loc)
      case MouseCoords(loc) => Some(loc)
    }
  }
}
case object MouseNone extends MouseTarget
case class MouseSpellChoice(spellId: SpellId, side: Option[Side], loc: Loc) extends MouseTarget
case class MouseSpellHand(spellId: SpellId, side: Side, loc: Loc) extends MouseTarget
case class MouseSpellPlayed(played: Option[SpellPlayedInfo], loc: Loc) extends MouseTarget
case class MousePiece(pieceSpec: PieceSpec, loc: Loc) extends MouseTarget
case class MouseTile(loc: Loc) extends MouseTarget
case class MouseTech(techIdx: Int, loc: Loc) extends MouseTarget
case class MouseReinforcement(pieceName: Option[PieceName], side:Side, loc: Loc) extends MouseTarget
case class MouseDeadPiece(pieceSpec: PieceSpec, loc: Loc) extends MouseTarget
case class MouseExtraTechAndSpell(loc: Loc) extends MouseTarget
case class MouseEndTurn(loc: Loc) extends MouseTarget
case class MouseResignBoard(loc: Loc) extends MouseTarget
case class MouseRedo(loc: Loc) extends MouseTarget
case object MouseNextBoard extends MouseTarget
case object MousePrevBoard extends MouseTarget
case class MouseTerrain(terrain: Terrain, loc: Loc) extends MouseTarget
case class MousePause(loc: Loc) extends MouseTarget
case class MouseCoords(loc: Loc) extends MouseTarget

//Different modes the mouse can be in for selecting different things
sealed trait MouseMode {
  //Handlers in this mouse mode
  def handleMouseDown(curTarget: MouseTarget, game: Game, board: BoardState, undo: Boolean, ourSide: Option[Side]): Unit
  def handleMouseMove(dragTarget: MouseTarget, curTarget: MouseTarget, game: Game, board: BoardState, ourSide: Option[Side]): Unit
  def handleMouseUp(dragTarget: MouseTarget, curTarget: MouseTarget, game: Game, board: BoardState, boardIdx: Int, undo: Boolean, ourSide: Option[Side]): Unit
}

//---------------------------------------------------------------------------------------
//Current state of the mouse.

case class MouseState(val ourSide: Option[Side], val ui: UI, val client: Client)
{
  //The current target moused-over
  var hovered : MouseTarget = MouseNone
  //The target of the last mousedown, if the mouse is still held down.
  var dragTarget: MouseTarget = MouseNone

  //Last pixel location of the mouse, for refreshing the state in case the board changed
  var lastPixelLoc: Option[PixelLoc] = None

  // Are we in the middle of undoing something?
  var undoing: Boolean = false

  //Current mode that the mouse is operating in.
  var mode: MouseMode = NormalMouseMode(this)

  var selectedCity: Option[Piece] = None

  def clear() = {
    hovered = MouseNone
    dragTarget = MouseNone
    lastPixelLoc = None
    mode = NormalMouseMode(this)
  }

  def clearSelect() = {
    dragTarget = MouseNone
    mode = NormalMouseMode(this)
  }

  def refresh(game: Game, board: BoardState) = {
    lastPixelLoc.foreach { pixelLoc =>
      handleMouseMove(pixelLoc,game,board)
    }
  }

  private def getTarget(pixelLoc: PixelLoc, game: Game, board: BoardState): MouseTarget = {
    val hexLoc = HexLoc.ofPixel(pixelLoc, Drawing.gridSize)
    for(component <- ui.clickableComponents) {
      val target = component.getMouseTarget(game,board,hexLoc)
      if(target != MouseNone)
        return target
    }
    MouseNone
  }

  def handleMouseDown(pixelLoc: PixelLoc, game: Game, board: BoardState, undo: Boolean) : Unit = {
    undoing = undo
    lastPixelLoc = Some(pixelLoc)
    if(!client.gotFatalError) {
      val curTarget = getTarget(pixelLoc,game,board)
      // selectedCity = curTarget.findPiece(board)
      dragTarget = curTarget

      mode.handleMouseDown(curTarget,game,board, undo, ourSide)
    }
  }

  def handleMouseUp(pixelLoc: PixelLoc, game: Game, board: BoardState, boardIdx: Int, undo: Boolean): Unit = {
    undoing = false
    lastPixelLoc = Some(pixelLoc)
    if(!client.gotFatalError) {
      val curTarget = getTarget(pixelLoc,game,board)
      mode.handleMouseUp(dragTarget,curTarget,game,board,boardIdx,undo, ourSide)
      dragTarget = MouseNone
    }
  }

  def handleMouseMove(pixelLoc: PixelLoc, game: Game, board: BoardState) : Unit = {
    lastPixelLoc = Some(pixelLoc)

    val curTarget = getTarget(pixelLoc,game,board)
    hovered = curTarget
    if(!client.gotFatalError) {
      mode.handleMouseMove(dragTarget,curTarget,game,board, ourSide)
    }
  }

}

//---------------------------------------------------------------------------------------
//Normal mouse mode - click on things to do their usual actions, click and drag on
//pieces to move and attack.

case class NormalMouseMode(val mouseState: MouseState) extends MouseMode {

  //Tolerance in seconds for double click
  val doubleClickTolerance = 0.40

  //When clicking and dragging for a movement, the path of that movement, and the piece movements associated with it
  var path : List[Loc] = Nil
  var pathMovements : List[(PieceSpec,List[Loc])] = Nil

  //Double click implementation
  //First mousedown -> (target,time,false)
  //Second mousedown -> (target,time,true)
  //Second mouseup -> double click effect happens
  var doubleClickState: Option[(MouseTarget,Double,Boolean)] = None

  private def clearPath() = {
    path = Nil
    pathMovements = Nil
  }

  //Current time in seconds
  private def getNow(): Double = {
    (new scala.scalajs.js.Date()).getTime() / 1000.0
  }

  private def doubleClickTimeOkay(prevTime: Double): Boolean = {
    prevTime + doubleClickTolerance >= getNow()
  }

  def makeActionId(): String = {
    mouseState.client.makeActionId()
  }

  def handleMouseDown(curTarget: MouseTarget, game: Game, board: BoardState, undo: Boolean, ourSide: Option[Side]): Unit = {
    val _ = (game,board,undo,ourSide)
    doubleClickState = doubleClickState match {
      case None => Some((curTarget,getNow(),false))
      case Some((prevTarget,prevTime,_)) =>
        if(curTarget == prevTarget && doubleClickTimeOkay(prevTime)) Some((curTarget,prevTime,true))
        else Some((curTarget,getNow(),false))
    }

    clearPath()
  }

  def handleMouseMove(dragTarget: MouseTarget, curTarget: MouseTarget, game: Game, board: BoardState, ourSide: Option[Side]): Unit = {
    val _ = (game, ourSide)
    //Clear double click if mouse changes targets
    doubleClickState match {
      case None => ()
      case Some((prevTarget,_,_)) =>
        if(curTarget != prevTarget)
          doubleClickState = None
    }

    //Update path to be a shortest path from [dragTarget] to [curLoc] that
    //shares the longest prefix with the current [path].
    //(actually, not always shortest, see the case below where not mergable)
    val dragPiece = dragTarget.findPiece(board)
    val curLoc = curTarget.getLoc()
    (dragPiece, curLoc) match {
      case (None,_) | (_,None) => clearPath()
      case (Some(dragPiece), Some(curLoc)) =>
        if(dragPiece.side != board.side)
          clearPath()
        else {
          val newLegalMove = {
            //Truncate path to current loc, or extend by current loc, to use as the bias
            //for the next path
            val pathBias = {
              val idx = path.indexOf(curLoc)
              if(idx != -1) path.take(idx+1)
              else path :+ curLoc
            }

            val targetPiece = curTarget.findPiece(board)
            targetPiece match {
              case None =>
                //Ordinary move to location
                board.findPathForUI(dragPiece,pathBias=pathBias,isRotationPath=false) { case (loc,_) => loc == curLoc }
              case Some(targetPiece) =>
                //Merge into friendly swarm
                if(targetPiece.side == dragPiece.side) {
                  //If not mergable, then the user might be trying to swap, so preserve path as much as possible
                  //even if the path becomes nonminimal
                  val dpStats = dragPiece.curStats(board)
                  val tpStats = targetPiece.curStats(board)
                  val isRotationPath = {
                    !board.canSwarmTogether(dpStats,tpStats) || dpStats.swarmMax < board.pieces(curLoc).length + 1
                  }
                  board.findPathForUI(dragPiece,pathBias=pathBias,isRotationPath) { case (loc,_) => loc == curLoc }
                }
                //Attack enemy piece
                else {
                  val dpStats = dragPiece.curStats(board)
                  val tpStats = targetPiece.curStats(board)
                  if(!board.canAttack(dpStats,attackerHasMoved=false,dragPiece.actState,tpStats))
                    None
                  else {
                    val attackRange = { if(tpStats.isFlying) dpStats.attackRangeVsFlying else dpStats.attackRange }
                    board.findPathForUI(dragPiece,pathBias=pathBias,isRotationPath=false) { case (loc,revMovements) =>
                      revMovements.length <= 1 &&
                      board.topology.distance(loc, curLoc) <= attackRange &&
                      (loc == dragPiece.loc || !dpStats.isLumbering)
                    }
                  }
                }
            }
          }
          newLegalMove match {
            case None =>
              clearPath()
            case Some((newPath,newPathMovements)) =>
              path = newPath
              pathMovements = newPathMovements
          }
        }
    }
  }

  //The action to perform on mouseup from dragging the given piece
  def dragPieceMouseUpActions(curTarget: MouseTarget, curLoc: Loc, piece: Piece, board: BoardState): List[PlayerAction] = {
    //Attack if enemy piece under the mouseUp
    val attack: Option[List[PlayerAction]] = {
      curTarget.findPiece(board) match {
        case None => None
        case Some(other) =>
          if(other.side == piece.side) {
            None
          } else {
            // Pieces with flurry automatically attack as many times as necessary to kill the unit.
            val stats = piece.curStats(board)
            val attacks = stats.attackEffect match {
              case Some(Damage(1)) => Math.min(stats.numAttacks, other.curStats(board).defense.getOrElse(stats.numAttacks))
              case _ => 1
            }
            Some(List.tabulate(attacks)(_ => Attack(piece.spec, other.spec)))
          }
      }
    }
    val movementFromPath: Option[PlayerAction] = {
      //Move based on the path, if we have a nontrivial path and either the final location
      //is under the mouse OR we're attacking.
      if(attack.nonEmpty || (path.length >= 1 && path.last == curLoc)) {
        //Move based on path only if attempting to attack
        if(attack.nonEmpty) {
          if(path.length <= 1) None
          else Some(Movements(List(Movement(piece.spec, path.toVector))))
        }
        //Do fancy movement that potentially includes swaps
        else {
          val filteredPathMovements = pathMovements.filter { case (_,path) => path.length > 1 }
          if(filteredPathMovements.nonEmpty)
            Some(Movements(filteredPathMovements.map { case (pieceSpec,path) => Movement(pieceSpec,path.toVector) }))
          else if(path.length > 1)
            Some(Movements(List(Movement(piece.spec, path.toVector))))
          else
            None
        }
      }
      //Use teleporter
      else if(board.tiles(piece.loc).terrain == Teleporter)
        Some(Teleport(piece.spec, piece.loc, curLoc))
      else
        None
    }
    //If the path is empty and the target is a tile one hex away from the piece's current location
    //then attempt a move so that we can report an illegal move error as help
    val movement = {
      if(attack.isEmpty && movementFromPath.isEmpty) {
        if(board.topology.distance(piece.loc,curLoc) == 1)
          Some(Movements(List(Movement(piece.spec, Vector(piece.loc,curLoc)))))
        else movementFromPath
      }
      else movementFromPath
    }
    val blink =
      curTarget match {
        case MouseReinforcement(_,_,_) =>
          if(piece.curStats(board).canBlink) {
            Some(Blink(piece.spec, piece.loc))
          } else {
            None
          }
        case _ =>
          None
      }

    movement.toList ++ attack.getOrElse(Nil) ++ blink.toList
  }

  def handleMouseUp(dragTarget: MouseTarget, curTarget: MouseTarget, game: Game, board: BoardState, boardIdx: Int, undo: Boolean, ourSide: Option[Side]): Unit = {
    val didDoubleClick = {
      doubleClickState match {
        case None => false
        case Some((prevTarget,prevTime,secondDown)) =>
          prevTarget == curTarget && doubleClickTimeOkay(prevTime) && secondDown
      }
    }
    //Branch based on what we selected on the mouseDown
    dragTarget match {
      case MouseNone => ()

      case MouseSpellChoice(spellId, _, _) =>
        if(ourSide == Some(game.curSide)) {
          if(undo) {
            mouseState.client.doActionOnCurBoard(GainSpellUndo(spellId, makeActionId()))
          } else {
            mouseState.client.doActionOnCurBoard(DoGeneralBoardAction(GainSpell(spellId),makeActionId()))
          }
        }

      case MouseSpellHand(spellId,side,_) =>
        if(ourSide == Some(game.curSide) && side == game.curSide) {
          if(undo) {
            //Require mouse down and up on the same target
            if(curTarget == dragTarget) {
              mouseState.client.doActionOnCurBoard(GainSpellUndo(spellId, makeActionId()))
            }
          } else {
            curTarget match {
              case MouseSpellPlayed(_,_) =>
                mouseState.client.doActionOnCurBoard(PlayerActions(List(DiscardSpell(spellId)),makeActionId()))
              case _ =>
                mouseState.client.externalInfo.get.spellsRevealed.get(spellId).foreach { spellName =>
                  val spell = Spells.spellMap(spellName)
                  spell match {
                    case (_: TargetedSpell) =>
                      curTarget.findPiece(board).foreach { piece =>
                        val targets = SpellOrAbilityTargets.singlePiece(piece.spec)
                        mouseState.client.doActionOnCurBoard(PlayerActions(List(PlaySpell(spellId, targets)),makeActionId()))
                      }
                    case (_: TileSpell) =>
                      curTarget.getLoc().foreach { loc =>
                        val targets = SpellOrAbilityTargets.singleLoc(loc)
                        mouseState.client.doActionOnCurBoard(PlayerActions(List(PlaySpell(spellId, targets)),makeActionId()))
                      }
                    case (spell: PieceAndLocSpell) =>
                      curTarget.findPiece(board).foreach { piece =>
                        spell.tryCanTargetPiece(side,piece) match {
                          case Failure(err) => mouseState.client.reportError(err.getLocalizedMessage)
                          case Success(()) =>
                            val pieceTargets = List(piece.spec)
                            val locTargets = board.tiles.filterLocs { loc =>
                              spell.tryCanTarget(side,piece,loc,board).isSuccess
                            }
                            mouseState.mode = DragPieceToLocMouseMode(mouseState,pieceTargets,locTargets) { case (piece,loc) =>
                              val targets = SpellOrAbilityTargets.pieceAndLoc(piece.spec,loc)
                              mouseState.client.doActionOnCurBoard(PlayerActions(List(PlaySpell(spellId, targets)),makeActionId()))
                            }
                        }
                      }
                    case (_: TerrainAndTileSpell) =>
                      curTarget.getLoc().foreach { loc =>
                        val arbitraryTerrain = Whirlwind(true)
                        def makeAction(terrain: Terrain) = {
                          PlaySpell(spellId, SpellOrAbilityTargets.terrainAndLoc(terrain, loc))
                        }
                        if(board.tryLegality(makeAction(arbitraryTerrain), mouseState.client.externalInfo.get).isSuccess) {
                          mouseState.mode = SelectTerrainMouseMode(mouseState) { (terrainOpt: Option[Terrain]) =>
                            terrainOpt match {
                              case None => ()
                              case Some(terrain) =>
                                mouseState.client.doActionOnCurBoard(PlayerActions(List(makeAction(terrain)),makeActionId()))
                            }
                          }
                        } else {
                          mouseState.client.doActionOnCurBoard(PlayerActions(List(makeAction(arbitraryTerrain)),makeActionId()))
                        }
                      }
                    case (_: NoTargetSpell) =>
                      if(curTarget == dragTarget && didDoubleClick) {
                        mouseState.client.doActionOnCurBoard(PlayerActions(List(PlaySpell(spellId, SpellOrAbilityTargets.none)),makeActionId()))
                      }
                  }
                }
            }
          }
        }
      case MouseSpellPlayed(info,_) =>
        info match {
          case None =>
          case Some(info) =>
            if(ourSide == Some(game.curSide) && undo && info.side == game.curSide && curTarget == dragTarget) {
              mouseState.client.doActionOnCurBoard(SpellUndo(info.spellId, makeActionId()))
            }
        }

      case MouseExtraTechAndSpell(_) =>
        if(curTarget == dragTarget) {
          mouseState.selectedCity match {
            case None => ()
            case Some(selectedCity) => 
              def makeAction() = {
                ClearQueue(selectedCity.id,undo,didDoubleClick)
              }
              mouseState.client.doActionOnCurBoard(PlayerActions(List(makeAction()), makeActionId()))
            }
        }

      case MouseEndTurn(_) =>
        //Require mouse down and up on the same target
        if(curTarget == dragTarget)
          mouseState.client.doGameAction(SetBoardDone(boardIdx,!game.isBoardDone(boardIdx)))

      case MouseResignBoard(_) =>
        //Require mouse down and up on the same target
        if(ourSide == Some(game.curSide) && curTarget == dragTarget) {
          mouseState.client.showResignConfirm()
        }
      case MouseRedo(_) =>
        mouseState.client.doActionOnCurBoard(Redo(makeActionId()))

      case MousePause(_) =>
        //Require mouse down and up on the same target
        if(curTarget == dragTarget) {
          mouseState.client.pause()
        }
      case MouseCoords(_) =>
        //Require mouse down and up on the same target
        if(curTarget == dragTarget) {
          mouseState.client.toggleCoords()
        }

      case MouseNextBoard =>
        //Require mouse down and up on the same target
        if(curTarget == dragTarget) {
          val client = mouseState.client
          if(client.curBoardIdx < client.numBoards - 1) {
            mouseState.clear()
            client.curBoardIdx += 1
            client.reportCurBoard()
            client.draw()
          }
        }

      case MouseTerrain(_,loc) => 
        if ((curTarget == dragTarget) && undo) {
          mouseState.selectedCity match {
            case None => ()
            case Some(selectedCity) => 
              def makeAction() = {
                SetTarget(selectedCity.id,loc)
              }
              mouseState.client.doActionOnCurBoard(PlayerActions(List(makeAction()), makeActionId()))
            }
        }  

      case MousePrevBoard =>
        if(curTarget == dragTarget) {
          val client = mouseState.client
          if(client.curBoardIdx > 0) {
            mouseState.clear()
            client.curBoardIdx -= 1
            client.reportCurBoard()
            client.draw()
          }
        }

      case MouseTech(techIdx,_) =>
        //Require mouse down and up on the same target
        val techState = game.techLine(techIdx)
        if(curTarget == dragTarget) {
          techState.tech match {
            case PieceTech(pieceName) =>
              mouseState.selectedCity match {
                case None => ()
                case Some(selectedCity) => 
                  def makeAction() = {
                    AddToQueue(pieceName,selectedCity.id,undo)
                  }
                  mouseState.client.doActionOnCurBoard(PlayerActions(List(makeAction()), makeActionId()))
                  // mouseState.client.doActionOnCurBoard(DoGeneralBoardAction(AddToScienceQueue(pieceName,selectedCity.id),makeActionId()))
            }
            case Copycat | Metamagic | TechSeller => ()
          }
        }

      case MouseDeadPiece(pieceSpec,_) =>
        if(ourSide == Some(game.curSide)) {
          if(undo) {
            //Require mouse down and up on the same target
            if(curTarget == dragTarget) {
              mouseState.client.doActionOnCurBoard(LocalPieceUndo(pieceSpec,makeActionId()))
            }
          }
        }

      case MouseReinforcement(pieceNameOpt,side,_) =>
        pieceNameOpt match {
          case None => ()
          case Some(pieceName) =>
            if(ourSide == Some(game.curSide) && side == game.curSide) {
              if(undo) {
                //Require mouse down and up on the same target
                if(curTarget == dragTarget) {
                  val pieceSpec = board.unsummonedThisTurn.reverse.findMap { case (pieceSpec,name,_,_) =>
                    if(pieceName == name) Some(pieceSpec) else None
                  }
                  pieceSpec match {
                    case Some(pieceSpec) =>
                      mouseState.client.doActionOnCurBoard(LocalPieceUndo(pieceSpec,makeActionId()))
                    case None =>
                      mouseState.client.doActionOnCurBoard(BuyReinforcementUndo(pieceName,makeActionId()))
                  }
                }
              }
              else {
                curTarget.getLoc().foreach { curLoc =>
                  mouseState.client.doActionOnCurBoard(PlayerActions(List(Spawn(curLoc,pieceName)),makeActionId()))
                }
              }
            }
        }

      case MouseTile(loc) =>
        if(undo) (
          if ((curTarget == dragTarget)) {
            mouseState.selectedCity match {
              case None => ()
              case Some(selectedCity) => 
                def makeAction() = {
                  SetTarget(selectedCity.id,loc)
                }
                mouseState.client.doActionOnCurBoard(PlayerActions(List(makeAction()), makeActionId()))
              }
          }  
        )
        else {
          mouseState.selectedCity = None;
          board.tiles(loc).terrain match {
            case Wall | Ground | Water(_) | Graveyard | SorceryNode | Teleporter |
                 Earthquake(_) | Firestorm(_) | Whirlwind(_) | Mist => ()
            case Spawner(_) =>
              mouseState.client.doActionOnCurBoard(PlayerActions(List(ActivateTile(loc)),makeActionId()))
          }
        }

      case MousePiece(dragSpec,_) =>
        if(undo) {
          //Require mouse down and up on the same target
          if(curTarget == dragTarget) {
            mouseState.client.doActionOnCurBoard(LocalPieceUndo(dragSpec,makeActionId()))
          }
        }
        else {
          board.findPiece(dragSpec) match {
            case None => ()
            case Some(piece) =>
              if (piece.baseStats.name == "city") {
                mouseState.selectedCity = Some(piece);
              } else {
                mouseState.selectedCity = None;
              }
              if(piece.side == game.curSide) {
                //Double-clicking on a piece activates its ability
                if(didDoubleClick) {
                  val pieceStats = piece.curStats(board)
                  val abilities = pieceStats.abilities
                  if(abilities.length > 0) {
                    //TODO disambiguate which action if there's more than one?
                    val ability = abilities(0)
                    val spec = piece.spec
                    ability match {
                      case Suicide | SpawnZombies | KillAdjacent | NecroPickAbility | (_:SelfEnchantAbility) =>
                        val abilityActions = List(ActivateAbility(spec,ability,SpellOrAbilityTargets.none))
                        mouseState.client.doActionOnCurBoard(PlayerActions(abilityActions,makeActionId()))
                      case MoveEarthquake | MoveFlood | MoveWhirlwind | MoveFirestorm =>
                        def makeAction(loc:Loc) = {
                          ActivateAbility(spec, ability, SpellOrAbilityTargets.singleLoc(loc))
                        }
                        val locTargets = board.tiles.filterLocs { loc =>
                          board.tryLegality(makeAction(loc), mouseState.client.externalInfo.get).isSuccess
                        }
                        // TODO: It would be nice to distinguish:
                        // 1) "This is illegal regardless of the target" (e.g. no mana)
                        // 2) "There happen to be no legal targets right now"
                        if(locTargets.isEmpty) { // No legal targets, get error message
                          mouseState.client.doActionOnCurBoard(PlayerActions(List(makeAction(piece.loc)), makeActionId()))
                        } else {
                          mouseState.mode = SelectTargetMouseMode(mouseState, List(), locTargets) { (target:MouseTarget) =>
                            target.getLoc() match {
                              case None => ()
                              case Some(loc) =>
                                mouseState.client.doActionOnCurBoard(PlayerActions(List(makeAction(loc)), makeActionId()))
                            }
                          }
                        }
                      case MoveTerrain =>
                        def makeAction(terrain:Terrain, loc:Loc) = {
                          ActivateAbility(spec, ability, SpellOrAbilityTargets.terrainAndLoc(terrain,loc))
                        }
                        val arbitraryTerrain = Whirlwind(true)
                        def isLegal(loc:Loc) = {
                          board.tryLegality(makeAction(arbitraryTerrain, loc), mouseState.client.externalInfo.get).isSuccess
                        }
                        def doIllegalAction(loc: Loc) = {
                          assert(!isLegal(loc))
                          mouseState.client.doActionOnCurBoard(PlayerActions(List(makeAction(arbitraryTerrain, loc)), makeActionId()))
                        }
                        val locTargets = board.tiles.filterLocs { loc => isLegal(loc) }
                        if(locTargets.isEmpty) {
                          doIllegalAction(piece.loc)
                        } else {
                          mouseState.mode = SelectTargetMouseMode(mouseState, List(), locTargets) { (target:MouseTarget) =>
                            target.getLoc() match {
                              case None => ()
                              case Some(loc) =>
                                println(loc)
                                if(isLegal(loc)) {
                                  mouseState.mode = SelectTerrainMouseMode(mouseState) { (terrainOpt: Option[Terrain]) =>
                                    terrainOpt match {
                                      case None => ()
                                      case Some(terrain) =>
                                        mouseState.client.doActionOnCurBoard(PlayerActions(List(makeAction(terrain, loc)), makeActionId()))
                                    }
                                  }
                                } else {
                                  println(loc)
                                  doIllegalAction(loc)
                                }
                            }
                          }
                        }

                      case (_:TargetedAbility) =>
                        val pieceTargets = List() // TODO: FIXME
                        mouseState.mode = SelectTargetMouseMode(mouseState, pieceTargets, List()) { (target:MouseTarget) =>
                          target.findPiece(board) match {
                            case None => ()
                            case Some(targetPiece) =>
                              val abilityActions = List(ActivateAbility(spec,ability,SpellOrAbilityTargets.singlePiece(targetPiece.spec)))
                              mouseState.client.doActionOnCurBoard(PlayerActions(abilityActions,makeActionId()))
                          }
                        }
                    }
                  }
                }
                //Otherwise, normal click-and-drag
                else {
                  curTarget.getLoc().foreach { curLoc =>
                    val actions = dragPieceMouseUpActions(curTarget, curLoc, piece, board)
                    actions.foreach { action =>
                      mouseState.client.doActionOnCurBoard(PlayerActions(List(action),makeActionId()))
                    }
                  }
                }
              }
          }
        }
    }

    path = Nil
    pathMovements = Nil

    if(didDoubleClick)
      doubleClickState = None
  }
}

//---------------------------------------------------------------------------------------
//Mouse mode for selecting a target of spell or ability

case class SelectTargetMouseMode(val mouseState: MouseState, val pieceTargets: List[PieceSpec], val locTargets: List[Loc])(f:MouseTarget => Unit) extends MouseMode {

  def handleMouseDown(curTarget: MouseTarget, game: Game, board: BoardState, undo: Boolean, ourSide: Option[Side]): Unit = {
    val _ = (curTarget,game,board, undo, ourSide)
  }

  def handleMouseMove(dragTarget: MouseTarget, curTarget: MouseTarget, game: Game, board: BoardState, ourSide: Option[Side]): Unit = {
    val _ = (dragTarget,curTarget,game,board, ourSide)
  }

  def handleMouseUp(dragTarget: MouseTarget, curTarget: MouseTarget, game: Game, board: BoardState, boardIdx: Int, undo: Boolean, ourSide: Option[Side]): Unit = {
    val _ = (game,board,boardIdx,undo, ourSide)

    //Restore normal mode
    mouseState.mode = NormalMouseMode(mouseState)

    //If the mouse up and mouse down are on the same location, we're good
    if(curTarget == dragTarget)
      f(curTarget)
  }
}

//---------------------------------------------------------------------------------------
//Mouse mode for dragging a piece to a location for a spell or ability

case class DragPieceToLocMouseMode(val mouseState: MouseState, val pieceTargets: List[PieceSpec], val locTargets: List[Loc])(f:(Piece,Loc) => Unit) extends MouseMode {

  def handleMouseDown(curTarget: MouseTarget, game: Game, board: BoardState, undo: Boolean, ourSide: Option[Side]): Unit = {
    val _ = (curTarget,game,board, undo, ourSide)
  }

  def handleMouseMove(dragTarget: MouseTarget, curTarget: MouseTarget, game: Game, board: BoardState, ourSide: Option[Side]): Unit = {
    val _ = (dragTarget,curTarget,game,board, ourSide)
  }

  def handleMouseUp(dragTarget: MouseTarget, curTarget: MouseTarget, game: Game, board: BoardState, boardIdx: Int, undo: Boolean, ourSide: Option[Side]): Unit = {
    val _ = (game,board,boardIdx,undo, ourSide)

    val piece = {
      if(pieceTargets.length == 1) board.findPiece(pieceTargets.head)
      else dragTarget.findPiece(board)
    }

    val loc = curTarget match {
      case MousePiece(_,loc) => Some(loc)
      case MouseTile(loc) => Some(loc)
      case MouseNone => None
      case MouseSpellChoice(_,_,_) => None
      case MouseSpellHand(_,_,_) => None
      case MouseSpellPlayed(_,_) => None
      case MouseTech(_,_) => None
      case MouseReinforcement(_,_,_) => None
      case MouseDeadPiece(_,_) => None
      case MouseExtraTechAndSpell(_) => None
      case MouseEndTurn(_) => None
      case MouseResignBoard(_) => None
      case MouseRedo(_) => None
      case MouseNextBoard => None
      case MousePrevBoard => None
      case MouseTerrain(_,_) => None
      case MousePause(_) => None
      case MouseCoords(_) => None
    }

    //Restore normal mode
    mouseState.mode = NormalMouseMode(mouseState)

    piece.foreach { piece =>
      loc.foreach { loc =>
        f(piece,loc)
      }
    }
  }
}

case class SelectTerrainMouseMode(val mouseState: MouseState)(f: Option[Terrain] => Unit) extends MouseMode {
  def handleMouseDown(curTarget: MouseTarget, game: Game, board: BoardState, undo: Boolean, ourSide: Option[Side]): Unit = {
    val _ = (curTarget,game,board, undo, ourSide)
  }
  def handleMouseMove(dragTarget: MouseTarget, curTarget: MouseTarget, game: Game, board: BoardState, ourSide: Option[Side]): Unit = {
    val _ = (dragTarget,curTarget,game,board, ourSide)
  }
  def handleMouseUp(dragTarget: MouseTarget, curTarget: MouseTarget, game: Game, board: BoardState, boardIdx: Int, undo: Boolean, ourSide: Option[Side]): Unit = {
    val _ = (game,board,boardIdx,undo, ourSide)
    val terrain = curTarget match {
      case MousePiece(_,_) => None
      case MouseTile(_) => None
      case MouseNone => None
      case MouseSpellChoice(_,_,_) => None
      case MouseSpellHand(_,_,_) => None
      case MouseSpellPlayed(_,_) => None
      case MouseTech(_,_) => None
      case MouseReinforcement(_,_,_) => None
      case MouseDeadPiece(_,_) => None
      case MouseExtraTechAndSpell(_) => None
      case MouseEndTurn(_) => None
      case MouseResignBoard(_) => None
      case MouseRedo(_) => None
      case MouseNextBoard => None
      case MousePrevBoard => None
      case MouseTerrain(terrain,_) => Some(terrain)
      case MousePause(_) => None
      case MouseCoords(_) => None
    }
    mouseState.mode = NormalMouseMode(mouseState)
    f(terrain)
  }
}
